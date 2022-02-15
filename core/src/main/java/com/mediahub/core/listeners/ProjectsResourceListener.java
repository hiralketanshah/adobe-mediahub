package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.google.common.collect.ImmutableMap;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.ValueFactory;
import javax.jcr.security.Privilege;
import java.security.Principal;
import java.util.*;

import static com.mediahub.core.constants.BnpConstants.MEDIALIBRARY_PATH;

/**
 * @author Shipra Arora
 * <p>
 * Listener class to provide read permissions to the parent project folders until projects.
 */

@Component(service = {
        ResourceChangeListener.class},
        immediate = true,
        property = {
                ResourceChangeListener.CHANGES + "=ADDED",
                ResourceChangeListener.CHANGES + "=CHANGED",
                ResourceChangeListener.PATHS + "=glob:/content/projects/**"// for handling custom invalidation for AF, AFF
        }
)

@ServiceDescription("listen on changes in the resource tree")
public class ProjectsResourceListener implements ResourceChangeListener {

    private final Logger log = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

    /**
     * The repository.
     */
    @Reference
    org.apache.sling.jcr.api.SlingRepository repository;
    @Reference
    private ConfigurationAdmin configAdmin;
    List<Principal> principalNameList;


    @Override
    public void onChange(List<ResourceChange> arg0) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        Session adminSession = null;
        try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {

            // getting session of System User
            adminSession = adminResolver.adaptTo(Session.class);
            for (ResourceChange my : arg0) {

                String projectPath = my.getPath();
                if ("cq/gui/components/projects/admin/card/projectcard".equals(adminResolver.getResource(projectPath).getResourceType())) {

                    JackrabbitSession js = (JackrabbitSession) adminSession;
                    PrincipalManager principalMgr = js.getPrincipalManager();
                    UserManager userManager = js.getUserManager();
                    ValueFactory vf = adminSession.getValueFactory();

                    Resource adminResource = adminResolver.getResource(projectPath);
                    principalNameList = new LinkedList<>();
                    Node projectNode = adminResource.adaptTo(Node.class);

                    Principal groupEditorPrincipal = principalMgr.getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EDITOR).getString());
                    Principal groupObserverPrincipal = principalMgr.getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OBSERVER).getString());
                    Principal groupOwnerPrincipal = principalMgr.getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OWNER).getString());
                    Principal groupOwnerProjectPublisher = principalMgr.getPrincipal(projectNode.getProperty(BnpConstants.ROLE_PROJECTPUBLISHER).getString());
                    Principal groupExternalContribPrincipal = principalMgr.getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR).getString());
                    principalNameList.add(groupEditorPrincipal);
                    principalNameList.add(groupObserverPrincipal);
                    principalNameList.add(groupOwnerPrincipal);
                    principalNameList.add(groupOwnerProjectPublisher);
                    principalNameList.add(groupExternalContribPrincipal);

                    if (projectNode.hasProperty(BnpConstants.ROLE_EDITOR) && (!projectNode.hasProperty("initialized") || (projectNode.hasProperty("initialized") && !projectNode.getProperty("initialized").getBoolean()))) {

                        Group projectInternalGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_INTERNAL_CONTRIBUTOR_GROUP);
                        projectInternalGroup.addMember(userManager.getAuthorizable(groupEditorPrincipal));
                        Group projectExternalGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_EXTERNAL_CONTRIBUTOR_GROUP);
                        projectExternalGroup.addMember(userManager.getAuthorizable(groupExternalContribPrincipal));
                        Group projectPublisherGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_PUBLISHER_GROUP);
                        projectPublisherGroup.addMember(userManager.getAuthorizable(groupOwnerProjectPublisher));
                        Group projectReaderGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_READER_GROUP);
                        projectReaderGroup.addMember(userManager.getAuthorizable(groupObserverPrincipal));
                        Group projectManagerGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_MANAGER_GROUP);
                        projectManagerGroup.addMember(userManager.getAuthorizable(groupOwnerPrincipal));

                        if (!userManager.isAutoSave()) {
                            js.save();
                        }

                        while (adminResource.getParent() != null && !StringUtils.equals(adminResource.getParent().getPath(), BnpConstants.AEM_PROJECTS_PATH)) {
                            adminResource = adminResource.getParent();
                            String parentFolderPath = adminResource.getPath();
                            Node parentFldrNode = adminResource.adaptTo(Node.class);
                            if (!parentFldrNode.hasNode(BnpConstants.REP_POLICY)) {
                                ModifiableValueMap mvp = adminResource.adaptTo(ModifiableValueMap.class);
                                mvp.put(BnpConstants.JCR_MIXINTYPES, BnpConstants.REP_ACCESSCONTROLLABLE);
                                String parentProjectPath = adminResource.getPath();
                                Resource reResource = adminResolver.getResource(parentProjectPath);
                                Node createPolicyNode = reResource.adaptTo(Node.class);
                                createPolicyNode.addNode(BnpConstants.REP_POLICY, BnpConstants.REP_ACL);
                            }
                            CreatePolicyNodeUtil.createRepPolicyNodes(adminSession, parentFolderPath, principalNameList, ImmutableMap.of("rep:glob", vf.createValue("")));
                        }

                        String damFolderPath = adminResolver.getResource(projectPath).getChild(JcrConstants.JCR_CONTENT).getValueMap().get("damFolderPath", String.class);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectInternalGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectExternalGroup.getPrincipal(), false, null, Privilege.JCR_REMOVE_NODE, Privilege.JCR_REMOVE_CHILD_NODES);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectExternalGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectPublisherGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        adminResource = adminResolver.getResource(damFolderPath);
                        while (adminResource.getParent() != null && !StringUtils.equals(adminResource.getParent().getPath(), BnpConstants.MEDIALIBRARY_PROJECTS_PATH)) {
                            adminResource = adminResource.getParent();
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupOwnerPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupOwnerProjectPublisher, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupEditorPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupObserverPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupExternalContribPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                        }

                        Resource projectResource = adminResolver.getResource(projectPath);
                        ModifiableValueMap map = projectResource.adaptTo(ModifiableValueMap.class);
                        map.put("initialized", true);
                        adminResolver.commit();
                    }

                    if (projectNode.hasProperty(BnpConstants.ROLE_EDITOR)) {
                        String damPath = adminResolver.getResource(projectPath).getChild(JcrConstants.JCR_CONTENT).getValueMap().get("project.path", String.class);
                        String[] damPathFolders = damPath.substring(1).split("/");
                        StringBuilder pathBuilder = new StringBuilder(MEDIALIBRARY_PATH);
                        if (damPathFolders.length > 3) {
                            for (int i = 3; i < damPathFolders.length && i <= 7; i++) {
                                pathBuilder.append("/").append(damPathFolders[i]);
                            }
                        }
                        Resource damPathSelectedFolderMetadata = adminResolver.getResource(pathBuilder.toString()).getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                        if (damPathSelectedFolderMetadata != null) {
                            String damPathSelectedFolderUUID = damPathSelectedFolderMetadata.getValueMap().get("uuid", String.class);
                            if (!StringUtils.isEmpty(damPathSelectedFolderUUID)) {
                                Group uuidGroup = (Group) userManager.getAuthorizable(damPathSelectedFolderUUID + "-contributor");
                                if (uuidGroup != null) {
                                    //Add managers to contributor
                                    Iterator<Authorizable> managerMembers = ((Group) userManager.getAuthorizable(groupOwnerPrincipal)).getDeclaredMembers();
                                    while (managerMembers.hasNext()) {
                                        Authorizable uuidGroupMember = managerMembers.next();
                                        uuidGroup.addMember(uuidGroupMember);
                                    }
                                    //Add publishers to contributor
                                    Iterator<Authorizable> publishersMembers = ((Group) userManager.getAuthorizable(groupOwnerProjectPublisher)).getDeclaredMembers();
                                    while (publishersMembers.hasNext()) {
                                        Authorizable uuidGroupMember = publishersMembers.next();
                                        uuidGroup.addMember(uuidGroupMember);
                                    }
                                    if (!userManager.isAutoSave()) {
                                        js.save();
                                    }
                                }
                            }
                        }
                    }
                }

            }
        } catch (LoginException | RepositoryException | PersistenceException e) {
            log.error("RepositoryException while Executing events", e);
        } finally {
            if (adminSession != null) {
                adminSession.logout();
            }
        }

    }
}
