package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.google.common.collect.ImmutableMap;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;
import com.mediahub.core.utils.ProjectPermissionsUtil;
import com.mediahub.core.utils.UserUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
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
import javax.jcr.Session;
import javax.jcr.ValueFactory;
import javax.jcr.security.Privilege;
import java.security.Principal;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
                if (adminResolver.getResource(projectPath) != null && "cq/gui/components/projects/admin/card/projectcard".equals(adminResolver.getResource(projectPath).getResourceType())) {

                    JackrabbitSession js = (JackrabbitSession) adminSession;
                    PrincipalManager principalMgr = js.getPrincipalManager();
                    UserManager userManager = js.getUserManager();
                    ValueFactory vf = adminSession.getValueFactory();

                    Resource projectResource = adminResolver.getResource(projectPath);
                    ValueMap projectValueMap = projectResource.getValueMap();
                    List<Principal> principalNameList = new LinkedList<>();

                    Principal groupEditorPrincipal = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_EDITOR, String.class));
                    Principal groupObserverPrincipal = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_OBSERVER, String.class));
                    Principal groupOwnerPrincipal = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_OWNER, String.class));
                    Principal groupOwnerProjectPublisher = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_PROJECTPUBLISHER, String.class));
                    Principal groupExternalContribPrincipal = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR, String.class));
                    principalNameList.add(groupEditorPrincipal);
                    principalNameList.add(groupObserverPrincipal);
                    principalNameList.add(groupOwnerPrincipal);
                    principalNameList.add(groupOwnerProjectPublisher);
                    principalNameList.add(groupExternalContribPrincipal);

                    if (projectValueMap.containsKey(BnpConstants.ROLE_EDITOR) && (!projectValueMap.containsKey("initialized") || projectValueMap.get("initialized", Boolean.class))) {

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

                        while (projectResource.getParent() != null && !StringUtils.equals(projectResource.getParent().getPath(), BnpConstants.AEM_PROJECTS_PATH)) {
                            projectResource = projectResource.getParent();
                            String parentFolderPath = projectResource.getPath();
                            Node parentFldrNode = projectResource.adaptTo(Node.class);
                            if (!parentFldrNode.hasNode(BnpConstants.REP_POLICY)) {
                                ModifiableValueMap mvp = projectResource.adaptTo(ModifiableValueMap.class);
                                mvp.put(BnpConstants.JCR_MIXINTYPES, BnpConstants.REP_ACCESSCONTROLLABLE);
                                String parentProjectPath = projectResource.getPath();
                                Resource reResource = adminResolver.getResource(parentProjectPath);
                                Node createPolicyNode = reResource.adaptTo(Node.class);
                                createPolicyNode.addNode(BnpConstants.REP_POLICY, BnpConstants.REP_ACL);
                            }
                            CreatePolicyNodeUtil.createRepPolicyNodes(adminSession, parentFolderPath, principalNameList, ImmutableMap.of("rep:glob", vf.createValue("")));
                            String groupName = UserUtils.deriveProjectGroupNameFromPath(projectResource.getPath());
                            if (userManager.getAuthorizable(groupName) != null) {
                                CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectPath, userManager.getAuthorizable(groupName).getPrincipal(), Privilege.JCR_ALL);
                            }
                        }

                        String damFolderPath = adminResolver.getResource(projectPath).getChild(JcrConstants.JCR_CONTENT).getValueMap().get("damFolderPath", String.class);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectInternalGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectExternalGroup.getPrincipal(), false, null, Privilege.JCR_REMOVE_NODE, Privilege.JCR_REMOVE_CHILD_NODES);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectExternalGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, projectPublisherGroup.getPrincipal(), Privilege.JCR_MODIFY_ACCESS_CONTROL);
                        projectResource = adminResolver.getResource(damFolderPath);
                        while (projectResource.getParent() != null && !StringUtils.equals(projectResource.getParent().getPath(), BnpConstants.MEDIALIBRARY_PROJECTS_PATH)) {
                            projectResource = projectResource.getParent();
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectResource.getPath(), groupOwnerPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectResource.getPath(), groupOwnerProjectPublisher, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectResource.getPath(), groupEditorPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectResource.getPath(), groupObserverPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectResource.getPath(), groupExternalContribPrincipal, ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                        }

                        ModifiableValueMap map = projectResource.adaptTo(ModifiableValueMap.class);
                        map.put("initialized", true);
                        adminResolver.commit();
                    }

                    if (projectValueMap.containsKey(BnpConstants.ROLE_EDITOR)) {
                        ProjectPermissionsUtil.assignMembersToMedialibrary(adminResolver, projectPath);
                    }
                }

            }
        } catch (Exception e) {
            log.error("RepositoryException while Executing events", e);
        } finally {
            if (adminSession != null) {
                adminSession.logout();
            }
        }

    }

}
