package com.mediahub.core.listeners;

import com.google.common.collect.ImmutableMap;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.*;
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

                JackrabbitSession js = (JackrabbitSession) adminSession;
                PrincipalManager principalMgr = js.getPrincipalManager();
                UserManager userManager = js.getUserManager();
                ValueFactory vf = adminSession.getValueFactory();

                String projectPath = my.getPath();
                if ("cq/gui/components/projects/admin/card/projectcard".equals(adminResolver.getResource(projectPath).getResourceType())) {
                    Resource adminResource = adminResolver.getResource(projectPath);
                    principalNameList = new LinkedList<>();
                    Node projectNode = adminResource.adaptTo(Node.class);


                    Principal groupEditorPrincipal = principalMgr
                            .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EDITOR).getString());
                    Principal groupObserverPrincipal = principalMgr
                            .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OBSERVER).getString());
                    Principal groupOwnerPrincipal = principalMgr
                            .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OWNER).getString());
                    Principal groupOwnerProjectPublisher = principalMgr
                            .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_PROJECTPUBLISHER).getString());
                    Principal groupExternalContribPrincipal = principalMgr
                            .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR).getString());
                    principalNameList.add(groupEditorPrincipal);
                    principalNameList.add(groupObserverPrincipal);
                    principalNameList.add(groupOwnerPrincipal);
                    principalNameList.add(groupOwnerProjectPublisher);
                    principalNameList.add(groupExternalContribPrincipal);

                    Group projectInternalGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_INTERNAL_CONTRIBUTOR_GROUP);
                    projectInternalGroup.addMember(userManager.getAuthorizable(groupEditorPrincipal));
                    Group projectExternalGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_EXTERNAL_CONTRIBUTOR_GROUP);
                    projectExternalGroup.addMember(userManager.getAuthorizable(groupExternalContribPrincipal));
                    Group projectPublisherGroup = (Group) userManager.getAuthorizable(BnpConstants.PROJECT_PUBLISHER_GROUP);
                    projectPublisherGroup.addMember(userManager.getAuthorizable(groupOwnerProjectPublisher));

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
                            adminResolver.commit();
                            String parentProjectPath = adminResource.getPath();
                            Resource reResource = adminResolver.getResource(parentProjectPath);
                            Node createPolicyNode = reResource.adaptTo(Node.class);
                            createPolicyNode.addNode(BnpConstants.REP_POLICY, BnpConstants.REP_ACL);
                            adminResolver.commit();
                        }
                        CreatePolicyNodeUtil.createRepPolicyNodes(adminSession, parentFolderPath, principalNameList, ImmutableMap.of("rep:glob", vf.createValue("")));

                    }

                    String damPath = adminResolver.getResource(projectPath).getChild("jcr:content").getValueMap().get("project.path", String.class);
                    CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damPath, groupOwnerPrincipal, Privilege.JCR_ALL);
                    CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damPath, groupOwnerProjectPublisher, Privilege.JCR_ALL);
                    adminResource = adminResolver.getResource(damPath);
                    while (adminResource.getParent() != null && !StringUtils.equals(adminResource.getParent().getPath(), BnpConstants.MEDIALIBRARY_PATH)) {
                        adminResource = adminResource.getParent();
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupOwnerPrincipal, Privilege.JCR_READ, ImmutableMap.of("rep:glob", vf.createValue("")));
                        CreatePolicyNodeUtil.createRepPolicyNode(adminSession, adminResource.getPath(), groupOwnerProjectPublisher, Privilege.JCR_READ, ImmutableMap.of("rep:glob", vf.createValue("")));
                    }
                }

                NodeIterator ite = adminResolver.getResource(projectPath).adaptTo(Node.class).getNodes(BnpConstants.REP_POLICY);
                if (ite.hasNext()) {
                    Node policies = ((Node) ite.next());
                    NodeIterator ite2 = policies.getNodes();
                    while (ite2.hasNext()) {
                        Node policyNode = (Node) ite2.next();
                        String primaryType = policyNode.getProperty("jcr:primaryType").getString();
                        String principalName = policyNode.getProperty("rep:principalName").getString();
                        if ("rep:DenyACE".equals(primaryType) && "projects-users".equals(principalName)) {
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, projectPath, principalMgr.getPrincipal("mediahub-basic-project-manager"), Privilege.JCR_ALL);
                            String damFolderPath = adminResolver.getResource(projectPath).getChild("jcr:content").getValueMap().get("damFolderPath", String.class);
                            CreatePolicyNodeUtil.createRepPolicyNode(adminSession, damFolderPath, principalMgr.getPrincipal("mediahub-basic-project-manager"), Privilege.JCR_ALL);
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
