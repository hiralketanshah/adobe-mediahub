package com.mediahub.core.workflows;


import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;
import java.security.Principal;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : MOVE ASSET BEFORE PUBLISH"})
public class MoveAssetsProcessWorkflow implements WorkflowProcess {

    public static final String CONTENT_DAM_PROJECTS = "/content/dam/projects/";
    public static final String USER_DEACTIVATION_SERVICE = "userDeactivationService";
    public static final String BNPP_STATUS = "bnpp-status";
    private static final Logger log = LoggerFactory.getLogger(MoveAssetsProcessWorkflow.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    /**
     * The method called by the AEM Workflow Engine to perform Workflow work.
     *
     * @param workItem        the work item representing the resource moving through the Workflow
     * @param workflowSession the workflow session
     * @param args            arguments for this Workflow Process defined on the Workflow Model (PROCESS_ARGS, argSingle, argMulti)
     * @throws WorkflowException when the Workflow Process step cannot complete. This will cause the WF to retry.
     */
    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap args) throws WorkflowException {

        if (!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {
            throw new WorkflowException("Unable to get the payload");
        }

        String initiator = workItem.getWorkflow() == null ? StringUtils.EMPTY : workItem.getWorkflow().getInitiator();

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        ResourceResolver resourceResolver = null;
        Session session = null;
        try {
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            session = resourceResolver.adaptTo(Session.class);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            log.debug("payloadPath : {}", payloadPath);
            Resource payload = resourceResolver.getResource(payloadPath);

            if (payload == null) {
                return;
            }
            Resource media = findMediaFolderPath(resourceResolver, payloadPath);

            String projectDamPath = StringUtils.EMPTY;
            // Get the project associated
            // Get the target PATH
            // Copy the folder / asset and subnodes
            if (StringUtils.contains(payload.getPath(), "/dam/projects/")) {

                Resource project = getProjectResource(resourceResolver, payload, media);

                if (project != null && (project.getChild(JcrConstants.JCR_CONTENT) != null)) {
                    projectDamPath = project.getChild(JcrConstants.JCR_CONTENT).getValueMap().get("project.path", StringUtils.EMPTY);
                    // Due to UUID Issue using session.move instead or resolver.move
                    Resource damPath = resourceResolver.getResource(projectDamPath);

                    //copyRepolicyNode(resourceResolver, session, payload, projectDamPath);
                    String newPath = moveProjectDamAsset(resourceResolver, session, payload, media, projectDamPath,
                            damPath, initiator);
                    if (StringUtils.isNotBlank(newPath)) {
                        WorkflowUtils.updateWorkflowPayload(workItem, workflowSession, newPath);
                    }
                }

                session.save();
                resourceResolver.commit();

                if (media != null && isFolderEmpty(media)) {
                    resourceResolver.delete(media);
                    resourceResolver.commit();
                }
            }
        } catch (LoginException | PersistenceException | RepositoryException e) {
            throw new WorkflowException("Error while moving asset from Projects", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }
    }


    /**
     * Get Project Resource
     *
     * @param resourceResolver
     * @param payload
     * @param media
     * @return
     */
    protected Resource getProjectResource(ResourceResolver resourceResolver, Resource payload,
                                          Resource media) {
        Resource project;
        String projectPath = DamUtil.getInheritedProperty("projectPath", payload, StringUtils.EMPTY);
        if( StringUtils.isNotBlank(projectPath) ) {
            project = resourceResolver.getResource(projectPath);
        } else if (media != null) {
            project = resourceResolver.getResource(
                StringUtils.replace(media.getParent().getPath(), "/dam", StringUtils.EMPTY));
        } else {
            project = resourceResolver.getResource(StringUtils.replace(payload.getParent().getPath(), "/dam", StringUtils.EMPTY));
        }
        return project;
    }

    /**
     * copy rep:policy node while moving asset
     *
     * @param resourceResolver - Resource Resolver from Service User
     * @param session          - admin session
     * @param payload          - payload Resource
     * @param projectDamPath   - dam path
     * @throws PersistenceException
     * @throws RepositoryException
     */
    protected void copyRepolicyNode(ResourceResolver resourceResolver, Session session,
                                    Resource payload, String projectDamPath)
            throws PersistenceException, RepositoryException {
        if (StringUtils.contains(payload.getPath(), CONTENT_DAM_PROJECTS) && StringUtils.isNotBlank(projectDamPath)) {
            String projectPath = getDamProjectPath(resourceResolver, payload);
            Resource destination = resourceResolver.getResource(projectDamPath);
            Resource policy = checkRepolicyExists(resourceResolver, destination);
            resourceResolver.commit();
            Iterator<Resource> resources = resourceResolver.getResource(projectPath + "/" + BnpConstants.REP_POLICY).listChildren();

            JackrabbitSession js = (JackrabbitSession) session;
            PrincipalManager principalMgr = js.getPrincipalManager();
            List<Principal> principalNameList = new LinkedList<>();

            while (resources.hasNext()) {
                Resource child = resources.next();
                if (StringUtils.contains(child.getName(), "allow") && StringUtils.startsWith(child.getValueMap().get(BnpConstants.REP_PRINCIPAL_NAME, StringUtils.EMPTY), "projects-") && StringUtils.endsWith(child.getValueMap().get(BnpConstants.REP_PRINCIPAL_NAME, StringUtils.EMPTY), "-publisher") && policy.getChild(child.getName()) == null) {
                    Principal principal = principalMgr.getPrincipal(child.getValueMap().get(BnpConstants.REP_PRINCIPAL_NAME, ""));
                    principalNameList.add(principal);
                }
            }

            if (policy != null && policy.getParent() != null) {
                CreatePolicyNodeUtil
                        .createRepPolicyNodes(session, policy.getParent().getPath(), principalNameList);
            }
        }

        if (resourceResolver.hasChanges()) {
            resourceResolver.commit();
        }
    }

    private String getDamProjectPath(ResourceResolver resourceResolver, Resource payload) {
        Resource payloadResource = resourceResolver.getResource(payload.getPath());
        if (payloadResource != null) {
            while (payloadResource.getParent() != null) {
                if (payloadResource.getValueMap().containsKey("projectPath")) {
                    return payloadResource.getPath();
                }
                payloadResource = payloadResource.getParent();
            }
        }
        return StringUtils.EMPTY;
    }

    /**
     * @param resourceResolver
     * @param destination
     * @return
     * @throws RepositoryException
     * @throws PersistenceException
     */
    protected Resource checkRepolicyExists(ResourceResolver resourceResolver, Resource destination) throws RepositoryException, PersistenceException {
        Resource policy = null;
        if (destination != null) {
            if (!destination.getValueMap().containsKey(JcrConstants.JCR_MIXINTYPES)) {
                Node destinationNode = destination.adaptTo(Node.class);
                destinationNode.addMixin("rep:AccessControllable");
                destinationNode.getSession().save();
            }

            if (destination.getChild(BnpConstants.REP_POLICY) != null) {
                policy = destination.getChild(BnpConstants.REP_POLICY);
            } else {
                policy = resourceResolver.create(destination, BnpConstants.REP_POLICY, Collections
                        .singletonMap(JcrConstants.JCR_PRIMARYTYPE, BnpConstants.REP_ACL));
            }
        }
        return policy;
    }

    /**
     * Method to move asset from source to destination
     *
     * @param resourceResolver
     * @param session          - session to copy or move node due to UUID
     * @param payload          - Payload from workflow
     * @param media            - Parent media folder resource
     * @param projectDamPath   - path of the dam to be migrated
     * @param damPath          - dam resource
     * @throws PersistenceException
     * @throws RepositoryException
     */
    protected String moveProjectDamAsset(ResourceResolver resourceResolver, Session session,
                                         Resource payload, Resource media, String projectDamPath, Resource damPath, String initiator)
            throws PersistenceException, RepositoryException {
        String newPath = "";
        if (damPath != null) {
            if (media != null) {
                Resource desitnationMedia = damPath.getChild(media.getName());
                if (desitnationMedia == null) {
                    Map<String, Object> folderProperties = new HashMap<>();
                    folderProperties.put(JcrConstants.JCR_PRIMARYTYPE, media.getValueMap().get(JcrConstants.JCR_PRIMARYTYPE, BnpConstants.SLING_FOLDER));
                    desitnationMedia = resourceResolver.create(damPath, media.getName(), folderProperties);
                    resourceResolver.commit();
                    Resource contentResource = media.getChild(JcrConstants.JCR_CONTENT);
                    setMetadata(resourceResolver, session, initiator, desitnationMedia,
                            contentResource);
                }
                newPath = desitnationMedia.getPath() + "/" + payload.getName();
                session.move(payload.getPath(), newPath);
            } else {
                newPath = projectDamPath + "/" + payload.getName();
                session.move(payload.getPath(), newPath);
            }
        }
        return newPath;
    }

    /**
     * @param resourceResolver
     * @param session
     * @param initiator
     * @param desitnationMedia
     * @param contentResource
     * @throws RepositoryException
     * @throws PersistenceException
     */
    private void setMetadata(ResourceResolver resourceResolver, Session session, String initiator,
                             Resource desitnationMedia, Resource contentResource)
            throws RepositoryException, PersistenceException {
        if (null != contentResource) {
            session.getWorkspace().copy(contentResource.getPath(), desitnationMedia.getPath() + "/" + JcrConstants.JCR_CONTENT);
            if (null != contentResource.getChild(BnpConstants.METADATA) && null != desitnationMedia.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA)) {
                ValueMap sourceMetadataValues = contentResource.getChild(BnpConstants.METADATA).getValueMap();
                ModifiableValueMap metadataValues = desitnationMedia.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).adaptTo(ModifiableValueMap.class);
                metadataValues.put(BNPP_STATUS, "validated");
                metadataValues.put(JcrConstants.JCR_CREATED_BY, sourceMetadataValues.get(JcrConstants.JCR_CREATED_BY, USER_DEACTIVATION_SERVICE));
                metadataValues.put(JcrConstants.JCR_LAST_MODIFIED_BY, initiator);
                resourceResolver.commit();
            }
        }
    }

    /**
     * Method to check if the folder is empty after the assets are moved
     *
     * @param media - Media Folder Resource
     * @return
     */
    protected boolean isFolderEmpty(Resource media) {
        if (media.hasChildren()) {
            Iterator<Resource> children = media.listChildren();
            while (children.hasNext()) {
                if (!StringUtils.equals(JcrConstants.JCR_CONTENT, children.next().getName()) || !StringUtils.equals("rep:policy", children.next().getName())) {
                    return Boolean.FALSE;
                }
            }
        }
        return Boolean.TRUE;
    }

    /**
     * Method to find the media folder if exists
     *
     * @param resourceResolver - resorce resolver to get resource by path
     * @param payloadPath      - path of payload
     * @return - media parent folder resource if exists
     */
    protected Resource findMediaFolderPath(ResourceResolver resourceResolver, String payloadPath) {
        Resource payload = resourceResolver.getResource(payloadPath);

        if (payload != null && payload.getParent().getChild(JcrConstants.JCR_CONTENT) != null && payload.getParent().getChild(
                JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null) {
            String isBnppMedia = payload.getParent().getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap().get("bnpp-media", Boolean.FALSE.toString());
            if (StringUtils.equals(isBnppMedia, Boolean.TRUE.toString())) {
                payload = payload.getParent();
                return payload;
            }
        }
        return null;
    }
}