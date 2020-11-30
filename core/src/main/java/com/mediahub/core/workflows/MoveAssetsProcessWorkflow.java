package com.mediahub.core.workflows;


import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : MOVE ASSET BEFORE PUBLISH"})
public class MoveAssetsProcessWorkflow implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(MoveAssetsProcessWorkflow.class);


    @Reference
    private ResourceResolverFactory resolverFactory;

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

        if(!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")){
            throw new WorkflowException("Impossible de recupérer le PayLoad");
        }

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        ResourceResolver resourceResolver = null;
        Session session = null;
        try {
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            session = resourceResolver.adaptTo(Session.class);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            log.info("payloadPath : {}",payloadPath);
            Resource payload = resourceResolver.getResource(payloadPath);
            Resource media = findMediaFolderPath(resourceResolver, payloadPath);
            // Get the project associated
            // Get the target PATH
            // Copy the folder / asset and subnodes
            if(StringUtils.contains(payload.getPath(),"/dam/")){

                Resource project = null;
                if(media != null){
                    project = resourceResolver.getResource(StringUtils.replace(media.getParent().getPath(),"/dam",StringUtils.EMPTY));
                } else {
                    project = resourceResolver.getResource(StringUtils.replace(payload.getParent().getPath(),"/dam",StringUtils.EMPTY));
                }


                if(project != null && (project.getChild(JcrConstants.JCR_CONTENT) != null)){
                    String projectDamPath = project.getChild(JcrConstants.JCR_CONTENT).getValueMap().get("project.path", StringUtils.EMPTY);
                    // Due to UUID Issue using session.move instead or resolver.move
                    Resource damPath = resourceResolver.getResource(projectDamPath);
                    moveProjectDamAsset(resourceResolver, session, payload, media, projectDamPath,
                        damPath);
                    WorkflowUtils.updateWorkflowPayload(workItem, workflowSession, projectDamPath);
                }
            }
            session.save();
            resourceResolver.commit();
        } catch (LoginException e) {
            throw new WorkflowException("Login exception", e);
        } catch (PersistenceException | RepositoryException e) {
            throw new WorkflowException("Persistence exception", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }
    }

    /**
     * Method to move asset from source to destination
     *
     * @param resourceResolver
     * @param session - session to copy or move node due to UUID
     * @param payload - Payload from workflow
     * @param media - Parent media folder resource
     * @param projectDamPath - path of the dam to be migrated
     * @param damPath - dam resource
     * @throws PersistenceException
     * @throws RepositoryException
     */
    private void moveProjectDamAsset(ResourceResolver resourceResolver, Session session,
        Resource payload, Resource media, String projectDamPath, Resource damPath)
        throws PersistenceException, RepositoryException {
        if(damPath != null){
            if(media != null){
                Resource desitnationMedia = damPath.getChild(media.getName());
                if(desitnationMedia == null ) {
                    Map<String, Object> folderProperties = new HashMap<>();
                    folderProperties.put(JcrConstants.JCR_PRIMARYTYPE, media.getValueMap().get(JcrConstants.JCR_PRIMARYTYPE, BnpConstants.SLING_FOLDER));
                    desitnationMedia = resourceResolver.create(damPath, media.getName(), folderProperties);
                    resourceResolver.commit();
                    Resource contentResource = media.getChild(JcrConstants.JCR_CONTENT);
                    if(null != contentResource){
                        session.getWorkspace().copy(contentResource.getPath(), desitnationMedia.getPath() + "/" + JcrConstants.JCR_CONTENT);
                    }
                }
                session.move(payload.getPath(), desitnationMedia.getPath() + "/" + payload.getName());
            } else {
                session.move(payload.getPath(), projectDamPath + "/" + payload.getName());
            }
        }
    }

    /**
     * Method to find the media folder if exists
     *
     * @param resourceResolver - resorce resolver to get resource by path
     * @param payloadPath - path of payload
     * @return - media parent folder resource if exists
     */
    private Resource findMediaFolderPath(ResourceResolver resourceResolver, String payloadPath) {
        Resource payload = resourceResolver.getResource(payloadPath);

        if(payload.getParent().getChild(JcrConstants.JCR_CONTENT) != null && payload.getParent().getChild(
            JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null){
            String isBnppMedia = payload.getParent().getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap().get("bnpp-media", Boolean.FALSE.toString());
            if(StringUtils.equals(isBnppMedia, Boolean.TRUE.toString())){
                payload = payload.getParent();
                return payload;
            }
        }
        return null;
    }
}