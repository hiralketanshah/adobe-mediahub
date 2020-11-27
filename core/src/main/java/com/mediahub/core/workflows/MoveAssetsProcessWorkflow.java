package com.mediahub.core.workflows;


import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
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
            Resource payload = setCorrectPayloadPath(resourceResolver, payloadPath);
            // Get the project associated
            // Get the target PATH
            // Copy the folder / asset and subnodes
            if(StringUtils.contains(payload.getPath(),"/dam/")){
                Resource project = resourceResolver.getResource(StringUtils.replace(payload.getParent().getPath(),"/dam",StringUtils.EMPTY));
                if(project != null && (project.getChild(JcrConstants.JCR_CONTENT) != null)){
                    String projectDamPath = project.getChild(JcrConstants.JCR_CONTENT).getValueMap().get("project.path", StringUtils.EMPTY);
                    // Due to UUID Issue using session.move instead or resolver.move
                    Resource damPath = resourceResolver.getResource(projectDamPath);
                    if(damPath != null && damPath.getChild(payload.getName()) == null){
                        session.move(payload.getPath(), projectDamPath + "/" + payload.getName());
                    }
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

    private Resource setCorrectPayloadPath(ResourceResolver resourceResolver, String payloadPath) {
        Resource payload = resourceResolver.getResource(payloadPath);

        if(payload.getParent().getChild("jcr:content") != null && payload.getParent().getChild(
            JcrConstants.JCR_CONTENT).getChild("metadata") != null){
            String isBnppMedia = payload.getParent().getChild(JcrConstants.JCR_CONTENT).getChild("metadata").getValueMap().get("bnpp-media", Boolean.FALSE.toString());
            if(StringUtils.equals(isBnppMedia, Boolean.TRUE.toString())){
                payload = payload.getParent();
            }
        }
        return payload;
    }
}