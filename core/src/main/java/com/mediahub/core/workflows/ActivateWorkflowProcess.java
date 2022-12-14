package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.model.WorkflowModel;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.AssetUtils;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Activate Asset"})
public class ActivateWorkflowProcess implements WorkflowProcess {

    private static final String JCR_PATH = "JCR_PATH";

    @Reference
    ResourceResolverFactory resolverFactory;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {
        if (!workItem.getWorkflowData().getPayloadType().equals(JCR_PATH)) {
            throw new WorkflowException("Unable to get the payload");
        }

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (payload != null && payload.getChild(JcrConstants.JCR_CONTENT) != null && payload.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null) {
                ValueMap properties = payload.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
                if (properties.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)) {
                    String[] status = AssetUtils.getBroadcastStatus(properties, BnpConstants.BNPP_BROADCAST_STATUS);
                    if (Arrays.asList(status).contains(BnpConstants.BROADCAST_VALUE_NOT_BROADCAST)) {
                        // MED-484 Not Broadcast status
                        deactivateAsset(workItem, workflowSession, properties, status);
                    } else {
                        activateAsset(workItem, workflowSession, payload, status);
                        deactivateAsset(workItem, workflowSession, properties, status);
                    }
                }
            }
        } catch (LoginException e) {
            throw new WorkflowException("Error while publishing asset", e);
        }

    }

    /**
     * Activate Asset or Media based on status
     *
     * @param workItem
     * @param workflowSession
     * @param payload
     * @param status
     * @throws WorkflowException
     */
    private void activateAsset(WorkItem workItem, WorkflowSession workflowSession, Resource payload,
        String[] status) throws WorkflowException {
        if (Arrays.asList(status).contains(BnpConstants.BROADCAST_VALUE_EXTERNAL)) {
            String workflowName = "/var/workflow/models/mediahub/mediahub---external-publish";
            WorkflowModel wfModel = workflowSession.getModel(workflowName);
            WorkflowData wfData = workflowSession.newWorkflowData(
                JCR_PATH, workItem.getWorkflowData().getPayload().toString());
            workflowSession.startWorkflow(wfModel, wfData);
        }

        if (Arrays.asList(status).contains(BnpConstants.BROADCAST_VALUE_INTERNAL)) {
            String workflowName = "/var/workflow/models/mediahub/mediahub---internal-publish";
            WorkflowModel wfModel = workflowSession.getModel(workflowName);
            WorkflowData wfData = workflowSession.newWorkflowData(
                JCR_PATH, workItem.getWorkflowData().getPayload().toString());
            workflowSession.startWorkflow(wfModel, wfData);
            if (payload.getParent() != null) {
                WorkflowData parentWorkflowData = workflowSession.newWorkflowData(
                    JCR_PATH, payload.getParent().getPath());
                workflowSession.startWorkflow(wfModel, parentWorkflowData);
            }
        }
    }

    /**
     * Deactivate asset or media based on status. It is also used to toggle between External and Internal publish
     *
     * @param workItem
     * @param workflowSession
     * @param properties
     * @param status
     * @throws WorkflowException
     */
    private void deactivateAsset(WorkItem workItem, WorkflowSession workflowSession,
        ValueMap properties, String[] status) throws WorkflowException {
        if ((!Arrays.asList(status).contains(BnpConstants.BROADCAST_VALUE_EXTERNAL)) && !StringUtils
            .isEmpty(properties.get(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, String.class))) {
            String workflowName = "/var/workflow/models/mediahub/mediahub---scene-7-deactivation";
            WorkflowModel wfModel = workflowSession.getModel(workflowName);
            WorkflowData wfData = workflowSession.newWorkflowData(
                JCR_PATH, workItem.getWorkflowData().getPayload().toString());
            workflowSession.startWorkflow(wfModel, wfData);
        }

        if ((!Arrays.asList(status).contains(BnpConstants.BROADCAST_VALUE_INTERNAL)) && !StringUtils.isEmpty(properties.get(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, String.class))) {
            String workflowName = "/var/workflow/models/mediahub/mediahub---internal-deactivation";
            WorkflowModel wfModel = workflowSession.getModel(workflowName);
            WorkflowData wfData = workflowSession.newWorkflowData(
                JCR_PATH, workItem.getWorkflowData().getPayload().toString());
            workflowSession.startWorkflow(wfModel, wfData);
        }
    }
}
