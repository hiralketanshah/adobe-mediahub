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
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Deactivation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Deactivate Asset"})
public class AssetDeactivateWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(AssetDeactivateWorkflowProcess.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {
        if (!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {
            throw new WorkflowException("Unable to get the payload");
        }

        ResourceResolver resourceResolver = null;
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (payload != null && payload.getChild(JcrConstants.JCR_CONTENT) != null && payload.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null) {
                ValueMap valueMap = payload.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
                if ( (!StringUtils.isEmpty(valueMap.get(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, String.class))) || (!StringUtils.isEmpty(valueMap.get(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, String.class))) ) {
                    String workflowName = "/var/workflow/models/mediahub/mediahub---scene-7-deactivation";
                    WorkflowModel wfModel = workflowSession.getModel(workflowName);
                    WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                    workflowSession.startWorkflow(wfModel, wfData);
                }
                if ( (!StringUtils.isEmpty(valueMap.get(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, String.class))) || (!StringUtils.isEmpty(valueMap.get(BnpConstants.BNPP_INTERNAL_FILE_URL, String.class))) ) {
                    String workflowName = "/var/workflow/models/mediahub/mediahub---internal-deactivation";
                    WorkflowModel wfModel = workflowSession.getModel(workflowName);
                    WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                    workflowSession.startWorkflow(wfModel, wfData);
                }

            }
        } catch (LoginException e) {
            throw new WorkflowException("Error while unpublishing asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }


    }
}
