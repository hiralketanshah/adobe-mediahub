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

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Activate Asset"})
public class ActivateWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(ActivateWorkflowProcess.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {
        if (!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {
            throw new WorkflowException("Impossible de recupérer le PayLoad");
        }

        ResourceResolver resourceResolver = null;
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (payload != null && payload.getChild(JcrConstants.JCR_CONTENT) != null && payload.getChild(JcrConstants.JCR_CONTENT).getChild("metadata") != null) {
                ValueMap properties = payload.getChild(JcrConstants.JCR_CONTENT).getChild("metadata").getValueMap();
                if (properties.containsKey("bnpp-broadcast-status")) {
                    String[] stasus = properties.get("bnpp-broadcast-status", new String[]{});

                    if (Arrays.asList(stasus).contains("not-broadcast")) {
                        // for future requirement
                    } else {

                        if (Arrays.asList(stasus).contains("external")) {
                            String workflowName = "/var/workflow/models/mediahub/mediahub---external-publish";
                            WorkflowModel wfModel = workflowSession.getModel(workflowName);
                            WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                            workflowSession.startWorkflow(wfModel, wfData);
                        }

                        if (Arrays.asList(stasus).contains("internal")) {
                            String workflowName = "/var/workflow/models/mediahub/mediahub---internal-publish";
                            WorkflowModel wfModel = workflowSession.getModel(workflowName);
                            WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                            workflowSession.startWorkflow(wfModel, wfData);
                            if (payload.getParent() != null) {
                                WorkflowData parentWorkflowData = workflowSession.newWorkflowData("JCR_PATH", payload.getParent().getPath());
                                workflowSession.startWorkflow(wfModel, parentWorkflowData);
                            }
                        }

                        if ((!Arrays.asList(stasus).contains("external")) && properties.containsKey("dam:scene7ID") && StringUtils.isNotBlank(properties.get("dam:scene7ID", StringUtils.EMPTY))) {
                            String workflowName = "/var/workflow/models/mediahub/mediahub---scene-7-deactivation";
                            WorkflowModel wfModel = workflowSession.getModel(workflowName);
                            WorkflowData wfData = workflowSession
                                    .newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                            workflowSession.startWorkflow(wfModel, wfData);
                        }

                        if ((!Arrays.asList(stasus).contains("internal")) && properties.containsKey(BnpConstants.BNPP_INTERNAL_BROADCAST_URL) && StringUtils.isNotBlank(properties.get(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, StringUtils.EMPTY))) {
                            String workflowName = "/var/workflow/models/mediahub/mediahub---internal-deactivation";
                            WorkflowModel wfModel = workflowSession.getModel(workflowName);
                            WorkflowData wfData = workflowSession
                                    .newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
                            workflowSession.startWorkflow(wfModel, wfData);
                        }

                    }
                }
            }
        } catch (LoginException e) {
            log.error("LoginException occured : {}", e.getMessage());
            throw new WorkflowException("Login exception", e);
        } catch (Exception e) {
            log.error("Exception occured : {}", e.getMessage());
            throw new WorkflowException("Login exception", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }


    }
}
