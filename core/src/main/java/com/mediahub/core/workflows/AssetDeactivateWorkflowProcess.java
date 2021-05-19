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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Abuthahir Ibrahim
 *
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
    if(!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")){
      throw new WorkflowException("Impossible de recupérer le PayLoad");
    }

    ResourceResolver resourceResolver = null;
    try {
      final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
          BnpConstants.WRITE_SERVICE);
      resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
      String payloadPath = workItem.getWorkflowData().getPayload().toString();
      Resource payload = resourceResolver.getResource(payloadPath);
      if(payload != null && payload.getChild(JcrConstants.JCR_CONTENT) != null && payload.getChild(JcrConstants.JCR_CONTENT).getChild("metadata") != null){
        ValueMap properties = payload.getChild(JcrConstants.JCR_CONTENT).getChild("metadata").getValueMap();
        if(properties.containsKey("dam:scene7ID") && StringUtils.isNotBlank(properties.get("dam:scene7ID",
            StringUtils.EMPTY))){
          String workflowName = "/var/workflow/models/mediahub/mediahub---scene-7-deactivation";
          WorkflowModel wfModel = workflowSession.getModel(workflowName);
          WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
          workflowSession.startWorkflow(wfModel, wfData);
        }

        if(properties.containsKey("bnpp-internal-broadcast-url") && StringUtils.isNotBlank(properties.get("bnpp-internal-broadcast-url",
            StringUtils.EMPTY))){
          String workflowName = "/var/workflow/models/mediahub/mediahub---internal-deactivation";
          WorkflowModel wfModel = workflowSession.getModel(workflowName);
          WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", workItem.getWorkflowData().getPayload().toString());
          workflowSession.startWorkflow(wfModel, wfData);
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
