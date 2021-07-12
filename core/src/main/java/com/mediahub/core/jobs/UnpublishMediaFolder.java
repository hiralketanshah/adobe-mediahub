package com.mediahub.core.jobs;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.workflow.WorkflowException;
import com.day.cq.workflow.WorkflowService;
import com.day.cq.workflow.WorkflowSession;
import com.day.cq.workflow.exec.WorkflowData;
import com.day.cq.workflow.model.WorkflowModel;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.Map;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = JobConsumer.class, immediate = true,
    property = {Constants.SERVICE_DESCRIPTION + "=Job to unpublish assets inside media",
        JobConsumer.PROPERTY_TOPICS + "=" + "unpublish/media/folder"})
/**
 * This job will invalidate scene7 cache JOB.
 *
 */
public class UnpublishMediaFolder implements JobConsumer {

  private static final Logger LOGGER = LoggerFactory.getLogger(UnpublishMediaFolder.class);

  @Reference
  ResourceResolverFactory resolverFactory;

  @Reference
  WorkflowService workflowService;

  @Override
  public JobResult process(Job job) {
    String payload = job.getProperty("offloading.input.payload").toString();

    if(StringUtils.isBlank(payload)){
      return JobResult.OK;
    }

    if(payload.contains("|")){
      payload = job.getProperty("offloading.input.payload").toString().split("\\|")[0];
    }

    LOGGER.debug("Asset path {}", payload);

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      Resource payloadResource = resourceResolver.resolve(payload);


          if(DamUtil.isAsset(payloadResource)){
            ModifiableValueMap valueMap = payloadResource.getChild(JcrConstants.JCR_CONTENT).getChild("metadata").adaptTo(
                ModifiableValueMap.class);
            String scene7Path = valueMap.getOrDefault(BnpConstants.BNPP_EXTERNAL_FILE_URL, StringUtils.EMPTY).toString();
            String publishPath = valueMap.getOrDefault(BnpConstants.BNPP_INTERNAL_FILE_URL, StringUtils.EMPTY).toString();

            WorkflowSession workflowSession = workflowService.getWorkflowSession(resourceResolver.adaptTo(
                Session.class));

            if(StringUtils.isNotBlank(scene7Path)){
              String workflowName = "/var/workflow/models/mediahub/mediahub---scene-7-deactivation";
              startWorkflow(job, workflowSession, workflowName, payloadResource.getPath());
            }

            if(StringUtils.isNotBlank(publishPath)){
              String workflowName = "/var/workflow/models/mediahub/mediahub---internal-deactivation";
              startWorkflow(job, workflowSession, workflowName, payloadResource.getPath());
            }

            valueMap.remove("bnpp-broadcast-status");
            resourceResolver.commit();
          }

    } catch (Exception e) {
      LOGGER.error("Error while unpublishing media", e);
      return JobResult.FAILED;
    }
    return JobResult.OK;
  }

  /**
   * @param job
   * @param workflowSession
   * @param workflowName
   */
  private void startWorkflow(Job job, WorkflowSession workflowSession, String workflowName, String assetPath) {
    try {
      WorkflowModel wfModel = workflowSession.getModel(workflowName);
      WorkflowData wfData = workflowSession.newWorkflowData("JCR_PATH", assetPath);
      workflowSession.startWorkflow(wfModel, wfData);
    } catch (WorkflowException e) {
      LOGGER.error("Error while starting unpublishing workflows", e);
    }

  }
}
