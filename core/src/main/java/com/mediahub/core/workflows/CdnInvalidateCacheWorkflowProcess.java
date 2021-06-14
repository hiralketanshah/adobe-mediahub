package com.mediahub.core.workflows;


import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.contentsync.handler.util.RequestResponseFactory;
import com.day.cq.wcm.api.WCMMode;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.engine.SlingRequestProcessor;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : CDN Cache Invalidation"})
public class CdnInvalidateCacheWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(CdnInvalidateCacheWorkflowProcess.class);


    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    Scene7DeactivationService scene7DeactivationService;

    @Reference
    RequestResponseFactory requestResponseFactory;

    @Reference
    private SlingRequestProcessor requestProcessor;

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
        throw new WorkflowException("Impossible de recupérer le PayLoad");
      }
          final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
          ResourceResolver resourceResolver = null;
          try {
              resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
              String payloadPath = workItem.getWorkflowData().getPayload().toString();
              log.info("payloadPath : {}", payloadPath);

              Map<String, Object> params = new HashMap<>();
              MetaDataMap dataMap = workItem.getWorkflow().getWorkflowData().getMetaDataMap();
              if(dataMap.containsKey(BnpConstants.BNPP_EXTERNAL_FILE_URL)){
                log.info("Cdn invalidation url : " + dataMap.get(BnpConstants.BNPP_EXTERNAL_FILE_URL, StringUtils.EMPTY));
                params.put("urls", dataMap.get(BnpConstants.BNPP_EXTERNAL_FILE_URL, StringUtils.EMPTY));
              }


              HttpServletRequest req = requestResponseFactory.createRequest("POST", scene7DeactivationService.getCdnCacheInvalidationPath(), params);
              WCMMode.DISABLED.toRequest(req);

              //Setup response
              ByteArrayOutputStream out = new ByteArrayOutputStream();
              HttpServletResponse resp = requestResponseFactory.createResponse(out);

              //Process request through Sling
              requestProcessor.processRequest(req, resp, resourceResolver);
              String html = out.toString();
              log.info("Cdn cache response : " + html);
          } catch (LoginException | ServletException | IOException e) {
              throw new WorkflowException("Login exception", e);
          } finally {
              if (resourceResolver != null && resourceResolver.isLive()) {
                  resourceResolver.close();
              }
          }

    }

}