package com.mediahub.core.servlets;

import com.day.cq.workflow.WorkflowException;
import com.day.cq.workflow.WorkflowService;
import com.day.cq.workflow.WorkflowSession;
import com.day.cq.workflow.model.WorkflowModel;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ResponseUtil;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.jcr.Session;
import javax.servlet.Servlet;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(
    service = Servlet.class,
    property = {
        Constants.SERVICE_DESCRIPTION + "=Trigger Worklow Servlet",
        "sling.servlet.methods=" + HttpConstants.METHOD_GET,
        "sling.servlet.paths=" + "/bin/mediahub/assetpublish"
    }
)
public class MultiAssetTriggerWorflowServlet extends SlingSafeMethodsServlet {

  private static final Logger log = LoggerFactory.getLogger(MetadataUpdaterServlet.class);

  @Reference
  private transient ResourceResolverFactory resolverFactory;

  @Reference
  private transient WorkflowService workflowService;

  private static final long serialVersionUID = 1L;

  @Override
  protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response)
      throws IOException {
    Map<String, String[]> parameterMap = request.getParameterMap();
      final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
          BnpConstants.WRITE_SERVICE);
      Map<String, Object> responseMap = new HashMap<>();

      try(ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)){
        String[] paths = parameterMap.get(BnpConstants.PATH);
        final WorkflowSession workflowSession = workflowService.getWorkflowSession(adminResolver.adaptTo(Session.class));
        final WorkflowModel workflowModel = workflowSession.getModel("/var/workflow/models/mediahub/mediahub---validation");
        for(String payloadPath : paths){
          workflowSession.startWorkflow(workflowModel, workflowSession.newWorkflowData("JCR_PATH", payloadPath));
        }
        responseMap.put("message", "The Assets will be deacitivated in sometime");
        ResponseUtil.setJsonResponse(200, response, responseMap);
      } catch (IOException | LoginException | WorkflowException e) {
        log.error("Error while unpublishing assets inside media : {0}", e);
        responseMap.put("message", e.getMessage());
        ResponseUtil.setJsonResponse(400, response, responseMap);
      }

  }

}
