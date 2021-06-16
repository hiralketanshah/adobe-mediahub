package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.contentsync.handler.util.RequestResponseFactory;
import com.day.cq.wcm.api.WCMMode;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.ByteArrayOutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.engine.SlingRequestProcessor;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.osgi.service.component.annotations.Reference;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
@MockitoSettings(strictness = Strictness.LENIENT)
public class CdnInvalidateCacheWorkflowProcessTest {

  @InjectMocks
  CdnInvalidateCacheWorkflowProcess workflowProcess = new CdnInvalidateCacheWorkflowProcess();

  @Mock
  private ResourceResolverFactory resolverFactory;

  @Mock
  private ResourceResolver resolver;

  @Mock
  WorkItem workItem;

  @Mock
  Workflow workflow;

  @Mock
  WorkflowData workflowData;

  @Mock
  Payload payload;

  @Mock
  Resource resource;

  @Mock
  WorkflowSession workflowSession;

  @Mock
  MetaDataMap metadataMap;

  @Mock
  ModifiableValueMap modifiableValueMap;

  @Mock
  RequestResponseFactory requestResponseFactory;

  @Mock
  Scene7DeactivationService scene7DeactivationService;

  @Mock
  HttpServletRequest request;

  @Mock
  HttpServletResponse response;

  @Mock
  SlingRequestProcessor requestProcessor;

  final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
      BnpConstants.WRITE_SERVICE);

  @Test
  public void execute() throws Exception {
    when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(workItem.getWorkflow()).thenReturn(workflow);
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(workflow.getWorkflowData()).thenReturn(workflowData);
    when(workflowData.getPayload()).thenReturn(payload);
    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowData.getMetaDataMap()).thenReturn(metadataMap);
    when(payload.toString()).thenReturn("/content/dam/projects/");

    when(resource.getChild(any())).thenReturn(resource);
    when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
    when(modifiableValueMap.containsKey("dam:scene7ID")).thenReturn(Boolean.TRUE);
    when(modifiableValueMap.get("dam:scene7ID", StringUtils.EMPTY)).thenReturn("dam:scene7ID");

    Map<String, Object> params = new HashMap<>();
    when(scene7DeactivationService.getCdnCacheInvalidationPath()).thenReturn("");
    when(requestResponseFactory.createRequest("POST", "", params)).thenReturn(request);
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    when(requestResponseFactory.createResponse(out)).thenReturn(response);
    doNothing().when(requestProcessor).processRequest(request, response, resolver);

    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void execute1() throws Exception {
    when(resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(workflowData.getPayload()).thenReturn(payload);
    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    Assertions.assertThrows(WorkflowException.class, () -> {
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

  @Test
  public void execute2() throws Exception {
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(workflowData.getPayload()).thenReturn(payload);
    when(workflowData.getPayloadType()).thenReturn(StringUtils.EMPTY);

    Assertions.assertThrows(WorkflowException.class, () -> {
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

}
