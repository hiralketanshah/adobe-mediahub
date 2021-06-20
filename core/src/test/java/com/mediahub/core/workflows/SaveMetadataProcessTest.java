package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.Collections;
import java.util.Map;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
@MockitoSettings(strictness = Strictness.LENIENT)
public class SaveMetadataProcessTest {

  @InjectMocks
  SaveMetadataProcess workflowProcess = new SaveMetadataProcess();

  @Mock
  private ResourceResolverFactory resolverFactory;

  @Mock
  private ResourceResolver resolver;

  @Mock
  WorkItem workItem;

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
  Externalizer externalizer;

  final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
      BnpConstants.WRITE_SERVICE);

  @Test
  public void execute() throws Exception {
    when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(workflowData.getPayload()).thenReturn(payload);
    when(payload.toString()).thenReturn("/content/dam/projects/");
    when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);
    when(resource.getChild(any())).thenReturn(resource);
    when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
    when(resource.getValueMap()).thenReturn(modifiableValueMap);
    when(externalizer.externalLink(any(), any(String.class), any(String.class) )).thenReturn("");
    when(modifiableValueMap.put(any(String.class),eq(BnpConstants.BNPP_INTERNAL_BROADCAST_URL))).thenReturn(BnpConstants.BNPP_INTERNAL_BROADCAST_URL);
    when(modifiableValueMap.put(any(String.class),eq(BnpConstants.BNPP_INTERNAL_FILE_URL))).thenReturn(BnpConstants.BNPP_INTERNAL_FILE_URL);
    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void execute1() throws Exception {
    when(resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
    Assertions.assertThrows(WorkflowException.class, () -> {
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

}
