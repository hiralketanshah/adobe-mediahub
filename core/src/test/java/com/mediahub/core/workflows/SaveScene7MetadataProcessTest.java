package com.mediahub.core.workflows;

import static com.mediahub.core.workflows.SaveScene7MetadataProcess.DAM_SCENE_7_TYPE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.constants.Scene7AssetType;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
@MockitoSettings(strictness = Strictness.LENIENT)
public class SaveScene7MetadataProcessTest {

  @InjectMocks
  SaveScene7MetadataProcess workflowProcess = new SaveScene7MetadataProcess();

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
  ModifiableValueMap modifiableValueMap;

  @Mock
  WorkflowSession workflowSession;

  @Mock
  MetaDataMap metadataMap;

  @Mock
  Scene7DeactivationService scene7DeactivationService;

  @Mock
  Resource s7Configresource;

  @Mock
  S7Config s7Config;

  @Mock
  Scene7Service scene7Service;

  final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
      BnpConstants.WRITE_SERVICE);

  @BeforeEach
  public void setUp() throws Exception {
    when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(workflowData.getPayload()).thenReturn(payload);
    when(payload.toString()).thenReturn("/content/dam/projects/");
    when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);
    when(resource.getChild(any())).thenReturn(resource);
    when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);

    when(scene7DeactivationService.getCloudConfigurationPath()).thenReturn("/conf/global/settings/cloudconfigs/dmscene7");
    when(resolver.getResource("/conf/global/settings/cloudconfigs/dmscene7")).thenReturn(s7Configresource);
    when(s7Configresource.adaptTo(S7Config.class)).thenReturn(s7Config);
    when(scene7Service.getAssets(any(), any(), any(), any())).thenReturn(Collections.EMPTY_LIST);
  }

  @Test
  public void execute() throws Exception {
    when(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY)).thenReturn(Scene7AssetType.VIDEO.getValue());
    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void execute1() throws Exception {
    when(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY)).thenReturn(Scene7AssetType.MASTER_VIDEO.getValue());
    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void execute2() throws Exception {
    when(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY)).thenReturn(Scene7AssetType.IMAGE.getValue());
    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void execute3() throws Exception {
    when(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY)).thenReturn(StringUtils.EMPTY);
    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

}
