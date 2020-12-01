package com.mediahub.core.workflows;

import static org.mockito.Mockito.when;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.sling.api.resource.LoginException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
public class ValidateMetadataProcessWorkflowTest {

  MetaDataMap metadataMap;

  @Mock
  WorkflowSession workflowSession;

  @Mock
  WorkItem workItem;

  @Mock
  WorkflowData workflowData;

  @InjectMocks
  ValidateMedataProcessWorkflow workflowProcess = new ValidateMedataProcessWorkflow();

  @BeforeEach
  public void setUp() throws Exception {
    metadataMap = new SimpleMetaDataMap();
    when(workItem.getWorkflowData()).thenReturn(workflowData);
  }

  @Test
  public void execute() throws Exception {

    when(workflowData.getPayloadType()).thenReturn("dummy");
    Assertions.assertThrows(WorkflowException.class, () ->{
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

}
