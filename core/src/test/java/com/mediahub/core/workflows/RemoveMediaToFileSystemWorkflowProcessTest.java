package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

@ExtendWith({ AemContextExtension.class })
public class RemoveMediaToFileSystemWorkflowProcessTest {

    private final AemContext context = new AemContext();

    private static final String PATH = "/content/projects/corporate_institutionalbankingcib.zip";

    @Mock
    WorkItem workItem;

    @Mock
    WorkflowData workflowData;

    @Mock
    WorkflowSession workflowSession;

    @Mock
    MetaDataMap metadataMap;

    @InjectMocks
    RemoveMediaToFileSystemWorkflowProcess workflowProcess;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        workflowProcess = context.registerInjectActivateService(new RemoveMediaToFileSystemWorkflowProcess());
    }

    @Test
    public void execute() throws Exception {
        when(workflowData.getPayload()).thenReturn(PATH);
        assertAll(() -> workflowProcess.execute(workItem, workflowSession, metadataMap));
    }
}
