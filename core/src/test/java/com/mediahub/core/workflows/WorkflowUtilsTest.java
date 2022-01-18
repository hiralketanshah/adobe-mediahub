package com.mediahub.core.workflows;

import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class WorkflowUtilsTest {

    @InjectMocks
    WorkflowUtils workflowUtil;

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
    Workflow workflow;
    
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
    
    Set<String> setOfKeys = new HashSet<>();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void updateWrkflowPayloadTest() throws Exception {
        setOfKeys.add("test");
        when(workflowSession.newWorkflowData(Mockito.any(String.class), Mockito.any())).thenReturn(workflowData);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workItem.getWorkflow()).thenReturn(workflow);
        when(workflowData.getMetaDataMap()).thenReturn(metadataMap);
        when(metadataMap.keySet()).thenReturn(setOfKeys);
        when(workItem.getWorkflow()).thenReturn(workflow);
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowUtil.updateWorkflowPayload(workItem, workflowSession, "Test-content-path");
        });
    }
}
