package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class ActivateWorkflowProcessTest {

    @InjectMocks
    ActivateWorkflowProcess workflowProcess = new ActivateWorkflowProcess();

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
    ValueMap valueMap;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(payload.toString()).thenReturn("/content/dam/projects/");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
    }

    @Test
    public void execute() throws Exception {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(valueMap.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS, new String[] {}))
                .thenReturn(new String[] { "not-broadcast" });
        when(valueMap.containsKey("dam:scene7ID")).thenReturn(Boolean.TRUE);
        when(valueMap.get("dam:scene7ID", StringUtils.EMPTY)).thenReturn("a|562043580");
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute1() throws Exception {
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(workflowData.getPayloadType()).thenReturn("");
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    @Test
    public void execute2() throws Exception {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(valueMap.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS, new String[] {}))
                .thenReturn(new String[] { "not-broadcast" });
        when(valueMap.containsKey(BnpConstants.BNPP_INTERNAL_BROADCAST_URL)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, String.class))
                .thenReturn("/content/dam/medialibrary/table.pdf");
        when(valueMap.get(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, String.class)).thenReturn("external");
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute3() throws Exception {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(valueMap.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(new String[] { "external" });
        when(valueMap.containsKey("dam:scene7ID")).thenReturn(Boolean.TRUE);
        when(valueMap.get("dam:scene7ID", StringUtils.EMPTY)).thenReturn("a|562043580");
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute4() throws Exception {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(valueMap.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS, new String[] {})).thenReturn(new String[] { "internal" });
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(new String[] { "internal" });
        when(valueMap.containsKey("dam:scene7ID")).thenReturn(Boolean.TRUE);
        when(valueMap.get("dam:scene7ID", StringUtils.EMPTY)).thenReturn("a|562043580");
        when(resource.getParent()).thenReturn(resource);
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute5() throws Exception {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

}
