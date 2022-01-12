package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
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
import org.mockito.Mockito;
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
public class AssetDeactivateWorkflowProcessTest {

    @InjectMocks
    AssetDeactivateWorkflowProcess workflowProcess = new AssetDeactivateWorkflowProcess();

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
        when(resource.getValueMap()).thenReturn(valueMap);
    }

    @Test
    public void execute() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(payload.toString()).thenReturn("/content/dam/projects/");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);

        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(ValueMap.class)).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.anyString())).thenReturn(Boolean.TRUE);
        when(valueMap.get(Mockito.anyString(), Mockito.any())).thenReturn("dam:scene7ID");

        // when(modifiableValueMap.put(any(String.class),eq("dam:scene7ID"))).thenReturn("dam:scene7ID");

        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute1() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(payload.toString()).thenReturn("/content/dam/projects/");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);

        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(ValueMap.class)).thenReturn(valueMap);
        when(valueMap.containsKey("bnpp-internal-broadcast-url")).thenReturn(Boolean.TRUE);
        when(valueMap.get("bnpp-internal-broadcast-url", StringUtils.EMPTY))
                .thenReturn("bnpp-internal-broadcast-url");

        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute2() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    @Test
    public void execute3() throws Exception {
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(workflowData.getPayloadType()).thenReturn(StringUtils.EMPTY);

        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

}
