package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;

import java.util.Calendar;
import java.util.Collections;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.JobManager;
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
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class UnpublishDynamicMediaProcessTest {

    @InjectMocks
    UnpublishDynamicMediaProcess workflowProcess = new UnpublishDynamicMediaProcess();

    @Mock
    private ResourceResolverFactory resolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    WorkflowData workflowData;

    @Mock
    WorkflowSession workflowSession;

    @Mock
    WorkItem workItem;

    @Mock
    Payload payload;

    @Mock
    MetaDataMap metadataMap;

    @Mock
    Resource resource;

    @Mock
    Resource s7Configresource;

    @Mock
    S7Config s7Config;

    @Mock
    ValueMap valueMap;

    @Mock
    Scene7DeactivationService scene7DeactivationService;

    @Mock
    Scene7Service scene7Service;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Workflow workflow;
    
    @Mock
    JobManager jobManager;
    
    @Mock
    Job job;

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
        when(scene7DeactivationService.getCloudConfigurationPath())
                .thenReturn("/conf/global/settings/cloudconfigs/dmscene7");
        when(resolver.getResource("/conf/global/settings/cloudconfigs/dmscene7")).thenReturn(s7Configresource);
        when(s7Configresource.adaptTo(S7Config.class)).thenReturn(s7Config);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("dam:scene7ID", StringUtils.EMPTY)).thenReturn("a|562043580");
        when(valueMap.get(Mockito.anyString(), Mockito.any())).thenReturn("a|562043580");
        when(resource.getPath()).thenReturn("testPath");
        when(jobManager.addJob(Mockito.anyString(), Mockito.any())).thenReturn(job);
        when(job.getId()).thenReturn("12345");
        when(job.getFinishedDate()).thenReturn(Calendar.getInstance());
        when(job.getJobState()).thenReturn(Job.JobState.SUCCEEDED);
    }

    @Test
    public void execute() throws Exception {
        when(workItem.getWorkflow()).thenReturn(workflow);
        when(workflow.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getMetaDataMap()).thenReturn(metadataMap);
        when(scene7Service.deleteAsset("a|562043580", s7Config)).thenReturn("failure");
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    @Test
    public void execute1() throws Exception {
        when(workItem.getWorkflow()).thenReturn(workflow);
        when(workflow.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getMetaDataMap()).thenReturn(metadataMap);
        when(scene7Service.deleteAsset("a|562043580", s7Config)).thenReturn("success");
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(modifiableValueMap.remove(any())).thenReturn(true);
        workflowProcess.execute(workItem, workflowSession, metadataMap);
        assertEquals(scene7Service.deleteAsset("a|562043580", s7Config), "success");
    }

    @Test
    public void execute2() throws Exception {
        when(workItem.getWorkflow()).thenReturn(workflow);
        when(workflow.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getMetaDataMap()).thenReturn(metadataMap);
        when(s7Configresource.adaptTo(S7Config.class)).thenReturn(null);
        when(scene7Service.deleteAsset("a|562043580", s7Config)).thenReturn("failure");
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

}
