package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.DamConstants;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import java.util.Calendar;
import java.util.Collections;
import java.util.Map;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.JobManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith({ AemContextExtension.class })
public class PublishScene7VideoAssetTest {

    @InjectMocks
    PublishScene7VideoAsset workflowProcess = new PublishScene7VideoAsset();

    @Mock
    private ResourceResolverFactory resolverFactory;

    @Mock
    private ResourceResolver resolver;
    
    @Mock
    JobManager jobManager;

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

    @Mock
    Asset asset;
    
    @Mock
    Job job;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);


    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
     
    }

    @Test
    public void execute() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(jobManager.addJob(Mockito.anyString(), Mockito.any())).thenReturn(job);
        when(job.getId()).thenReturn("1234");
        when(job.getFinishedDate()).thenReturn(Calendar.getInstance());
        when(job.getJobState()).thenReturn(Job.JobState.SUCCEEDED);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(payload.toString()).thenReturn("/content/dam/projects/");
        when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
        when(resource.adaptTo(Asset.class)).thenReturn(asset);
        when(asset.getMetadataValue(Mockito.anyString())).thenReturn("PublishComplete");
        when(resource.getResourceType()).thenReturn(DamConstants.NT_DAM_ASSET);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getPath()).thenReturn("/content/dam/projects/test");
        when(asset.getPath()).thenReturn("/content/dam/projects/test");
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(resource.getValueMap()).thenReturn(modifiableValueMap);
        when(externalizer.externalLink(any(), any(String.class), any(String.class))).thenReturn("");
        when(modifiableValueMap.put(any(String.class), eq(BnpConstants.BNPP_INTERNAL_BROADCAST_URL)))
                .thenReturn(BnpConstants.BNPP_INTERNAL_BROADCAST_URL);
        when(modifiableValueMap.put(any(String.class), eq(BnpConstants.BNPP_INTERNAL_FILE_URL)))
                .thenReturn(BnpConstants.BNPP_INTERNAL_FILE_URL);
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }
    
    @Test
    public void executeError() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(LoginException.class);
        
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

}
