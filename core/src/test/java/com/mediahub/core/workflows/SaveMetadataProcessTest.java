package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
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
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.DamConstants;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.day.cq.replication.Replicator;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
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
    Asset asset;

    @Mock
    MetaDataMap metadataMap;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Externalizer externalizer;

    @Mock
    S7Config s7Config;

    @Mock
    Scene7DeactivationService scene7DeactivationService;

    @Mock
    Scene7Service scene7Service;

    @Mock
    Scene7Asset scene7Asset;

    @Mock
    Replicator replicator;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(payload);
        when(payload.toString()).thenReturn("/content/dam/medialibrary");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);
        when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
        when(resource.adaptTo(S7Config.class)).thenReturn(s7Config);
        when(resource.getResourceType()).thenReturn(DamConstants.NT_DAM_ASSET);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(Asset.class)).thenReturn(asset);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(resource.getValueMap()).thenReturn(modifiableValueMap);
        when(externalizer.externalLink(any(), any(String.class), any(String.class))).thenReturn("");
        when(modifiableValueMap.put(any(String.class), eq(BnpConstants.BNPP_INTERNAL_BROADCAST_URL)))
                .thenReturn(BnpConstants.BNPP_INTERNAL_BROADCAST_URL);
        when(modifiableValueMap.put(any(String.class), eq(BnpConstants.BNPP_INTERNAL_FILE_URL)))
                .thenReturn(BnpConstants.BNPP_INTERNAL_FILE_URL);

        when(asset.getName()).thenReturn("test.mkv");

        when(scene7DeactivationService.getCloudConfigurationPath()).thenReturn("testpath");

        List<Scene7Asset> listOfScene7Asset = new ArrayList<>();
        listOfScene7Asset.add(scene7Asset);
        when(scene7Service.getAssets(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(listOfScene7Asset);
        when(scene7Service.getAssociatedAssets(Mockito.any(), Mockito.any())).thenReturn(scene7Asset);
        when(scene7Asset.getSubAssets()).thenReturn(listOfScene7Asset);

    }

    @Test
    public void execute540L() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        when(scene7Asset.getHeight()).thenReturn(540L);
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute720L() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        when(scene7Asset.getHeight()).thenReturn(720L);
        workflowProcess.execute(workItem, workflowSession, metadataMap);
    }

    @Test
    public void execute1080L() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        when(scene7Asset.getHeight()).thenReturn(1080L);
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
