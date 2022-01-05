package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.sling.api.resource.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import javax.jcr.*;

import java.util.*;

import static org.mockito.Mockito.when;

@ExtendWith({ AemContextExtension.class })
public class SaveMediaToFileSystemWorkflowProcessTest {

    private final AemContext context = new AemContext();
    private static final String PATH = "/content/dam/sitecheck.zip";

    @Mock
    WorkItem workItem;

    @Mock
    WorkflowSession wfsession;

    @Mock
    WorkflowData workflowData;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resolver;

    @Mock
    Session session;

    @Mock
    Resource resource;

    @Mock
    MetaDataMap metadataMap;

    @Mock
    ValueMap valueMap;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @InjectMocks
    private SaveMediaToFileSystemWorkflowProcess workflowProcess = new SaveMediaToFileSystemWorkflowProcess();

    Resource res;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        resourceResolverFactory = context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        workflowProcess = context.registerInjectActivateService(workflowProcess);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(workflowData.getPayload()).thenReturn(PATH);
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.adaptTo(ValueMap.class)).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.any())).thenReturn(true);
        when(valueMap.get(Mockito.anyString(), Mockito.any())).thenReturn("false");
        workflowProcess.resolverFactory = resourceResolverFactory;

    }

    @Test
    public void execute() throws Exception {
        when(resolver.getResource(Mockito.anyString())).thenReturn(resource);

        workflowProcess.execute(workItem, wfsession, metadataMap);
    }

}
