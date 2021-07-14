package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import javax.jcr.Session;
import javax.servlet.http.HttpServletRequest;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.workflow.WorkflowService;
import com.day.cq.workflow.WorkflowSession;
import com.day.cq.workflow.exec.WorkflowData;
import com.day.cq.workflow.model.WorkflowModel;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class UnpublishMediaFolderTest {

    private final AemContext context = new AemContext();

    @InjectMocks
    private UnpublishMediaFolder unpublishMediaFolder;

    @Mock
    Job job;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    ResourceResolver resolver;

    @Mock
    WorkflowService workflowService;
    
    @Mock
    WorkflowSession workflowSession;
    
    @Mock
    WorkflowModel wfModel;
    
    @Mock
    WorkflowData wfData;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        Resource res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
        context.create().resource("/content/test1/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource("/content/test1/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
                "nt:unstructured", "bnpp-external-file-master-url", "scene7", "bnpp-internal-file-url", "publishedpath");

        context.registerService(WorkflowService.class, workflowService);
        context.registerService(ResourceResolverFactory.class, resolverFactory);

        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.resolve(Mockito.any(String.class))).thenReturn(res);
        when(workflowService.getWorkflowSession( resolver.adaptTo(Session.class))).thenReturn(workflowSession);
        when(workflowSession.getModel("/var/workflow/models/mediahub/mediahub---scene-7-deactivation")).thenReturn(wfModel);
        when(workflowSession.newWorkflowData(Mockito.anyString(), Mockito.anyString())).thenReturn(wfData);
        when(job.getProperty(Mockito.any(String.class))).thenReturn("/content/test1|xyz");
    }

    @Test
    void process() {
        assertEquals(JobConsumer.JobResult.OK, unpublishMediaFolder.process(job));
    }

}
