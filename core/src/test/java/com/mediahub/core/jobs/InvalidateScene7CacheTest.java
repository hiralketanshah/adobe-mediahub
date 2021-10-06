package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.engine.SlingRequestProcessor;
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
import com.day.cq.contentsync.handler.util.RequestResponseFactory;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class InvalidateScene7CacheTest {

    private final AemContext context = new AemContext();

    @InjectMocks
    private InvalidateScene7Cache invalidateCache;

    @Mock
    Job job;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    ResourceResolver resolver;

    @Mock
    SlingRequestProcessor requestProcessor;

    @Mock
    Scene7DeactivationService scene7DeactivationService;

    @Mock
    RequestResponseFactory requestResponseFactory;

    @Mock
    HttpServletRequest req;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        Resource res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
        context.create().resource("/content/test1/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource("/content/test1/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
                "nt:unstructured", "bnpp-external-file-master-url", "scene7");

        context.registerService(SlingRequestProcessor.class, requestProcessor);
        context.registerService(Scene7DeactivationService.class, scene7DeactivationService);
        context.registerService(RequestResponseFactory.class, requestResponseFactory);
        context.registerService(ResourceResolverFactory.class, resolverFactory);

        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(scene7DeactivationService.getCdnCacheInvalidationPath()).thenReturn("testCache");
        when(requestResponseFactory.createRequest(Mockito.any(String.class), Mockito.any(String.class), Mockito.any()))
                .thenReturn(req);

        when(resolver.resolve(Mockito.any(String.class))).thenReturn(res);
        when(resolver.getResource(Mockito.any(String.class))).thenReturn(res);
        Mockito.when(job.getProperty(Mockito.any(String.class))).thenReturn("/content/test1|xyz");
    }

    @Test
    void process() {
        assertEquals(JobConsumer.JobResult.OK, invalidateCache.process(job));
    }

}
