package com.mediahub.core.servlets;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.JobManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class MediaBulkUnpublishServletTest {

    @InjectMocks
    MediaBulkUnpublishServlet mediaBulkUnpublishServlet;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    JobManager jobManager;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;

    @Mock
    Resource resource;

    Map<String, String[]> parameterMap = new HashMap<>();

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException {
        MockitoAnnotations.initMocks(this);
        String[] pathStrings = new String[] { "/content%2cdam/abc.jpg" };
        parameterMap.put("path", pathStrings);

        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.hasChildren()).thenReturn(true);
        List<Resource> resourceList = new ArrayList<>();
        resourceList.add(resource);
        when(resource.listChildren()).thenReturn(resourceList.iterator());
    }

    @Test
    void doGet() throws ServletException, IOException, LoginException, RepositoryException {

        when(request.getParameterMap()).thenReturn(parameterMap);
        mediaBulkUnpublishServlet.doGet(request, response);
    }

    @Test
    void doGetOne() throws ServletException, IOException, LoginException, RepositoryException {

        mediaBulkUnpublishServlet.doGet(request, response);
    }
}
