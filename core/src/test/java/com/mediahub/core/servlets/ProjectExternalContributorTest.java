package com.mediahub.core.servlets;

import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestPathInfo;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class ProjectExternalContributorTest {

    @InjectMocks
    ProjectExternalContributor projectExternalContributor;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;
    
    @Mock
    RequestPathInfo reqPathInfo;

  
    InputStream stream;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException, IOException {
        MockitoAnnotations.initMocks(this);
        
        when(request.getRequestPathInfo()).thenReturn(reqPathInfo);
        when(reqPathInfo.getSuffix()).thenReturn("/content/dam/test");
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        stream = classloader.getResourceAsStream("media-updater.csv");
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        
    }

    @Test
    void doGet() throws ServletException, IOException, LoginException, RepositoryException {
        projectExternalContributor.doGet(request, response);
    }
}
