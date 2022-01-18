package com.mediahub.core.servlets;

import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestPathInfo;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class ProjectMemberRenderConditionServletTest {

    @InjectMocks
    ProjectMemberRenderConditionServlet projectMemberRenderConditionServlet;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;

    @Mock
    Resource resource;

    @Mock
    ValueMap valueMap;

    @Mock
    RequestPathInfo reqPathInfo;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException, IOException {
        MockitoAnnotations.initMocks(this);

        when(request.getRequestPathInfo()).thenReturn(reqPathInfo);
        when(reqPathInfo.getSuffix()).thenReturn("/content/dam/test");
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(request.getResource()).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resourceResolver.getUserID()).thenReturn("123");

    }

    @Test
    void doGet() throws ServletException, IOException, LoginException, RepositoryException {
        projectMemberRenderConditionServlet.doGet(request, response);
    }
}
