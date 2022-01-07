package com.mediahub.core.servlets;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class MetadataUdpaterServletTest {

    @InjectMocks
    MetadataUpdaterServlet metadataUpdaterServlet;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;

    @Mock
    Resource resource;

    @Mock
    ModifiableValueMap valueMap;
    
    @Mock
    RequestParameter reqParam;
    
    @Mock
    Node node;

    InputStream stream;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException, IOException {
        MockitoAnnotations.initMocks(this);
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        stream = classloader.getResourceAsStream("media-updater.csv");
        when(request.getRequestParameter(Mockito.anyString())).thenReturn(reqParam);
        when(reqParam.getInputStream()).thenReturn(stream);

        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(valueMap);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.hasChildren()).thenReturn(true);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.anyString())).thenReturn(true);
    }

    @Test
    void doGet() throws ServletException, IOException, LoginException, RepositoryException {
        metadataUpdaterServlet.doPost(request, response);
    }
}
