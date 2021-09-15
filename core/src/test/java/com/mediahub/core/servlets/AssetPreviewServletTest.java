package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.servlet.ServletOutputStream;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.Rendition;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class AssetPreviewServletTest {

    @InjectMocks
    AssetPreviewServlet assetPreviewServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    Resource resource;

    @Mock
    Property property;

    @Mock
    PrintWriter printWriter;

    @Mock
    Node node;
    
    @Mock
    Asset asset;
    
    @Mock
    Rendition rendition;
    
    @Mock
    ServletOutputStream outputStream;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testDoGet() throws Exception {
        testDoGetTwo();
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		InputStream is = classloader.getResourceAsStream("users-list.csv");
        when(property.getString()).thenReturn("image/jpeg");
        when(resp.getWriter()).thenReturn(printWriter);
        when(resource.adaptTo(Asset.class)).thenReturn(asset);
        when(asset.getOriginal()).thenReturn(rendition);
        when(rendition.getStream()).thenReturn(is);
        when(resp.getOutputStream()).thenReturn(outputStream);
        assetPreviewServlet.doGet(req, resp);
        assertNotNull(resp.getWriter());

    }

    @Test
    public void testDoGetOne() throws Exception {
        testDoGetTwo();
        when(property.getString()).thenReturn("video/mp4");
        when(resp.getWriter()).thenReturn(printWriter);

        assetPreviewServlet.doGet(req, resp);
        assertNotNull(resp.getWriter());

    }

    public void testDoGetTwo() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(req.getResource()).thenReturn(resource);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(node.getNode("jcr:content")).thenReturn(node);
        when(node.getProperty(BnpConstants.JCR_TITLE)).thenReturn(property);
        when(property.getString()).thenReturn("Page Title");
        when(req.getParameter("content")).thenReturn("abc");
        when(resourceResolver.getResource("abc")).thenReturn(resource);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(node.getNode("jcr:content")).thenReturn(node);
        when(node.getNode("metadata")).thenReturn(node);
        when(node.getProperty("dc:format")).thenReturn(property);
    }

}
