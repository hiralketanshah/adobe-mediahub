package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.day.cq.tagging.Tag;
import com.day.cq.tagging.TagManager;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Session;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.JobManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class TagsCreatorServletTest {

    @InjectMocks
    TagsCreatorServlet tagsCreatorServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    Tag tag;

    @Mock
    Resource resource;

    @Mock
    RequestParameter assetRequestParameter;

    @Mock
    RequestParameter mediaRequestParameter;

    @Mock
    RequestParameter entitiesRequestParameter;

    @Mock
    TagManager tagManager;

    InputStream assetFile;
    InputStream mediaFile;
    InputStream entitiesFile;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws IOException {
        MockitoAnnotations.initMocks(this);

        when(req.getRequestParameter("assets-file")).thenReturn(assetRequestParameter);
        when(req.getRequestParameter("medias-file")).thenReturn(mediaRequestParameter);
        when(req.getRequestParameter("entities-file")).thenReturn(entitiesRequestParameter);
    }

    @Test
    public void testDoPost() throws Exception {
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        assetFile = classloader.getResourceAsStream("media-updater.csv");
        mediaFile = classloader.getResourceAsStream("media-updater.csv");
        entitiesFile = classloader.getResourceAsStream("media-updater.csv");
        when(assetRequestParameter.getInputStream()).thenReturn(assetFile);
        when(mediaRequestParameter.getInputStream()).thenReturn(mediaFile);
        when(entitiesRequestParameter.getInputStream()).thenReturn(entitiesFile);
        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        Mockito.when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);

        when(req.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);

        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);

        when(tagManager.resolve("mediahub:")).thenReturn(tag);
        assertAll(() -> tagsCreatorServlet.doPost(req, resp));
    }

}
