package com.mediahub.core.filters;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.spi.resource.provider.ResolveContext;
import org.apache.sling.spi.resource.provider.ResourceProvider;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.jcr.Session;
import javax.servlet.ServletException;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.AnalyticsTrackingService;
import com.mediahub.core.services.impl.AnalyticsTrackingServiceImpl;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith({ AemContextExtension.class })
class AssetTrackingProviderTest {

    @InjectMocks
    private AssetTrackingProvider fixture;

    private final AemContext context = new AemContext();

    Resource res;

    @Mock
    ResourceResolver resolver;

    @Mock
    QueryBuilder builder;

    @Mock
    Query query;

    @Mock
    Session session;

    @Mock
    SearchResult result;

    @Mock
    Asset asset;
    
    @Mock
    InternalResource resource;

    @InjectMocks
    private AnalyticsTrackingService trackingService = new AnalyticsTrackingServiceImpl();

    ResolveContext<Object> resolveContext;

    @BeforeEach
    void setup() throws IOException, ServletException {
        MockitoAnnotations.initMocks(this);
        context.registerInjectActivateService(trackingService);
        String[] status = { "external", "internal" };
        context.create().resource("/content/abc", JcrConstants.JCR_PRIMARYTYPE, BnpConstants.DAM_SLING_FOLDER);
        res = context.create().resource("/content/abc/test1.mpv", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
        context.create().resource("/content/abc/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource("/content/abc/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured",
                BnpConstants.BNPP_MEDIA_TYPE, status, BnpConstants.BNPP_MEDIA_CATEGORY, "1", BnpConstants.BNPP_LANGUAGE,
                "en", BnpConstants.BNPP_MEDIA_THEME, "dark", BnpConstants.BNPP_MEDIA_ENTITIES, "en",
                BnpConstants.BNPP_MEDIA_GEOGRAPHICAL, "fr", BnpConstants.BNPP_MEDIA_COUNTRY, "test",
                BnpConstants.BNPP_MEDIA_SPONSOR, "abc");
        context.create().resource("/content/abc/test1.mpv/jcr:content", JcrConstants.JCR_PRIMARYTYPE,
                "nt:unstructured");
        context.create().resource("/content/abc/test1.mpv/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
                "nt:unstructured", "bnpp-broadcast-status", status, BnpConstants.BNPP_CONFIDENTIALITY, "1",
                BnpConstants.BNPP_LANGUAGE, "en", BnpConstants.DAM_FILE_FORMAT, "video/m4v",
                BnpConstants.BNPP_SUBTITLES, "en", BnpConstants.BNPP_SUBTITLE_LANGUAGES, "fr",
                BnpConstants.DAM_FILE_TITLE, "test-file");

        List<Resource> listOfResources = new ArrayList<>();
        listOfResources.add(res);
        Iterator<Resource> resources = listOfResources.iterator();

        resolveContext = new ResolveContext<Object>() {

            @Override
            public ResourceResolver getResourceResolver() {
                return resolver;
            }

            @Override
            public Object getProviderState() {
                return null;
            }

            @Override
            public ResourceProvider<?> getParentResourceProvider() {
                return null;
            }

            @Override
            public ResolveContext<?> getParentResolveContext() {
                return null;
            }
        };

        when(resolver.adaptTo(QueryBuilder.class)).thenReturn(builder);
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(builder.createQuery(Mockito.any(PredicateGroup.class), Mockito.any(Session.class))).thenReturn(query);
        when(query.getResult()).thenReturn(result);
        when(result.getResources()).thenReturn(resources);

        context.registerInjectActivateService(fixture);
    }
    
    @Test
    void listChildrenTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/player/uui", null, null);
        fixture.listChildren(resolveContext, intResource);
        when(resource.getResource()).thenReturn(resource);
        when(resource.getResourceResolver()).thenReturn(resolver);
        fixture.listChildren(resolveContext, resource);
        
    }

    @Test
    void internalResourcePlayerTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/player/uui", null, null);
        assertEquals("mediahub/assets/tracking", intResource.getResourceType());

    }

    @Test
    void internalResourceMasterTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/master/uui", null, null);
        assertEquals("dam:Asset", intResource.getResourceType());

    }

    @Test
    void internalResourceHDTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/hd/uui", null, null);
        assertEquals("mediahub/assets/tracking", intResource.getResourceType());

    }

    @Test
    void internalResourceMDTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/md/uui", null, null);
        assertEquals("mediahub/assets/tracking", intResource.getResourceType());

    }

    @Test
    void internalResourceSuperHDTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/superhd/uui", null, null);
        assertEquals("mediahub/assets/tracking", intResource.getResourceType());

    }

    @Test
    void internalResourceTest(AemContext context) {
        Resource intResource = fixture.getResource(resolveContext, "/content/internal/test/uui", null, null);
        assertEquals("mediahub/assets/tracking", intResource.getResourceType());

    }

    @Test
    void externalResourcePlayerTest(AemContext context) {
        Resource extResourcePlayer = fixture.getResource(resolveContext, "/content/external/player/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResourcePlayer.getResourceType());
    }

    @Test
    void externalResourceMasterTest(AemContext context) {
        Resource extResource = fixture.getResource(resolveContext, "/content/external/master/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResource.getResourceType());
    }

    @Test
    void externalResourceHdTest(AemContext context) {
        Resource extResource = fixture.getResource(resolveContext, "/content/external/hd/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResource.getResourceType());
    }

    @Test
    void externalResourceMdTest(AemContext context) {
        Resource extResource = fixture.getResource(resolveContext, "/content/external/md/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResource.getResourceType());
    }

    @Test
    void externalResourceSuperHdTest(AemContext context) {
        Resource extResource = fixture.getResource(resolveContext, "/content/external/superhd/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResource.getResourceType());
    }

    @Test
    void externalResourceTest(AemContext context) {
        Resource extResource = fixture.getResource(resolveContext, "/content/external/test/uui", null, null);
        assertEquals("mediahub/assets/tracking", extResource.getResourceType());
    }
}
