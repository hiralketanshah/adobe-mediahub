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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.jcr.Session;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
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

	@InjectMocks
	private AnalyticsTrackingService trackingService = new AnalyticsTrackingServiceImpl();

	ResolveContext<Object> resolveContext;

	@BeforeEach
	void setup() {
		MockitoAnnotations.initMocks(this);
		context.registerInjectActivateService(trackingService);
		String[] status = { "external", "internal" };
		res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
		context.create().resource("/content/test1/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
		context.create().resource("/content/test1/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
				"nt:unstructured", "bnpp-broadcast-status", status);

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
	void internalResourceTest(AemContext context) {
		Resource intResource = fixture.getResource(resolveContext, "/content/internal/format/uui", null, null);
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
}
