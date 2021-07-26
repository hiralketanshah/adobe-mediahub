package com.mediahub.core.filters;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceMetadata;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;

import com.day.cq.commons.jcr.JcrConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(AemContextExtension.class)
class InternalResourceTest {

	private InternalResource fixture;

	private final AemContext context = new AemContext();

	Resource res;

	@BeforeEach
	void setup() {
		MockitoAnnotations.initMocks(this);

		res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
		context.create().resource("/content/test1/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
		context.create().resource("/content/test1/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
				"nt:unstructured", "bnpp-external-file-master-url", "scene7");
		fixture = new InternalResource(res, "/content/test");
	}

	@Test
	void resourceMetadataTest(AemContext context) {
		ResourceMetadata metadata = fixture.getResourceMetadata();
		assertEquals("/content/test1", metadata.getResolutionPath());
		assertEquals("dam:Asset", fixture.getResourceType());
		assertEquals(res.adaptTo(ValueMap.class), fixture.getValueMap());
		assertEquals("/content/test", fixture.getPath());
		assertEquals(res, fixture.getResource());
		assertEquals(res.getResourceSuperType(), fixture.getResourceSuperType());
		assertEquals(res.getResourceResolver(), fixture.getResourceResolver());
	}

}
