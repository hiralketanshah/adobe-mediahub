package com.mediahub.core.filters;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import javax.servlet.FilterChain;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith({ AemContextExtension.class })
class AssetTrackingProviderFilterTest {

	private AssetTrackingProviderFilter fixture = new AssetTrackingProviderFilter();

	@Mock
	SlingHttpServletRequest slingRequest;

	@Mock
	SlingHttpServletResponse slingResponse;

	@BeforeEach
	void setup() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	void doFilter(AemContext context) {
		assertAll(() -> fixture.doFilter(slingRequest, slingResponse, mock(FilterChain.class)));
	}
}
