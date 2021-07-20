package com.mediahub.core.services.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.services.Scene7DeactivationService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class Scene7DeactivationServiceImplTest {

	private final AemContext context = new AemContext();

	Scene7DeactivationService scene7DeactivationService;

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
		scene7DeactivationService = context.registerInjectActivateService(new Scene7DeactivationServiceImpl());
	}

	@Test
	public void testProperties() throws Exception {
		assertEquals("/content/mediahub/us/en.s7cdninvalidation.json",
				scene7DeactivationService.getCdnCacheInvalidationPath());
		assertEquals("/conf/global/settings/cloudconfigs/dmscene7",
				scene7DeactivationService.getCloudConfigurationPath());
	}
}
