package com.mediahub.core.models;

import static org.junit.jupiter.api.Assertions.*;

import java.security.Principal;
import java.util.Collections;
import java.util.Map;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.constants.BnpConstants;
import static org.mockito.Mockito.when;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;


@ExtendWith({ AemContextExtension.class })
class PrivacyPolicyTest {
	
	@InjectMocks
	PrivacyPolicy privacyPolicy;
	
	@Mock
	ResourceResolverFactory resolverFactory;
	
	@Mock
	SlingHttpServletRequest request;

	@Mock
	ResourceResolver resolver;
	
	@Mock
	Principal user;
	
	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }
	
	@Test
	void test() throws LoginException {
		when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
		when(request.getUserPrincipal()).thenReturn(user);
	}

}
