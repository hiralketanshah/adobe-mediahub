package com.mediahub.core.models;

import java.security.Principal;
import java.util.Collections;
import java.util.Map;

import javax.jcr.RepositoryException;
import javax.jcr.Value;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.constants.BnpConstants;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
	UserManager userManager;

	@Mock
	Principal user;

	@Mock
	Authorizable authorizable;

	@Mock
	Value val;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
	public void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);
		when(request.getUserPrincipal()).thenReturn(user);
		when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
		when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
		when(userManager.getAuthorizable(Mockito.any(Principal.class))).thenReturn(authorizable);
	}

	@Test
	void testWithLanguage() throws LoginException, RepositoryException {
		Value[] value = { val };
		when(authorizable.getProperty(Mockito.anyString())).thenReturn(value);
		when(request.getUserPrincipal()).thenReturn(user);
		when(authorizable.getPath()).thenReturn("testPath");
		when(val.getString()).thenReturn("testString");
		privacyPolicy.init();
		assertEquals(privacyPolicy.getCurrentUserPath(), "testPath");
		assertEquals(privacyPolicy.getExportedType(), "mediahub/components/privacypolicy");

	}

	@Test
	void testWithoutLanguage() throws LoginException, RepositoryException {
		when(authorizable.getProperty("./preferences/language")).thenReturn(null);
		when(request.getUserPrincipal()).thenReturn(user);
		privacyPolicy.init();
		assertEquals(privacyPolicy.getLanguage(), "en");
	}

}
