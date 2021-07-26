package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.UserCreationService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.jcr.RepositoryException;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.servlet.ServletException;

import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class TrackingRedirectServletTest {

	@InjectMocks
	private TrackingRedirectServlet fixture;

	public AemContext context = new AemContext();

	@Mock
	SlingHttpServletRequest request;

	@Mock
	ResourceResolver resourceResolver;

	Resource res;
	Resource resWithoutRedirect;
	MockSlingHttpServletResponse response;

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
		res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset", "redirectTarget",
				"testTarget");
		resWithoutRedirect = context.create().resource("/content/test2", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
		response = context.response();
	}

	@Test
	void doGetWithRedirect(AemContext context) throws IOException {
		when(request.getResource()).thenReturn(res);
		fixture.doGet(request, response);
		assertEquals(302, response.getStatus());
	}

	@Test
	void doGetWithoutRedirect(AemContext context) throws IOException {
		when(request.getResource()).thenReturn(resWithoutRedirect);
		fixture.doGet(request, response);
		assertEquals(200, response.getStatus());
	}
}
