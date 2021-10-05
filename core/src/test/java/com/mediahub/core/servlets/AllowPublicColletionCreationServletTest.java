package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jcr.Session;

import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
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

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class AllowPublicColletionCreationServletTest {

	@InjectMocks
	AllowPublicCollectionCreationServlet allowPublicCollectionCreationServlet;

	@Mock
	SlingHttpServletRequest req;

	@Mock
	SlingHttpServletResponse resp;

	@Mock
	ResourceResolverFactory resourceResolverFactory;

	@Mock
	ResourceResolver resourceResolver;

	@Mock
	UserManager userManager;

	@Mock
	Session session;

	@Mock
	PrintWriter printWriter;

	@Mock
	User user;

	@Mock
	Group group;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testDoGet() throws Exception {
		List<Group> groups = new ArrayList<>();
		groups.add(group);
		Iterator<Group> iterator = groups.iterator();

		Mockito.when(req.getResourceResolver()).thenReturn(resourceResolver);
		Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
		Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);
		when(session.getUserID()).thenReturn("admin");
		when(userManager.getAuthorizable("admin")).thenReturn(user);
		when(user.memberOf()).thenReturn(iterator);
		when(group.getID()).thenReturn("administrators");

		when(resp.getWriter()).thenReturn(printWriter);
		assertAll(() -> allowPublicCollectionCreationServlet.doGet(req, resp));
	}
}
