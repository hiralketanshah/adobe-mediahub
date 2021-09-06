package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.security.Principal;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;

import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.adobe.granite.asset.api.Asset;
import com.adobe.granite.asset.api.Rendition;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class UpdateInternalUsersTest {

	@InjectMocks
	UpdateInternalUsers updateInternalUsers;

	@Mock
	SlingHttpServletRequest req;

	@Mock
	SlingHttpServletResponse resp;

	@Mock
	ResourceResolverFactory resourceResolverFactory;

	@Mock
	ResourceResolver resourceResolver;

	@Mock
	Session session;

	@Mock
	Resource resource;

	@Mock
	Property property;

	@Mock
	PrintWriter printWriter;

	@Mock
	Node node;

	@Mock
	Asset asset;

	@Mock
	Rendition rendition;

	@Mock
	Query query;

	@Mock
	QueryBuilder builder;

	@Mock
	SearchResult result;

	@Mock
	Hit hit;

	@Mock
	UserManager userManager;

	@Mock
	User user;

	@Mock
	Value val;

	@Mock
	ValueFactory valFactory;

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testDoGet() throws Exception  {

		ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		InputStream is = classloader.getResourceAsStream("users-list.csv");

		Value[] propertyValue = new Value[] { val };

		when(resp.getWriter()).thenReturn(printWriter);
		when(req.getResourceResolver()).thenReturn(resourceResolver);
		when(resourceResolver.getResource(BnpConstants.CSV_FILE_PATH)).thenReturn(resource);
		when(resourceResolver.getResource(BnpConstants.CSV_USER_INFO)).thenReturn(null);
		when(resource.adaptTo(Asset.class)).thenReturn(asset);
		when(asset.getRendition(Mockito.anyString())).thenReturn(rendition);
		when(rendition.getStream()).thenReturn(is);
		when(resourceResolver.adaptTo(Session.class)).thenReturn(session);
		when(session.getValueFactory()).thenReturn(valFactory);
		when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
		when(userManager.getAuthorizable(Mockito.anyString())).thenReturn(null);
		when(userManager.createUser(Mockito.anyString(), Mockito.anyString(), Mockito.any(Principal.class),
				Mockito.anyString())).thenReturn(user);
		when(user.getProperty(Mockito.anyString())).thenReturn(propertyValue);
		when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
		when(query.getResult()).thenReturn(result);

		assertAll(() -> updateInternalUsers.doGet(req, resp));

	}

}
