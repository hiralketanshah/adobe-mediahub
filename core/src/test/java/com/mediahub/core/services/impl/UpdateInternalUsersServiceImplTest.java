package com.mediahub.core.services.impl;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.Principal;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.management.NotCompliantMBeanException;

import org.apache.jackrabbit.api.security.user.AuthorizableExistsException;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.PersistenceException;
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

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class UpdateInternalUsersServiceImplTest {

	@InjectMocks
	UpdateInternalUsersServiceImpl fetchPrice;

	@Mock
	ResourceResolverFactory resolverFactory;

	@Mock
	ResourceResolver resolver;

	@Mock
	UserManager userManager;

	@Mock
	Session session;

	@Mock
	Asset resource;

	@Mock
	User user;

	@Mock
	Value val;

	@Mock
	ValueFactory valFactory;

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
	Group group;

	@Mock
	Node node;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);
	Resource csvResource;

	@BeforeEach
	public void setup() throws NotCompliantMBeanException, org.apache.sling.api.resource.LoginException {
		MockitoAnnotations.initMocks(this);
		when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

	}

	@Test
	public void testRun() throws AuthorizableExistsException, RepositoryException, PersistenceException {
		Value[] propertyValue = new Value[] { val };
		ClassLoader classloader = Thread.currentThread().getContextClassLoader();
		InputStream is = classloader.getResourceAsStream("users-list.csv");
		BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(is));
		when(resolver.adaptTo(Session.class)).thenReturn(session);
		when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
		when(resolver.getResource(BnpConstants.CSV_FILE_PATH)).thenReturn(resource);
		when(resolver.getResource(BnpConstants.CSV_USER_INFO)).thenReturn(null);
		when(resolver.getResource(BnpConstants.CSV_USER_STATUS)).thenReturn(null);
		when(session.getValueFactory()).thenReturn(valFactory);
		when(userManager.createUser(Mockito.anyString(), Mockito.anyString(), Mockito.any(Principal.class),
				Mockito.anyString())).thenReturn(user);
		when(session.getNode(Mockito.anyString())).thenReturn(node);
		when(userManager.getAuthorizable("mediahub-basic")).thenReturn(group);
		when(user.getProperty(Mockito.anyString())).thenReturn(propertyValue);
		when(resource.adaptTo(Asset.class)).thenReturn(asset);
		when(asset.getRendition(Mockito.anyString())).thenReturn(rendition);
		when(rendition.getStream()).thenReturn(is);

		when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
		when(query.getResult()).thenReturn(result);
		Map<String, com.mediahub.core.data.User> inputMap = fetchPrice.convertStreamToHashMap(brUserInfo, true);
		assertAll(() -> fetchPrice.createAndUpdateUsers(BnpConstants.CSV_FILE_PATH, BnpConstants.CSV_USER_INFO,
				BnpConstants.CSV_USER_STATUS));
		assertAll(() -> fetchPrice.createAndSaveUsers(inputMap, new LinkedHashMap<>(), new LinkedHashMap<>(),
				userManager, session));
		assertAll(() -> fetchPrice.deletedUnwantedUsers(resolver, inputMap));
		assertAll(() -> fetchPrice.removeAllUsers());
	}

}
