package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import com.adobe.granite.workflow.WorkflowException;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.QueryUtils;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Session;
import javax.servlet.ServletException;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class BnpInternalUsersTest {

	@InjectMocks
	BnpInternalUsers bnpInternalUsers;

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

	@Mock
	Query query;

	@Mock
	QueryBuilder queryBuilder;

	@Mock
	SearchResult result;
	
	@Mock
	Resource res;
	
	@Mock
	UserManager um;
	
	@Mock
	Authorizable authorizable;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);
	List<Authorizable> listOfAuths = new ArrayList<>();

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
		listOfAuths.add(authorizable);
	}

	@Test
	public void testDoGet() throws Exception {
		List<Resource> resources = new ArrayList<>();
		resources.add(res);
		Iterator<Resource> iterator = resources.iterator();

		Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
		Mockito.when(res.adaptTo(User.class)).thenReturn(user);
		Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);
		Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(um);
		Mockito.when(um.findAuthorizables(Mockito.any())).thenReturn(listOfAuths.iterator());
		/*when(session.getUserID()).thenReturn("admin");
		when(userManager.getAuthorizable("admin")).thenReturn(user);
		when(user.memberOf()).thenReturn(iterator);
		when(group.getID()).thenReturn("administrators");*/

		when(resourceResolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
		when(queryBuilder.createQuery(PredicateGroup.create(QueryUtils.getPredicateMapInternalUsers("/home/users")), resourceResolver.adaptTo(Session.class))).thenReturn(query);
		when(query.getResult()).thenReturn(result);
		when(result.getResources()).thenReturn(iterator);

		when(resp.getWriter()).thenReturn(printWriter);
		bnpInternalUsers.doGet(req, resp);
	}
	
	@Test
	public void testDoGetError() throws LoginException, ServletException, IOException {
	    Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(LoginException.class);
	    bnpInternalUsers.doGet(req, resp);
	}

}
