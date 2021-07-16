package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
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
import org.apache.sling.api.resource.LoginException;
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
public class UserImportServletTest {

	@InjectMocks
	private UserImportServlet fixture;

	public AemContext context = new AemContext();

	@Mock
	User user;

	@Mock
	SlingHttpServletRequest request;

	@Mock
	ResourceResolver resourceResolver;

	@Mock
	UserCreationService userCreationService;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
	public void setupMock() {
		MockitoAnnotations.initMocks(this);
		context.registerService(UserCreationService.class, userCreationService);
	}

	@Test
	void doGet(AemContext context) throws RepositoryException {

		MockSlingHttpServletResponse response = context.response();
		final List<Group> groups = new ArrayList<>();
		Group userGroup = Mockito.mock(Group.class);
		groups.add(userGroup);
		when(request.getResourceResolver()).thenReturn(resourceResolver);
		when(resourceResolver.adaptTo(User.class)).thenReturn(user);
		when(user.memberOf()).thenReturn(groups.iterator());
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR);
		assertAll(() -> fixture.doGet(request, response));
	}
}
