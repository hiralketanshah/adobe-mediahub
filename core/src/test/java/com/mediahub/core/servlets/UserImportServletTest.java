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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.jcr.RepositoryException;

import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
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
	MockSlingHttpServletResponse response;
	Group userGroup;

	@BeforeEach
	public void setupMock() throws RepositoryException {
		MockitoAnnotations.initMocks(this);
		context.registerService(UserCreationService.class, userCreationService);

		response = context.response();
		final List<Group> groups = new ArrayList<>();
		userGroup = Mockito.mock(Group.class);
		groups.add(userGroup);
		when(request.getResourceResolver()).thenReturn(resourceResolver);
		when(resourceResolver.adaptTo(User.class)).thenReturn(user);
		when(user.memberOf()).thenReturn(groups.iterator());
	}

	@Test
	void doGet(AemContext context) throws RepositoryException {
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR);
		assertAll(() -> fixture.doGet(request, response));
	}

	@Test
	void doGetMediahubAdmin(AemContext context) throws RepositoryException {
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_ADMINISTRATOR);
		assertAll(() -> fixture.doGet(request, response));
	}

	@Test
	void doGetMediahubReader(AemContext context) throws RepositoryException {
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_BASIC_READER);
		assertAll(() -> fixture.doGet(request, response));
	}

	@Test
	void doGetMediahubEntityManager(AemContext context) throws RepositoryException {
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER);
		assertAll(() -> fixture.doGet(request, response));
	}

	@Test
	void doGetMediahubReaderLib(AemContext context) throws RepositoryException {
		when(userGroup.getID()).thenReturn(BnpConstants.MEDIAHUB_READER_MEDIALIBRARY);
		assertAll(() -> fixture.doGet(request, response));
	}
}
