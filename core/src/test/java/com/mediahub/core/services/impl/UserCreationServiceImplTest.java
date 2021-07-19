package com.mediahub.core.services.impl;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import javax.jcr.AccessDeniedException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.ValueFactory;

import static org.mockito.Mockito.when;

import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.testing.mock.sling.ResourceResolverType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class UserCreationServiceImplTest {

	private final AemContext context = new AemContext(ResourceResolverType.JCR_MOCK);

	@InjectMocks
	UserCreationServiceImpl userCreationServiceImpl;

	@Mock
	ResourceResolverFactory resolverFactory;

	@Mock
	ResourceResolver resolver;

	@Mock
	JackrabbitSession session;

	@Mock
	UserManager userManager;

	@Mock
	ValueFactory valueFactory;

	@Mock
	User user;
	
	@Mock
	Group group;
	
	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

	@BeforeEach
	public void setupMock()
			throws IOException, AccessDeniedException, UnsupportedRepositoryOperationException, RepositoryException {
		MockitoAnnotations.initMocks(this);
		Mockito.when(resolver.adaptTo(Session.class)).thenReturn(session);
		Mockito.when(session.getUserManager()).thenReturn(userManager);
		Mockito.when(session.getValueFactory()).thenReturn(valueFactory);
		Mockito.when(userManager.createUser(Mockito.any(String.class), Mockito.any(String.class))).thenReturn(user);
		Mockito.when(userManager.getAuthorizable(BnpConstants.BASIC_GROUP)).thenReturn(group);
	}
	
	@Test
	public void testReadCsv() throws Exception {
		Resource res = context.create().resource("/content/dam/test.jpg", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
        context.create().resource("/content/dam/test.jpg" + "/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource("/content/dam/test.jpg" + "/jcr:content/renditions", JcrConstants.JCR_PRIMARYTYPE, "nt:folder");
        context.create().resource("/content/dam/test.jpg" + "/jcr:content/renditions/original", JcrConstants.JCR_PRIMARYTYPE, "nt:file");
        context.create().resource("/content/dam/test.jpg" + "/jcr:content/renditions/original/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured","jcr:data","This is original rendition\n 1234, test@gmail.com,test first name, test last name","jcr:mimeType","image/png");

        context.create().resource("/content/dam/test.jpg" + "/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(Mockito.anyString())).thenReturn(res);

		userCreationServiceImpl.readCsv("/content/dam/test.jpg");

	}

}
