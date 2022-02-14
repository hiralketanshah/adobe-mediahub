package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.commons.Externalizer;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.Session;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith({ AemContextExtension.class })
class ExistinguserProjectAccessEmailTest {

	private final AemContext context = new AemContext();

	@InjectMocks
	private ExistingUserProjectAccessEmail existingUserProjectAccessEmail;

	@Mock
	Job job;

	@Mock
	GenericEmailNotification genericEmailNotification;

	@Mock
	ResourceResolverFactory resolverFactory;

	@Mock
	ResourceResolver resolver;

	@Mock
	Resource resource;

	@Mock
	ValueMap valueMap;

	@Mock
	QueryBuilder builder;

	@Mock
	JackrabbitSession session;
	
	@Mock
	UserManager userManager;

	@Mock
	Query query;

	@Mock
	SearchResult res;
	
	@Mock
	Hit hit;

	@Mock
	Node node;

	@Mock
	Property property;

	@Mock
	I18nProvider provider;

	@Mock
	SlingSettingsService slingSettingsService;
	
	@Mock
	Externalizer externalizer;
	
	@Mock
	User user;

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
	void setUp() throws Exception {
		MockitoAnnotations.initMocks(this);

		context.registerService(GenericEmailNotification.class, genericEmailNotification);
		context.registerService(ResourceResolverFactory.class, resolverFactory);

		when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
		when(resolver.adaptTo(QueryBuilder.class)).thenReturn(builder);
		when(resolver.adaptTo(Session.class)).thenReturn(session);
		
		when(session.getUserManager()).thenReturn(userManager);
		when(userManager.getAuthorizable(Mockito.anyString())).thenReturn(user);
		when(user.getProperty(Mockito.anyString())).thenReturn(null);
		when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
		when(query.getResult()).thenReturn(res);
		when(res.getQueryStatement()).thenReturn("Statement");
		
		List<Hit> listOfHits = new ArrayList<>();
		listOfHits.add(hit);
		
		when(res.getHits()).thenReturn(listOfHits);
		when(hit.getResource()).thenReturn(resource);
		when(hit.getPath()).thenReturn("TestPath");

		when(session.getNodeByIdentifier(Mockito.anyString())).thenReturn(node);
		when(node.getNode(Mockito.anyString())).thenReturn(node);
		when(node.getPath()).thenReturn("testpath");
		when(node.getProperty(Mockito.anyString())).thenReturn(property);

		when(job.getProperty(Mockito.anyString(), Mockito.any())).thenReturn("testPath");
		when(job.getProperty("userID", StringUtils.EMPTY)).thenReturn("testID");
		when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
		when(resource.getName()).thenReturn("rep:members");
		when(resource.getParent()).thenReturn(resource);
		when(resource.getValueMap()).thenReturn(valueMap);
		when(resource.getChild(Mockito.anyString())).thenReturn(resource);
		when(valueMap.get("alternateTitle", StringUtils.EMPTY)).thenReturn("Observers");
		when(valueMap.containsKey("rep:principalName")).thenReturn(true);
		when(valueMap.get(BnpConstants.EMAIL, StringUtils.EMPTY)).thenReturn("test@adobe.com");
		when(valueMap.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY)).thenReturn("test");
		when(valueMap.get(BnpConstants.EXPIRY, StringUtils.EMPTY)).thenReturn("test");

	}

	@Test
	void process() {
		ArrayList<Object> listOfAfterValue = new ArrayList<>();
		listOfAfterValue.add("1234");
		when(job.getProperty(BnpConstants.BEFORE_VALUE)).thenReturn("test");
		when(job.getProperty(BnpConstants.BEFORE_VALUE, new ArrayList<>())).thenReturn(new ArrayList());
		when(job.getProperty(BnpConstants.AFTER_VALUE)).thenReturn(listOfAfterValue);
		when(job.getProperty(BnpConstants.AFTER_VALUE, new ArrayList<>())).thenReturn(listOfAfterValue);
		List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(resource.listChildren()).thenReturn(userList.iterator());
		assertEquals(JobConsumer.JobResult.OK, existingUserProjectAccessEmail.process(job));
	}

}
