package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.Session;

import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
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
	Session session;

	@Mock
	Query query;

	@Mock
	SearchResult res;

	@Mock
	Node node;

	@Mock
	Property property;

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
		when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
		when(query.getResult()).thenReturn(res);
		when(res.getQueryStatement()).thenReturn("Statement");

		when(session.getNodeByIdentifier(Mockito.anyString())).thenReturn(node);
		when(node.getNode(Mockito.anyString())).thenReturn(node);
		when(node.getPath()).thenReturn("testpath");
		when(node.getProperty(Mockito.anyString())).thenReturn(property);

		when(job.getProperty(BnpConstants.PATH, String.class)).thenReturn("testPath");
		when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
		when(resource.getName()).thenReturn("rep:members");
		when(resource.getParent()).thenReturn(resource);
		when(resource.getValueMap()).thenReturn(valueMap);
		when(valueMap.containsKey("rep:principalName")).thenReturn(true);
		when(valueMap.get(BnpConstants.EMAIL, StringUtils.EMPTY)).thenReturn("test@adobe.com");
		when(valueMap.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY)).thenReturn("test");
		when(valueMap.get(BnpConstants.EXPIRY, StringUtils.EMPTY)).thenReturn("test");

	}

	@Test
	void process() {
		ArrayList<Object> listOfAfterValue = new ArrayList<>();
		listOfAfterValue.add("1234");
		when(job.getProperty(BnpConstants.AFTER_VALUE)).thenReturn(listOfAfterValue);
		when(job.getProperty(BnpConstants.AFTER_VALUE, new ArrayList<>())).thenReturn(listOfAfterValue);
		assertEquals(JobConsumer.JobResult.OK, existingUserProjectAccessEmail.process(job));
	}

}
