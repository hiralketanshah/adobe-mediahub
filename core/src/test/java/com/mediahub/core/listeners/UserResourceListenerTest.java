package com.mediahub.core.listeners;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.RepositoryException;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.oak.spi.security.user.UserConstants;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.JobManager;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.event.Event;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class})
class UserResourceListenerTest {

    public static final String HOME_USERS_PATH = "/home/users/0/0UBUzwrctPQmlkePWjBh";
    private UserResourceListener fixture = new UserResourceListener();

    @InjectMocks
    private TestLogger logger = TestLoggerFactory.getTestLogger(fixture.getClass());

    private AemContext context;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private Resource resource;

    @Mock
    JobManager jobManager;

    @Mock
    Job job;
    
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    Resource profileResource;

    @BeforeEach
    void setup() throws LoginException {
		MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        context.registerService(JobManager.class, jobManager);
        profileResource = context.create().resource(HOME_USERS_PATH + "/profile", JcrConstants.JCR_PRIMARYTYPE, JcrConstants.NT_UNSTRUCTURED);
    }

    @Test
    void handleEvent() throws LoginException {
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(SlingConstants.PROPERTY_PATH, HOME_USERS_PATH);
        eventMap.put("resourceType", UserConstants.NT_REP_USER);
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, eventMap);
        fixture.resourceResolverFactory = resourceResolverFactory;
        fixture.jobManager = jobManager;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(HOME_USERS_PATH)).thenReturn(resource);
        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(profileResource);
        final Map<String, Object> properties = new HashMap<>();
        properties.put(BnpConstants.FIRST_NAME,"Abu");
        properties.put(BnpConstants.EMAIL,"abibrahi@adobe.com");
        when(jobManager.addJob("user/welcome/email", properties)).thenReturn(job);
        fixture.handleEvent(resourceEvent);

        assertAll(
                () -> assertEquals(BnpConstants.EVENT_TOPIC, resourceEvent.getTopic()),
                () -> assertEquals(HOME_USERS_PATH, resourceEvent.getProperty(SlingConstants.PROPERTY_PATH).toString())
        );
    }

    @Test
    void handleEventWithLoginException() throws LoginException {
        Map<String, Object> eventMap = new HashMap<>();
        eventMap.put(SlingConstants.PROPERTY_PATH, HOME_USERS_PATH);
        eventMap.put("resourceType", UserConstants.NT_REP_USER);
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, eventMap);
        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        fixture.handleEvent(resourceEvent);
        Assertions.assertThrows(LoginException.class, () ->{
            fixture.resourceResolverFactory.getServiceResourceResolver(authInfo);
        });
    }

}
