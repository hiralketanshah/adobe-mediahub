package com.mediahub.core.listeners;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;

import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
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
import org.osgi.service.event.Event;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({ AemContextExtension.class })
class ExternalLinksProjectListenerTest {

    private ExternalLinksProjectListener fixture = new ExternalLinksProjectListener();

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
    private Session session;

    @Mock
    private Node node;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() {
        MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
    }

    @Test
    void handleEvent() throws LoginException, RepositoryException {
        Event resourceEvent = new Event("event/topic", Collections.singletonMap(SlingConstants.PROPERTY_PATH,
                "/content/projects/test/jcr:content/dashboard/gadgets/externallinks"));
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(session.getNode(Mockito.anyString())).thenReturn(node);
        when(node.addNode(Mockito.anyString(), Mockito.anyString())).thenReturn(node);

        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        Resource listRes = context.create().resource("/etc/acs-commons/lists/external-links/jcr:content/list/link-one",
                JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured", JcrConstants.JCR_TITLE, "link-one", "value",
                "https://wwwgoogle.com");

        List<Resource> listOfResources = new ArrayList<>();
        listOfResources.add(listRes);
        Iterator<Resource> resources = listOfResources.iterator();

        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);
        when(resource.getChild(JcrConstants.JCR_CONTENT + "/list")).thenReturn(resource);
        when(resource.listChildren()).thenReturn(resources);
        fixture.handleEvent(resourceEvent);

        assertAll(() -> assertEquals("event/topic", resourceEvent.getTopic()),
                () -> assertEquals("/content/projects/test/jcr:content/dashboard/gadgets/externallinks",
                        resourceEvent.getProperty(SlingConstants.PROPERTY_PATH).toString()));
    }
}
