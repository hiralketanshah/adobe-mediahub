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
import java.util.Iterator;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;

import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.osgi.service.event.Event;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class})
class FolderMetadataSchemaListenerTest {

    private FolderMetadataSchemaListener fixture = new FolderMetadataSchemaListener();

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
    private Node node;
    
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    Resource appRes;
    Resource tabRes;

    @BeforeEach
    void setup() throws LoginException {
		MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        appRes = context.create().resource(BnpConstants.APPS_DAM, JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource(BnpConstants.APPS_DAM+"/temp", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        
        tabRes = context.create().resource(BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource(BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/items", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
        context.create().resource(BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/items/schematabs", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
    }

    @Test
    void handleEvent() throws LoginException, PersistenceException, RepositoryException {
        ArrayList<Resource> arrayList = new ArrayList<>();
        Iterator<Resource> iterator = arrayList.iterator();
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/"));
        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resolver.getResource(BnpConstants.APPS_DAM)).thenReturn(appRes);
        when(resolver.getResource(BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs")).thenReturn(tabRes);
        when(resolver.getResource(BnpConstants.FOLDER_WIZARD_PATH)).thenReturn(appRes);

        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getChild(BnpConstants.TEMP)).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getName()).thenReturn(StringUtils.EMPTY);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        doNothing().when(resolver).commit();
        when(resolver.copy(any(),any())).thenReturn(resource);
        when(resolver.create(any(),any(),any())).thenReturn(resource);
        when(resource.listChildren()).thenReturn(iterator);
        when(node.getParent()).thenReturn(node);
        doNothing().when(node).orderBefore(any(String.class), any(String.class));

        fixture.handleEvent(resourceEvent);

        assertAll(
                () -> assertEquals(BnpConstants.EVENT_TOPIC, resourceEvent.getTopic()),
                () -> assertEquals(BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/", resourceEvent.getProperty(SlingConstants.PROPERTY_PATH).toString())
        );
    }

    @Test
    void handleEventWithLoginException() throws LoginException {
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub/corporate_institutionalbankingcib/jcr:content"));
        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        fixture.handleEvent(resourceEvent);
        Assertions.assertThrows(LoginException.class, () ->{
            fixture.resourceResolverFactory.getServiceResourceResolver(authInfo);
        });
    }

}
