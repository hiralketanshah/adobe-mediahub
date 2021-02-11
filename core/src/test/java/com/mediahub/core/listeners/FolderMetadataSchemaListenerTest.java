package com.mediahub.core.listeners;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
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
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.osgi.service.event.Event;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
@MockitoSettings(strictness = Strictness.LENIENT)
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

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() throws LoginException {
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
    }

    @Test
    void handleEvent() throws LoginException, PersistenceException {
        ArrayList<Resource> arrayList = new ArrayList<>();
        Iterator<Resource> iterator = arrayList.iterator();
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/"));
        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getChild(BnpConstants.TEMP)).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getName()).thenReturn(StringUtils.EMPTY);
        doNothing().when(resolver).commit();
        when(resolver.copy(any(),any())).thenReturn(resource);
        when(resolver.create(any(),any(),any())).thenReturn(resource);
        when(resource.listChildren()).thenReturn(iterator);

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
