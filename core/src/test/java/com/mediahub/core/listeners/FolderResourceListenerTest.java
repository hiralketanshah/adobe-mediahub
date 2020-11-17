/*
 *  Copyright 2018 Adobe Systems Incorporated
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.mediahub.core.listeners;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.osgi.service.event.Event;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
class FolderResourceListenerTest {

    private FolderResourceListener fixture = new FolderResourceListener();

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
    void handleEvent() throws LoginException {
        Event resourceEvent = new Event("event/topic", Collections.singletonMap(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub/corporate_institutionalbankingcib/jcr:content"));

        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);
        fixture.handleEvent(resourceEvent);

        assertAll(
                () -> assertEquals("event/topic", resourceEvent.getTopic()),
                () -> assertEquals("/content/dam/mediahub/corporate_institutionalbankingcib/jcr:content", resourceEvent.getProperty(SlingConstants.PROPERTY_PATH).toString())
        );
    }

    @Test
    void handleEventWithLoginException() throws LoginException {
        Event resourceEvent = new Event("event/topic", Collections.singletonMap(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub/corporate_institutionalbankingcib/jcr:content"));
        fixture.resourceResolverFactory = resourceResolverFactory;
        when(fixture.resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        fixture.handleEvent(resourceEvent);
        Assertions.assertThrows(LoginException.class, () ->{
            fixture.resourceResolverFactory.getServiceResourceResolver(authInfo);
        });
    }

    @Test
    void captureDamAssetChanges() throws PersistenceException {
        Map<String, Object> eventData = new HashMap<>();
        eventData.put(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub");
        eventData.put(BnpConstants.USER_ID, "admin");
        Event resourceEvent = new Event(BnpConstants.TOPIC_RESOURCE_ADDED, eventData);
        ValueMap map = mock(ValueMap.class);
        ModifiableValueMap adpatableResource = mock(ModifiableValueMap.class);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getParent().getValueMap()).thenReturn(map);
        when(map.get(JcrConstants.JCR_PRIMARYTYPE, String.class)).thenReturn(BnpConstants.DAM_ASSET);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(adpatableResource);
        fixture.captureDamAssetChanges(resourceEvent, resolver, resource);

        assertAll(
            () -> assertEquals(BnpConstants.TOPIC_RESOURCE_ADDED, resourceEvent.getTopic()),
            () -> assertEquals(3, resourceEvent.getPropertyNames().length),
            () -> assertEquals(BnpConstants.TOPIC_RESOURCE_ADDED, resourceEvent.getTopic()),
            () -> assertEquals("admin", resourceEvent.getProperty(BnpConstants.USER_ID))
        );
    }

    @Test
    void captureFolderChanges() throws PersistenceException {
        Map<String, Object> eventData = new HashMap<>();
        eventData.put(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub");
        eventData.put(BnpConstants.USER_ID, "admin");
        Event resourceEvent = new Event(BnpConstants.TOPIC_RESOURCE_ADDED, eventData);
        ValueMap map = mock(ValueMap.class);
        ModifiableValueMap adpatableResource = mock(ModifiableValueMap.class);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getParent().getValueMap()).thenReturn(map);
        when(map.get(JcrConstants.JCR_PRIMARYTYPE, String.class)).thenReturn(BnpConstants.SLING_FOLDER);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(adpatableResource);
        fixture.captureFolderChanges(resourceEvent, resolver, resource);

        assertAll(
            () -> assertEquals(BnpConstants.TOPIC_RESOURCE_ADDED, resourceEvent.getTopic()),
            () -> assertEquals(3, resourceEvent.getPropertyNames().length),
            () -> assertEquals(BnpConstants.TOPIC_RESOURCE_ADDED, resourceEvent.getTopic()),
            () -> assertEquals("admin", resourceEvent.getProperty(BnpConstants.USER_ID))
        );
    }

    @Test
    void captureFolderResourceChanged() throws PersistenceException {
        Map<String, Object> eventData = new HashMap<>();
        eventData.put(SlingConstants.PROPERTY_PATH, "/content/dam/mediahub");
        eventData.put(BnpConstants.USER_ID, "admin");
        Event resourceEvent = new Event(BnpConstants.TOPIC_RESOURCE_CHANGED, eventData);
        ValueMap map = mock(ValueMap.class);
        ModifiableValueMap adpatableResource = mock(ModifiableValueMap.class);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getParent().getValueMap()).thenReturn(map);
        when(map.get(JcrConstants.JCR_PRIMARYTYPE, String.class)).thenReturn(BnpConstants.SLING_FOLDER);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(adpatableResource);
        fixture.captureFolderChanges(resourceEvent, resolver, resource);

        assertAll(
            () -> assertEquals(BnpConstants.TOPIC_RESOURCE_CHANGED, resourceEvent.getTopic()),
            () -> assertEquals(3, resourceEvent.getPropertyNames().length),
            () -> assertEquals("admin", resourceEvent.getProperty(BnpConstants.USER_ID))
        );
    }

}
