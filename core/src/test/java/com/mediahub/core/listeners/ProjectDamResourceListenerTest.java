package com.mediahub.core.listeners;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.day.cq.dam.api.DamConstants;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;

import javax.jcr.RepositoryException;
import javax.jcr.Value;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChange.ChangeType;
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
class ProjectDamResourceListenerTest {

	@InjectMocks
    private ProjectDamResourceListener fixture = new ProjectDamResourceListener();

    @InjectMocks
    private TestLogger logger = TestLoggerFactory.getTestLogger(fixture.getClass());

    private AemContext context;
    
    @Mock
    GenericEmailNotification genericEmailNotification;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private Resource resource;

    @Mock
    ResourceChange resourceChanged;

    @Mock
    ValueMap valueMap;

    @Mock
    Group authorizable;
    
    @Mock
    User authorizableUser;

    @Mock
    UserManager userManager;
    
    @Mock
    private Value value;

    @Mock
    private Value value1;

    @Mock
    private Value value2;
    
    @Mock
    User user;


    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() throws LoginException {
    	MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        context.registerService(GenericEmailNotification.class, genericEmailNotification);
    }

    @Test
    void handleEvent() throws LoginException, PersistenceException {
        ArrayList<Resource> arrayList = new ArrayList<>();
        Iterator<Resource> iterator = arrayList.iterator();
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/"));
        fixture.resolverFactory = resourceResolverFactory;
        when(fixture.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getChild(BnpConstants.TEMP)).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getName()).thenReturn(StringUtils.EMPTY);
        doNothing().when(resolver).commit();
        when(resolver.copy(any(),any())).thenReturn(resource);
        when(resolver.create(any(),any(),any())).thenReturn(resource);
        when(resource.listChildren()).thenReturn(iterator);

        List<ResourceChange> list = new ArrayList<>();
        list.add(resourceChanged);
        fixture.onChange(list);
    }

    @Test
    void handleEvent1() throws LoginException, PersistenceException, RepositoryException {
        ArrayList<Resource> arrayList = new ArrayList<>();
        Iterator<Resource> iterator = arrayList.iterator();
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/"));
        fixture.resolverFactory = resourceResolverFactory;
        when(fixture.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getChild(BnpConstants.TEMP)).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getName()).thenReturn(StringUtils.EMPTY);
        doNothing().when(resolver).commit();
        when(resolver.copy(any(),any())).thenReturn(resource);
        when(resolver.create(any(),any(),any())).thenReturn(resource);
        when(resource.listChildren()).thenReturn(iterator);

        List<ResourceChange> list = new ArrayList<>();
        list.add(resourceChanged);
        when(resourceChanged.getType()).thenReturn(ChangeType.ADDED);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.SLING_RESOURCETYPE, "")).thenReturn("granite/comments/components/comment");

        when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(userManager.getAuthorizable("admin")).thenReturn(authorizableUser);
        when(valueMap.get(com.day.cq.commons.jcr.JcrConstants.JCR_CREATED_BY, org.apache.commons.lang3.StringUtils.EMPTY)).thenReturn("admin");
        when(authorizableUser.isGroup()).thenReturn(Boolean.FALSE);
        when(value.getString()).thenReturn("test@gmail.com");
        Value[] valueArray = { value };
        when(value1.getString()).thenReturn("Test");
        Value[] valueArray1 = { value1 };
        when(authorizableUser.getProperty("./profile/email")).thenReturn(valueArray);
        when(authorizableUser.getProperty(BnpConstants.PROFILE_GIVEN_NAME)).thenReturn(valueArray1);

        fixture.onChange(list);
    }

    @Test
    void handleEvent2() throws LoginException, PersistenceException, RepositoryException {
        ArrayList<Resource> arrayList = new ArrayList<>();
        Iterator<Resource> iterator = arrayList.iterator();
        Event resourceEvent = new Event(BnpConstants.EVENT_TOPIC, Collections.singletonMap(SlingConstants.PROPERTY_PATH, BnpConstants.CONF_FOLDERMETADATASCHEMA + "/mediahub-medias-schema/jcr:content/tabs/"));
        fixture.resolverFactory = resourceResolverFactory;
        when(fixture.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.getResource(any())).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getChild(BnpConstants.TEMP)).thenReturn(resource);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getName()).thenReturn(StringUtils.EMPTY);
        doNothing().when(resolver).commit();
        when(resolver.copy(any(),any())).thenReturn(resource);
        when(resolver.create(any(),any(),any())).thenReturn(resource);
        when(resource.listChildren()).thenReturn(iterator);

        List<ResourceChange> list = new ArrayList<>();
        list.add(resourceChanged);
        when(resourceChanged.getType()).thenReturn(ChangeType.ADDED);
        when(resource.getResourceType()).thenReturn(DamConstants.NT_DAM_ASSETCONTENT);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.SLING_RESOURCETYPE, "")).thenReturn("granite/comments/components/comment");

        when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(userManager.getAuthorizable("admin")).thenReturn(authorizable);
        when(valueMap.get(BnpConstants.ROLE_OWNER, org.apache.commons.lang3.StringUtils.EMPTY)).thenReturn("admin");
        when(authorizable.isGroup()).thenReturn(Boolean.TRUE);
        when(value.getString()).thenReturn("test@gmail.com");
        final List<Authorizable> groups = new ArrayList<>();
        user.setProperty("./profile/email", value);
		groups.add(user);
        when(authorizable.getMembers()).thenReturn(groups.iterator());

        fixture.sendNotificationEmail(resolver, resource);
    }

}
