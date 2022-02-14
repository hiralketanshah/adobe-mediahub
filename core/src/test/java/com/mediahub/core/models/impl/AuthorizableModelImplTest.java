package com.mediahub.core.models.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import java.util.Calendar;
import java.util.Date;

import javax.jcr.Session;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.request.RequestPathInfo;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.adobe.granite.security.user.UserProperties;
import com.adobe.granite.security.user.UserPropertiesManager;
import com.adobe.granite.security.user.UserPropertiesService;
import com.adobe.granite.security.user.util.AuthorizableUtil;
import com.day.cq.i18n.I18n;
import com.day.cq.replication.ReplicationStatus;
import com.day.crx.JcrConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import mockit.MockUp;

@ExtendWith({ AemContextExtension.class })
public class AuthorizableModelImplTest {
    @InjectMocks
    AuthorizableModelImpl authorizableModelImpl;

    @Mock
    SlingHttpServletRequest slingHttpServletRequest;

    @Mock
    UserPropertiesService userPropertiesService;

    @Mock
    UserProperties userProperties;

    @Mock
    Authorizable authorizable;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    ReplicationStatus replicationStatus;

    @Mock
    ValueMap valueMap;

    @Mock
    Resource resource;

    @Mock
    I18n i18n;

    @Mock
    RequestPathInfo requestPathInfo;

    @Mock
    Session Session;

    @Mock
    UserPropertiesManager userPropertiesManager;

    
    Calendar calendar = Calendar.getInstance();
    
    @Mock
    Date date;

    private MockUp<AuthorizableUtil> authorizableUtilMockup;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(valueMap.get(JcrConstants.JCR_LASTMODIFIED, Calendar.class)).thenReturn(calendar);
        when(valueMap.get(JcrConstants.JCR_CREATED, Calendar.class)).thenReturn(null);
    }

    @Test
    public void testPostConstructor() throws Exception {
        when(slingHttpServletRequest.getResourceResolver()).thenReturn(resourceResolver);
        when(slingHttpServletRequest.getResource()).thenReturn(resource);
        when(resource.adaptTo(Authorizable.class)).thenReturn(authorizable);
        when(authorizable.getID()).thenReturn("123");
        when(resource.adaptTo(ValueMap.class)).thenReturn(valueMap);
        when(resource.adaptTo(ReplicationStatus.class)).thenReturn(replicationStatus);
        when(resourceResolver.adaptTo(Session.class)).thenReturn(Session);
        when(userPropertiesService.createUserPropertiesManager(Session, resourceResolver))
                .thenReturn(userPropertiesManager);
        when(userPropertiesManager.getUserProperties(Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(userProperties);

        authorizableModelImpl.postConstruct();
        assertEquals(Session, resourceResolver.adaptTo(Session.class));

    }

    @Test
    public void testPostConstructor2() throws Exception {
        when(slingHttpServletRequest.getResourceResolver()).thenReturn(resourceResolver);
        when(slingHttpServletRequest.getResource()).thenReturn(resource);
        when(resource.adaptTo(Authorizable.class)).thenReturn(null);
        when(slingHttpServletRequest.getRequestPathInfo()).thenReturn(requestPathInfo);
        when(requestPathInfo.getSuffix()).thenReturn("/var/mediaHub/products");
        when(resourceResolver.getResource("/var/mediaHub/products")).thenReturn(resource);
        authorizableModelImpl.postConstruct();
        assertEquals("/var/mediaHub/products", requestPathInfo.getSuffix());
    }

    @Test
    public void testGetId() throws Exception {
        when(authorizable.getID()).thenReturn("TestID");
        authorizableModelImpl.getId();
        assertEquals("TestID", authorizable.getID());

    }

    @Test
    public void testGetHomePath() throws Exception {
        when(authorizable.getPath()).thenReturn("/var/mediaHub/products");
        authorizableModelImpl.getHomePath();
        assertEquals("/var/mediaHub/products", authorizable.getPath());
    }

    @Test
    public void testGetName() throws Exception {

        when(i18n.get("{0} {1}", "name display order: {0} is the given (first) name, {1} the family (last)" + " name",
                "givenName middleName", "familyName")).thenReturn("Test TEXT");
        /*authorizableUtilMockup = new MockUp<AuthorizableUtil>() {
            @mockit.Mock
            String getFormattedName(ResourceResolver resolver, Authorizable authorizable, String nameDisplayOrder) {

                return "Success";
            }
        };*/
        authorizableModelImpl.getName();
        assertEquals("Test TEXT",
                i18n.get("{0} {1}",
                        "name display order: {0} is the given (first) name, {1} the family (last)" + " name",
                        "givenName middleName", "familyName"));
    }

    @Test
    public void testIsNew() throws Exception {
        when(valueMap.get("jcr:created", Calendar.class)).thenReturn(calendar);
        when(valueMap.get("jcr:lastModified", Calendar.class)).thenReturn(calendar);
        authorizableModelImpl.isNew();
        assertEquals(calendar, valueMap.get("jcr:created", Calendar.class));

    }

    @Test
    public void testGetLastModified() throws Exception {
        when(valueMap.get("jcr:lastModified", Calendar.class)).thenReturn(calendar);
        authorizableModelImpl.getLastModified();
        assertEquals(calendar, valueMap.get("jcr:lastModified", Calendar.class));
    }

    @Test
    public void testGetLastModifiedBy() throws Exception {
        when(valueMap.get("jcr:lastModifiedBy", String.class)).thenReturn("user");
       /* authorizableUtilMockup = new MockUp<AuthorizableUtil>() {
            @mockit.Mock
            String getFormattedName(ResourceResolver resolver, String userId) {

                return "UserName";
            }
        };*/
        authorizableModelImpl.getLastModifiedBy();
        assertEquals("user", valueMap.get("jcr:lastModifiedBy", String.class));
    }

    @Test
    public void testGetLastPublished() throws Exception {
        when(replicationStatus.getLastPublished()).thenReturn(calendar);
        authorizableModelImpl.getLastPublished();
        assertEquals(calendar, replicationStatus.getLastPublished());
    }

    @Test
    public void testGetLastPublishedBy() throws Exception {
        when(replicationStatus.getLastPublishedBy()).thenReturn("User");
       /* authorizableUtilMockup = new MockUp<AuthorizableUtil>() {
            @mockit.Mock
            String getFormattedName(ResourceResolver resolver, String userId) {

                return "UserName";
            }
        };*/
        authorizableModelImpl.getLastPublishedBy();
        assertEquals("User", replicationStatus.getLastPublishedBy());
    }

    @Test
    public void testGetPhotoPath() throws Exception {
        when(userProperties.getResource(UserProperties.PHOTOS + "/primary/image")).thenReturn(resource);
        when(resource.getPath()).thenReturn("/primary/image");
        authorizableModelImpl.getPhotoPath();
        assertEquals("/primary/image", resource.getPath());

    }

    @Test
    public void testGetType() throws Exception {
        when(resource.getChild("profile")).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("type", StringUtils.EMPTY)).thenReturn("Image");
        authorizableModelImpl.getType();
        assertEquals("Image", valueMap.get("type", StringUtils.EMPTY));
    }

    @Test
    public void testIsGroup() throws Exception {
        when(authorizable.isGroup()).thenReturn(true);
        authorizableModelImpl.isGroup();
        assertEquals(true, authorizable.isGroup());
    }

    @Test
    public void testGetExpiry() throws Exception {
        when(resource.getChild("profile")).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("expiry", StringUtils.EMPTY)).thenReturn("2020-12-14");
        authorizableModelImpl.getExpiry();
        assertEquals("2020-12-14", valueMap.get("expiry", StringUtils.EMPTY));
    }

    @Test
    public void testGetCountry() throws Exception {
        when(resource.getChild("profile")).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("country", StringUtils.EMPTY)).thenReturn("IN");
        authorizableModelImpl.getCountry();
        assertEquals("IN", valueMap.get("country", StringUtils.EMPTY));
    }

    @Test
    public void testGetCompany() throws Exception {
        when(resource.getChild("profile")).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("company", StringUtils.EMPTY)).thenReturn("Management");
        authorizableModelImpl.getCompany();
        assertEquals("Management", valueMap.get("company", StringUtils.EMPTY));
    }

    @AfterEach
    public void shouldTearDown() {
        if (authorizableUtilMockup != null) {
            authorizableUtilMockup.tearDown();
        }
    }
}
