package com.mediahub.core.schedulers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import java.util.Set;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.UnsupportedRepositoryOperationException;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import acscommons.io.jsonwebtoken.lang.Assert;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({ AemContextExtension.class })
public class AssetExpiryNotificationSchedulerTest {

    @InjectMocks
    private AssetExpiryNotificationScheduler fixture;

    @Mock
    GenericEmailNotification genericEmailNotification;

    @Mock
    private ResourceResolver resolver;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    private SearchResult searchResult;

    @Mock
    ValueMap valueMap;

    @Mock
    private Query query;

    @Mock
    private Session session;

    @Mock
    Resource resource;

    @Mock
    User user;

    @Mock
    UserManager userManager;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Authorizable authorizable;

    @Mock
    SlingSettingsService slingSettingsService;

    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() {
        MockitoAnnotations.initMocks(this);
        Set<String> runModes = new HashSet<>();
        runModes.add("stage");
        when(slingSettingsService.getRunModes()).thenReturn(runModes);
        TestLoggerFactory.clear();
    }

    @Test
    void run() throws LoginException {
        try {
            AssetExpiryNotificationScheduler.Config config = mock(AssetExpiryNotificationScheduler.Config.class);
            when(config.getDamPath()).thenReturn(BnpConstants.DAM_PATH);
            when(config.scheduler_expression()).thenReturn("0 0 6 1/1 * ? *");
            when(config.scheduler_concurrent()).thenReturn(Boolean.FALSE);
            QueryBuilder queryBuilder = mock(QueryBuilder.class);
            when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
            when(resolver.adaptTo(Session.class)).thenReturn(session);
            when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
            when(resolver.adaptTo(Session.class)).thenReturn(session);
            when(queryBuilder.createQuery(any(PredicateGroup.class), any(Session.class))).thenReturn(query);
            when(query.getResult()).thenReturn(searchResult);
            when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
            List<Resource> userList = new ArrayList<>();
            userList.add(resource);
            when(searchResult.getResources()).thenReturn(userList.iterator());
            when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
            when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
            when(resource.getValueMap()).thenReturn(valueMap);
            when(valueMap.get(BnpConstants.EXPIRY, String.class)).thenReturn("2019/06/09");
            when(valueMap.get(JcrConstants.JCR_CREATED_BY, StringUtils.EMPTY)).thenReturn("admin");
            when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
            when(modifiableValueMap.get("notified", Boolean.FALSE)).thenReturn(Boolean.FALSE);
            when(resource.getPath()).thenReturn("/etc/home/user");
            when(userManager.getAuthorizableByPath("/etc/home/user")).thenReturn(user);
            when(user.isDisabled()).thenReturn(false);
            when(valueMap.get(BnpConstants.EMAIL, String.class)).thenReturn("MediaHub@gmail.com");
            when(valueMap.get(BnpConstants.FIRST_NAME, String.class)).thenReturn("TestUser");
            when(resolver.hasChanges()).thenReturn(true);
            fixture.activate(config);
            fixture.run();
            assertEquals("0 0 6 1/1 * ? *", config.scheduler_expression());
            assertEquals(Boolean.FALSE, config.scheduler_concurrent());
        } catch (UnsupportedRepositoryOperationException e) {
            e.printStackTrace();
        } catch (RepositoryException e) {
            e.printStackTrace();
        }
    }

    @Test
    void sendWarningMail() {
        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.FIRST_NAME, org.apache.commons.lang3.StringUtils.EMPTY)).thenReturn("FirstName");
        fixture.sendWarningMail(resource, new String[] { "abibrahi@adobe.com" },
                "/etc/mediahub/mailtemplates/assetexpirationtemplate.html", resource);
        Assert.notNull(resource.getChild(BnpConstants.PROFILE));
    }

    @Test
    void sendWarningMail1() {
        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(null);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.FIRST_NAME, org.apache.commons.lang3.StringUtils.EMPTY)).thenReturn("FirstName");
        fixture.sendWarningMail(resource, new String[] { "abibrahi@adobe.com" },
                "/etc/mediahub/mailtemplates/assetexpirationtemplate.html", resource);
        Assert.isNull(resource.getChild(BnpConstants.PROFILE));
    }

}
