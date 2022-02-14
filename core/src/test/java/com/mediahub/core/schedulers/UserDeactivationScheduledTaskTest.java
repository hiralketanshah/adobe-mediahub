package com.mediahub.core.schedulers;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.UserUtils;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.*;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.Value;

import java.security.Principal;
import java.text.ParseException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith({AemContextExtension.class})
public class UserDeactivationScheduledTaskTest {

    @InjectMocks
    private UserDeactivationScheduledTask fixture;

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
    Group group;
    
    @Mock
    Principal principal;
    
    @Mock
    UserManager userManager;

    @Mock
    QueryBuilder builder;

    @Mock
    Authorizable authorizable;
    
    @Mock
    SlingSettingsService slingSettingsService;

    @Mock
    I18nProvider provider;
    
    @Mock
    Value val;
    
    
    Value[] values;

    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() {
        MockitoAnnotations.initMocks(this);
        values = new Value[] { val };
        Set<String> runModes = new HashSet<>();
        runModes.add("stage");
        when(slingSettingsService.getRunModes()).thenReturn(runModes);
        TestLoggerFactory.clear();
        
        when(resolver.adaptTo(QueryBuilder.class)).thenReturn(builder);
        when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
        when(query.getResult()).thenReturn(searchResult);
        List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(searchResult.getResources()).thenReturn(userList.iterator());
    }

    @Test
    void run() throws LoginException {
        try {
            List<Group> listOfGroups = new ArrayList<>();
            when(user.memberOf()).thenReturn(listOfGroups.listIterator());
            UserDeactivationScheduledTask.Config config = mock(UserDeactivationScheduledTask.Config.class);
            when(config.getUserType()).thenReturn(BnpConstants.EXTERNAL);
            when(config.scheduler_expression()).thenReturn("0 1 0 1/1 * ? *");
            when(config.scheduler_concurrent()).thenReturn(Boolean.FALSE);
            when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
            when(resolver.adaptTo(Session.class)).thenReturn(session);
            
            when(resolver.adaptTo(Session.class)).thenReturn(session);
            
            
            when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
            
            when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
            when(resource.getValueMap()).thenReturn(valueMap);
            when(valueMap.get(BnpConstants.EXPIRY, String.class)).thenReturn("2019/06/09");
            when(valueMap.get(BnpConstants.EXPIRY, StringUtils.EMPTY)).thenReturn("2019/06/09");
            when(resource.getPath()).thenReturn("/etc/home/user");
            when(userManager.getAuthorizableByPath("/etc/home/user")).thenReturn(user);
            when(user.isDisabled()).thenReturn(false);
            when(valueMap.get(BnpConstants.EMAIL, String.class)).thenReturn("MediaHub@gmail.com");
            when(valueMap.get(BnpConstants.FIRST_NAME, String.class)).thenReturn("TestUser");
            when(resolver.hasChanges()).thenReturn(true);
            fixture.activate(config);
            fixture.run();

            assertEquals("0 1 0 1/1 * ? *", config.scheduler_expression());
            assertEquals(Boolean.FALSE, config.scheduler_concurrent());

        } catch (UnsupportedRepositoryOperationException e) {
            e.printStackTrace();
        } catch (RepositoryException e) {
            e.printStackTrace();
        }
    }

    @Test
    void sendWarningMail() throws RepositoryException {
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY)).thenReturn("FirstName");
        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
        when(provider.translate("User will be Deactivated in 30 days", LocaleUtils
                .toLocale(UserUtils.getUserLanguage(user)))).thenReturn("User will be Deactivated in 30 days");
        fixture.sendMail(resource, new String[]{"abibrahi@adobe.com"},
                "/etc/mediahub/mailtemplates/userexpirationmailtemplate.html", "User will be Deactivated in 30 days");
    }

    @Test
    void sendDeactivationgMail() throws RepositoryException {
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY)).thenReturn("FirstName");
        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
        when(provider.translate("User will be Deactivated", LocaleUtils
                .toLocale(UserUtils.getUserLanguage(user)))).thenReturn("User will be Deactivated");
        fixture.sendMail(resource, new String[]{"abibrahi@adobe.com"},
                "/etc/mediahub/mailtemplates/userexpirationmailtemplate.html", "User will be Deactivated");
    }

    @Test
    void getPredicateMapProjectSearch() {
        Map<String, String> map = fixture.getPredicateMapProjectSearch(BnpConstants.AEM_PROJECTS_PATH,
                "externalContributer");
        assertEquals(map.get(BnpConstants.PATH), BnpConstants.AEM_PROJECTS_PATH);
    }

    @Test
    void getMembersFromGroup() throws LoginException {

        QueryBuilder queryBuilder = mock(QueryBuilder.class);
        when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.SLING_RESOURCETYPE, StringUtils.EMPTY)).thenReturn("");

        List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(builder.createQuery(any(PredicateGroup.class), any(Session.class))).thenReturn(query);
        when(query.getResult()).thenReturn(searchResult);
        when(searchResult.getResources()).thenReturn(userList.iterator());
        fixture.getMembersFromGroup(userManager, builder, resolver, "groupName", "role");
    }

    @Test
    void fetchUserMailFromGroup() throws ParseException, RepositoryException {
        List<Group> groupList = new ArrayList<>();
        groupList.add(group);
        
        List<Authorizable> userList = new ArrayList<>();
        userList.add(user);
        when(user.isGroup()).thenReturn(false);
        when(user.getProperty(Mockito.anyString())).thenReturn(values);
        when(user.memberOf()).thenReturn(groupList.listIterator());
        when(group.getPrincipal()).thenReturn(principal);
        when(principal.getName()).thenReturn("test-external-contributor");
        when(userManager.getAuthorizableByPath(Mockito.any())).thenReturn(user);
        when(userManager.getAuthorizable(Mockito.anyString())).thenReturn(group);
        when(group.isGroup()).thenReturn(true);
        when(group.getMembers()).thenReturn(userList.listIterator());
        when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(Mockito.anyString())).thenReturn("test");
        when(valueMap.get(Mockito.anyString(), Mockito.anyString())).thenReturn("cq/gui/components/projects/admin/card/projectcard");
        
        Calendar calendar = Calendar.getInstance();
        fixture.deactivateExpiredUsers(userManager, resource, calendar, builder, resolver);
        calendar.add(Calendar.DATE, -1);
        fixture.deactivateExpiredUsers(userManager, resource, calendar, builder, resolver);
        calendar.add(Calendar.DATE, -2);
        fixture.deactivateExpiredUsers(userManager, resource, calendar, builder, resolver);
        calendar.add(Calendar.DATE, -7);
        fixture.deactivateExpiredUsers(userManager, resource, calendar, builder, resolver);
    }
    
}
