package com.mediahub.core.services.impl;

import com.adobe.granite.asset.api.Asset;
import com.adobe.granite.asset.api.Rendition;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.data.UserInfo;
import com.mediahub.core.data.UserStatus;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.jackrabbit.api.security.user.AuthorizableExistsException;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.PersistenceException;
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

import javax.jcr.*;
import javax.management.NotCompliantMBeanException;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.Principal;
import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

@ExtendWith({AemContextExtension.class})
public class UpdateInternalUsersServiceImplTest {

    @InjectMocks
    UpdateInternalUsersServiceImpl fetchPrice;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    ResourceResolver resolver;

    @Mock
    UserManager userManager;

    @Mock
    Session session;

    @Mock
    Asset resource;

    @Mock
    Asset resourceInfo;

    @Mock
    Asset resourceStatus;

    @Mock
    User user;

    @Mock
    Value val;

    @Mock
    ValueFactory valFactory;

    @Mock
    Asset asset;

    @Mock
    Rendition rendition;

    @Mock
    Asset assetInfo;

    @Mock
    Rendition renditionInfo;

    @Mock
    Asset assetStatus;

    @Mock
    Rendition renditionStatus;

    @Mock
    Query query;

    @Mock
    QueryBuilder builder;

    @Mock
    SearchResult result;

    @Mock
    Hit hit;

    @Mock
    Group group;

    @Mock
    Node node;

    InputStream isList;
    InputStream isInfo;
    InputStream isStatus;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
    Resource csvResource;

    @BeforeEach
    public void setup() throws NotCompliantMBeanException, org.apache.sling.api.resource.LoginException {
        MockitoAnnotations.initMocks(this);
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        isList = classloader.getResourceAsStream("users-list.csv");
        isInfo = classloader.getResourceAsStream("users-info.csv");
        isStatus = classloader.getResourceAsStream("users-status.csv");

    }

    @Test
    public void testRun() throws AuthorizableExistsException, RepositoryException, PersistenceException {
        Value[] propertyValue = new Value[]{val};


        BufferedReader brUserList = new BufferedReader(new InputStreamReader(isList));
        Map<String, com.mediahub.core.data.User> inputMap = fetchPrice.convertStreamToHashMap(brUserList, false);

        BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(isInfo));
        Map<String, UserInfo> infoMap = fetchPrice.convertStreamToHashMapUserInfo(brUserInfo, false);

        BufferedReader brUserStatus = new BufferedReader(new InputStreamReader(isStatus));
        Map<String, UserStatus> statusMap = fetchPrice.convertStreamToHashMapUserStatus(brUserStatus, false);

        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(resolver.getResource(BnpConstants.CSV_FILE_PATH)).thenReturn(resource);
        when(resolver.getResource(BnpConstants.CSV_USER_INFO)).thenReturn(resourceInfo);
        when(resolver.getResource(BnpConstants.CSV_USER_STATUS)).thenReturn(resourceStatus);
        when(session.getValueFactory()).thenReturn(valFactory);
        when(userManager.createUser(Mockito.anyString(), Mockito.anyString(), Mockito.any(Principal.class),
                Mockito.anyString())).thenReturn(user);
        when(session.getNode(Mockito.anyString())).thenReturn(node);
        when(userManager.getAuthorizable(BnpConstants.MEDIAHUB_READER_MEDIALIBRARY)).thenReturn(group);
        when(user.getProperty(Mockito.anyString())).thenReturn(propertyValue);
        when(resource.adaptTo(Asset.class)).thenReturn(asset);
        when(asset.getRendition(Mockito.anyString())).thenReturn(rendition);
        when(rendition.getStream()).thenReturn(isList);

        when(resourceInfo.adaptTo(Asset.class)).thenReturn(assetInfo);
        when(assetInfo.getRendition(Mockito.anyString())).thenReturn(renditionInfo);
        when(renditionInfo.getStream()).thenReturn(isInfo);

        when(resourceStatus.adaptTo(Asset.class)).thenReturn(assetStatus);
        when(assetStatus.getRendition(Mockito.anyString())).thenReturn(renditionStatus);
        when(renditionStatus.getStream()).thenReturn(isStatus);

        when(builder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
        when(query.getResult()).thenReturn(result);
        assertAll(() -> fetchPrice.createAndUpdateUsers(BnpConstants.CSV_FILE_PATH, BnpConstants.CSV_USER_INFO,
                BnpConstants.CSV_USER_STATUS));
        assertAll(() -> fetchPrice.createAndSaveUsers(inputMap, infoMap, statusMap,
                userManager, session));
        assertAll(() -> fetchPrice.deletedUnwantedUsers(resolver, inputMap));
        assertAll(() -> fetchPrice.removeAllUsers());
    }

}
