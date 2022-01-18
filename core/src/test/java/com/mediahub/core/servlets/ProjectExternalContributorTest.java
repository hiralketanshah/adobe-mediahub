package com.mediahub.core.servlets;

import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestPathInfo;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class ProjectExternalContributorTest {

    @InjectMocks
    ProjectExternalContributor projectExternalContributor;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;
    
    @Mock
    RequestPathInfo reqPathInfo;
    
    @Mock
    Resource resource;
    
    @Mock
    ValueMap valueMap;
    
    @Mock
    UserManager um;
    
    @Mock
    Group group;
    
    @Mock
    PrintWriter printWriter;

  
    InputStream stream;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException, IOException, RepositoryException {
        MockitoAnnotations.initMocks(this);
        List<Authorizable> listOfMembers = new ArrayList<>();
        listOfMembers.add(group);
        when(request.getRequestPathInfo()).thenReturn(reqPathInfo);
        when(resourceResolver.getResource(Mockito.anyString())).thenReturn(resource);
        when(resource.adaptTo(ValueMap.class)).thenReturn(valueMap);
        when(resourceResolver.adaptTo(UserManager.class)).thenReturn(um);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.any())).thenReturn(true);
        when(valueMap.get(Mockito.anyString(), Mockito.any())).thenReturn("test");
        when(valueMap.get(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR, StringUtils.EMPTY)).thenReturn("test");
        when(reqPathInfo.getSuffix()).thenReturn("/content/dam/test");
        when(um.getAuthorizable(Mockito.anyString())).thenReturn(group);
        when(group.isGroup()).thenReturn(true);
        when(group.getMembers()).thenReturn(listOfMembers.iterator());
        when(response.getWriter()).thenReturn(printWriter);
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        stream = classloader.getResourceAsStream("media-updater.csv");
    }

    @Test
    void doGet() throws ServletException, IOException, LoginException {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        projectExternalContributor.doGet(request, response);
    }
    
    @Test
    void doGetError() throws ServletException, IOException, LoginException {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        projectExternalContributor.doGet(request, response);
    }
}
