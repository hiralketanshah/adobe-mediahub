package com.mediahub.core.servlets;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;

import acscommons.com.jcraft.jsch.Session;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
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
class ResetPasswordServletTest {

    @InjectMocks
    ResetPasswordServlet resetPasswordServlet;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;

    @Mock
    Resource resource;

    @Mock
    ValueMap valueMap;
    
    @Mock
    RequestParameter reqParam;
    
    @Mock
    RequestParameterMap reqParamMap;
    
    @Mock
    PrintWriter printWriter;

    @Mock
    Iterator<Resource> requiredFields;
    
    @Mock
    QueryBuilder queryBuilder;

    @Mock
    Query query;

    @Mock
    SearchResult result;
    
    @Mock
    User user;

    Map<String, String[]> parameterMap = new HashMap<>();

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws LoginException, IOException {
        MockitoAnnotations.initMocks(this);

        when(request.getRequestParameterMap()).thenReturn(reqParamMap);
        when(reqParamMap.getValue(Mockito.anyString())).thenReturn(reqParam);
        when(reqParam.getString()).thenReturn("123");

        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
 
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
       /* when(resource.getChild(Mockito.anyString())).thenReturn(resource);
        when(resource.hasChildren()).thenReturn(true);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.anyString())).thenReturn(true);
        */
        when(resource.adaptTo(User.class)).thenReturn(user);
        
        when(resourceResolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);

        
        when(queryBuilder.createQuery(Mockito.any(), Mockito.any())).thenReturn(query);
        when(query.getResult()).thenReturn(result);
        List<Resource> resourceList = new ArrayList<>();
        resourceList.add(resource);
        when(result.getResources()).thenReturn(resourceList.iterator());
        when(resource.listChildren()).thenReturn(resourceList.iterator());
        
        when(response.getWriter()).thenReturn(printWriter);
    }

    @Test
    void doPost() throws ServletException, IOException, LoginException, RepositoryException {

        when(request.getParameterMap()).thenReturn(parameterMap);
        when(valueMap.get("bnpp-title-en", StringUtils.EMPTY)).thenReturn("testValueMap");
        resetPasswordServlet.doPost(request, response);
    }

 
}
