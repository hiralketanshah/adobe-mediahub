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
package com.mediahub.core.servlets;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.ServletException;
import mockit.MockUp;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
class CheckActiveChildAssetTest {

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    UserManager userManager;

    @Mock
    Authorizable authorizable;

    @InjectMocks
    CheckActiveChildAssets checkActiveChildAssets;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    SlingHttpServletResponse response;

    @Mock
    Group group;

    @Mock
    RequestParameter requestParameter;

    @Mock
    PrintWriter out;

    @Mock
    Resource resource;

    @Mock
    ValueMap valueMap;

    private MockUp<DamUtil> damUtils;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
        damUtils = new MockUp<DamUtil>() {
            @mockit.Mock
            Iterator<Asset> getAssets(Resource resource) {
                List<Asset> assets = new ArrayList<>();
                return assets.iterator();
            }
        };
    }

    @Test
    void doGet()
        throws ServletException, IOException, LoginException, RepositoryException {

        //MockSlingHttpServletRequest request = context.request();
        //MockSlingHttpServletResponse response = context.response();

        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(userManager.getAuthorizable("administrators")).thenReturn((Authorizable) group);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getUserID()).thenReturn("admin");
        when(request.getRequestParameter("paths")).thenReturn(requestParameter);
        when(requestParameter.toString()).thenReturn("");
        when(response.getWriter()).thenReturn(null);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(resource.hasChildren()).thenReturn(false);
        checkActiveChildAssets.doGet(request, response);

        //assertEquals("Title = resource title", response.getOutputAsString());
    }

    @Test
    void doGet1()
        throws ServletException, IOException, LoginException, RepositoryException {

        ArrayList<Resource> arrayList = new ArrayList<>();
        arrayList.add(resource);
        Iterator<Resource> iterator = arrayList.iterator();

        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(userManager.getAuthorizable("administrators")).thenReturn((Authorizable) group);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getUserID()).thenReturn("admin");
        when(request.getRequestParameter("paths")).thenReturn(requestParameter);
        when(requestParameter.toString()).thenReturn("");
        when(resourceResolver.getResource("")).thenReturn(resource);
        when(response.getWriter()).thenReturn(null);
        when(resource.hasChildren()).thenReturn(Boolean.TRUE);
        when(resource.listChildren()).thenReturn(iterator);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
        when(resource.getChild(BnpConstants.METADATA)).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);

        checkActiveChildAssets.doGet(request, response);
    }

    @AfterEach
    public void shouldTearDown() {
        if (damUtils != null) {
            damUtils.tearDown();
        }
    }
}
