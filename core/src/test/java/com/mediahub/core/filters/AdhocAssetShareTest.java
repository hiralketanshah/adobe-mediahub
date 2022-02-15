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
package com.mediahub.core.filters;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.servlets.AssetPreviewServlet;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.jcr.Value;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.testing.mock.sling.servlet.MockRequestPathInfo;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletRequest;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith(AemContextExtension.class)
class AdhocAssetShareTest {

    @InjectMocks
    AdhochAssetShare adhochAssetShare;

    @Mock
    SlingHttpServletRequest slingRequest;

    @Mock
    RequestParameterMap requestParameterMap;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    RequestParameter shareLinkValue;

    @Mock
    Resource resource;

    @Mock
    ResourceResolver resolver;

    @Mock
    ResourceResolverFactory resolverFactory;

    @BeforeEach
    void setup() {
        MockitoAnnotations.initMocks(this);
        TestLoggerFactory.clear();
    }


    @Test
    void doFilter(AemContext context) throws IOException, ServletException, LoginException {
        MockSlingHttpServletRequest request = context.request();
        MockSlingHttpServletResponse response = context.response();

        when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
        when(slingRequest.getRequestParameterMap()).thenReturn(requestParameterMap);
        when(requestParameterMap.getValue("shareLink")).thenReturn(shareLinkValue);
        when(shareLinkValue.getString()).thenReturn("http://localhost:4502/linkshare.html?sh=d94ebbee_c411_4031_bc82_9b93d68c2d5f.N8CEhSl8IiTp6sW6OmXzeeGXSvPlbztENlVvKXsahnM&secured=true");
        when(resolver.getResource(anyString())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);

        adhochAssetShare.doFilter(slingRequest, response, mock(FilterChain.class));
        assertNotNull(requestParameterMap.getValue("shareLink"));
    }
}
