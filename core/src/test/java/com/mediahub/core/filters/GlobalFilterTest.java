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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith({AemContextExtension.class})
class GlobalFilterTest {

	private GlobalFilter fixture = new GlobalFilter();

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
    }


    @Test
    void doFilter(AemContext context) throws IOException, ServletException, LoginException {
        MockSlingHttpServletResponse response = context.response();

       when(slingRequest.getHeader(Mockito.anyString())).thenReturn("test,test");
        fixture.doFilter(slingRequest, response, mock(FilterChain.class));
    }
}
