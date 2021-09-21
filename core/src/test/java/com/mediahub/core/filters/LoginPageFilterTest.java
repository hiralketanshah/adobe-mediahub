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

import java.io.IOException;
import java.security.Principal;
import java.util.List;

import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import static org.mockito.Mockito.when;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

@ExtendWith(AemContextExtension.class)
class LoginPageFilterTest {

    private LoginPageFilter fixture = new LoginPageFilter();

    private TestLogger logger = TestLoggerFactory.getTestLogger(fixture.getClass());
    
    @Mock
    SlingHttpServletRequest request;
    
    @Mock
    SlingHttpServletResponse response;
    
    @Mock
    Principal principal;
    
    @Mock
    UserManager userManager;
    
	@Mock
	ResourceResolver resolver;
	
	@Mock
	User authorizable;
	
	@Mock
	Group group;
	
	@Mock
	Resource resource;
    
	@Mock
	ValueMap valueMap;
	
	@Mock
	Value val;

    @BeforeEach
    void setup() {
    	MockitoAnnotations.initMocks(this);
    }


    @Test
    void doFilter(AemContext context) throws IOException, ServletException, RepositoryException {
    	
    	Value[] value = {val};
        
    	when(request.getUserPrincipal()).thenReturn(principal);
    	when(request.getRequestURI()).thenReturn("/apps/mediahub/content/test.html");
    	when(request.getResourceResolver()).thenReturn(resolver);
    	when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
    	when(userManager.getAuthorizable(principal)).thenReturn(authorizable);
    	when(authorizable.getProperty(Mockito.anyString())).thenReturn(value);
    	when(userManager.getAuthorizable("mediahub-basic")).thenReturn(group);
    	when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
    	when(resource.getChild(Mockito.anyString())).thenReturn(resource);
    	when(resource.getValueMap()).thenReturn(valueMap);
    	when(group.isMember(Mockito.any(User.class))).thenReturn(true);
    	when(authorizable.isAdmin()).thenReturn(false);
        fixture.init(mock(FilterConfig.class));
        fixture.doFilter(request, response, mock(FilterChain.class));
        fixture.destroy();

        List<LoggingEvent> events = logger.getLoggingEvents();
        assertEquals(1, events.size());
        
    }
}
