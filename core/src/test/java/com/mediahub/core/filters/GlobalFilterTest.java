package com.mediahub.core.filters;

import static org.junit.jupiter.api.Assertions.assertAll;
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
       assertAll(() -> fixture.doFilter(slingRequest, response, mock(FilterChain.class)));
    }
}
