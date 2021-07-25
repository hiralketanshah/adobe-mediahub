package com.mediahub.core.filters;

import org.apache.sling.api.resource.LoginException;
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

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@ExtendWith(AemContextExtension.class)
class AssetShareFilterTest {

	@InjectMocks
	private AssetShareFilter fixture;

	private final AemContext context = new AemContext();
	
    @Mock
    ResourceResolverFactory resolverFactory;
    
    @Mock
    ResourceResolver resolver;
    
    @Mock
    HttpServletRequest req;

    @Mock
    HttpServletResponse resp;

	Resource res;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
	@BeforeEach
	void setup() throws LoginException {
		MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resolverFactory);
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        Map<String, String[]> mapOfParams = new HashMap<>();
        mapOfParams.put("sh", new String[] {"test.text"});
        
        when(req.getParameterMap()).thenReturn(mapOfParams);
        
		res = context.create().resource("/var/dam/share/test", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured", "secured",true);
		when(resolver.getResource(Mockito.any(String.class))).thenReturn(res);
		
	}

	@Test
	void extractCredentialsTest(AemContext context) {
		
		 Cookie c = new Cookie("login-token", "123");
	        Cookie[] cookies = new Cookie[1];
	        cookies[0] = c;
	        when(req.getCookies()).thenReturn(cookies);
	        
		fixture.extractCredentials(req, resp);

	}

	@Test
	void extractCredentialsTest1(AemContext context) {
		
	        Cookie[] cookies = new Cookie[0];
	        when(req.getCookies()).thenReturn(cookies);
	        
		fixture.extractCredentials(req, resp);

	}

}
