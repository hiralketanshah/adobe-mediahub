package com.mediahub.core.services.impl;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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

import com.adobe.acs.commons.email.EmailService;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.AuthService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class AuthServiceImplTest {

    AemContext context = new AemContext();

    @InjectMocks
    AuthServiceImpl authServiceImpl;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    Resource resource;

    @Mock
    AuthServiceImpl.Config authServiceImplConfig;

    Map<String, Object> parameters = new HashMap<>();

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    Map<String, String> parametersMap = new HashMap<>();

    InputStream isList;

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
        parameters.put("metascopes", "abc,xyz");
        parameters.put("jwtToken", "abc,xyz");
        when(authServiceImplConfig.keyExpiration()).thenReturn(24);
        when(authServiceImplConfig.metascopes()).thenReturn("test1,test2");
        when(authServiceImplConfig.privateKey()).thenReturn(
                "AAAAB3NzaC1yc2EAAAABJQAAAQEAj+mDdeaqGrDESy6i/xWkXeDZhgH7a43o1NcQturDjFofGtgTxzIZUl+oLdJYg4jiR2qzlfno0RsMHE0/cwLObcLxP07LspFV8mgg8JQmGIHQHKAmDzZfRYVyRCRlDqSD+3yXQAEqpb4wk8Q2jEYDdvb4q4DRBKk97ZUFmJ7w0S5O4c1mYaAMBY+MLXwDbPCHk8aoL/ltKuqx5yBLuEitvgyVovV7llJ62uvC4KT+cc4D0AQIha8kyaTpIKGKKLynMY44IV9zFyBOWEOmbj9ca+D7hHYsosoQ4Ns+IMxDUG6E3atm3o4w22tWPzG2e43CfO5rlIeVRD+fS7EBedeWrw==");

        when(resourceResolver.getResource(Mockito.anyString())).thenReturn(resource);

        authServiceImpl.activate(authServiceImplConfig);

    }

    @Test
    public void testGetCustomReport() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);

        parametersMap.put("template", "testTemplate");
        assertThrows(RuntimeException.class, () -> {
            
        authServiceImpl.getAuthToken();

    });
    }

}
