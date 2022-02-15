package com.mediahub.core.services.impl;

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
public class AnalyticsGetterServiceImplTest {

    AemContext context = new AemContext();

    @InjectMocks
    AnalyticsGetterServiceImpl analyticsGetterServiceImpl;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    Resource resource;

    @Mock
    AnalyticsGetterServiceImpl.Config config;

    @Mock
    AuthServiceImpl.Config authServiceImplConfig;

    @Mock
    AuthService authService;

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
        when(config.analyticsApiUrl()).thenReturn("https://httpbin.org/");
        when(config.globalCompanyId()).thenReturn("anything");
        when(resourceResolver.getResource(Mockito.anyString())).thenReturn(resource);
        ClassLoader classloader = Thread.currentThread().getContextClassLoader();
        isList = classloader.getResourceAsStream("users-list.csv");
        when(resource.adaptTo(InputStream.class)).thenReturn(isList);
        analyticsGetterServiceImpl.activate(config);

    }

    @Test
    public void testGetCustomReport() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);

        parametersMap.put("template", "testTemplate");
        analyticsGetterServiceImpl.getCustomReport(parametersMap, Calendar.getInstance().getTime(),
                Calendar.getInstance().getTime());

    }

    @Test
    public void testGetCustomReportWithoutTemplate() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);

        analyticsGetterServiceImpl.getCustomReport(parametersMap, Calendar.getInstance().getTime(),
                Calendar.getInstance().getTime());

    }

    @Test
    public void testGetCustomReportError() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(LoginException.class);
        parametersMap.put("template", "testTemplate");
        analyticsGetterServiceImpl.getCustomReport(parametersMap, Calendar.getInstance().getTime(),
                Calendar.getInstance().getTime());

    }
    
    @Test
    public void testGetCustomReportNoResource() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        parametersMap.put("template", "testTemplate");
        when(resourceResolver.getResource(Mockito.anyString())).thenReturn(null);

        analyticsGetterServiceImpl.getCustomReport(parametersMap, Calendar.getInstance().getTime(),
                Calendar.getInstance().getTime());

    }

}
