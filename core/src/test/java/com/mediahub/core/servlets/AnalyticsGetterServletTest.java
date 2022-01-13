package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.security.KeyFactory;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
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

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.AnalyticsGetterService;
import com.mediahub.core.services.AuthService;
import com.mediahub.core.services.impl.AnalyticsGetterServiceImpl;
import com.mediahub.core.services.impl.AuthServiceImpl;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class AnalyticsGetterServletTest {

    private final AemContext context = new AemContext();

    @InjectMocks
    AnalyticsGetterServlet analyticsGetterServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    PrintWriter printWriter;

    @InjectMocks
    AnalyticsGetterService analyticsService = new AnalyticsGetterServiceImpl();
    
    @Mock
    AnalyticsGetterServiceImpl.Config config;

    @Mock
    AuthService authService;

    @Mock
    ResourceResolverFactory resolverFactory;
    
    @Mock
    ResourceResolver resourceResolver;
    
    @Mock
    Resource resource;

    Value[] values;

    Map<String, Object> parameters = new HashMap<>();
    
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws RepositoryException, IOException, LoginException {
        MockitoAnnotations.initMocks(this);
        
        

        parameters.put("metascopes", "abc,xyz");
        parameters.put("jwtToken", "abc,xyz");
        parameters.put("privateKey",
                "AAAAB3NzaC1yc2EAAAABJQAAAQEAj+mDdeaqGrDESy6i/xWkXeDZhgH7a43o1NcQturDjFofGtgTxzIZUl+oLdJYg4jiR2qzlfno0RsMHE0/cwLObcLxP07LspFV8mgg8JQmGIHQHKAmDzZfRYVyRCRlDqSD+3yXQAEqpb4wk8Q2jEYDdvb4q4DRBKk97ZUFmJ7w0S5O4c1mYaAMBY+MLXwDbPCHk8aoL/ltKuqx5yBLuEitvgyVovV7llJ62uvC4KT+cc4D0AQIha8kyaTpIKGKKLynMY44IV9zFyBOWEOmbj9ca+D7hHYsosoQ4Ns+IMxDUG6E3atm3o4w22tWPzG2e43CfO5rlIeVRD+fS7EBedeWrw==");

        context.registerService(AuthService.class, authService);
        context.registerService(ResourceResolverFactory.class, resolverFactory);
        context.registerService(AnalyticsGetterService.class, analyticsService);
        
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(resourceResolver.getResource(Mockito.any())).thenReturn(resource);

        when(req.getParameter("startDate")).thenReturn("04-01-2022");
        when(req.getParameter("endDate")).thenReturn("06-01-2022");
        when(req.getParameter("path")).thenReturn("/content/dam/test.jpg");
        when(resp.getWriter()).thenReturn(printWriter);
      //  context.registerInjectActivateService(authService, parameters);
        context.registerInjectActivateService(analyticsService);
        context.registerInjectActivateService(analyticsGetterServlet);

    }

    public void testDoGet() throws ValueFormatException, IllegalStateException, RepositoryException {
        when(req.getParameter("dimension")).thenReturn("dimension");

        assertAll(() -> analyticsGetterServlet.doGet(req, resp));
    }

    @Test
    public void testDoGetOne() {
        when(req.getRequestParameterMap()).thenReturn(new RequestParameterMap() {

            @Override
            public Collection<RequestParameter[]> values() {
                return null;
            }

            @Override
            public int size() {
                return 0;
            }

            @Override
            public RequestParameter[] remove(Object key) {
                return null;
            }

            @Override
            public void putAll(Map<? extends String, ? extends RequestParameter[]> m) {
            }

            @Override
            public RequestParameter[] put(String key, RequestParameter[] value) {
                return null;
            }

            @Override
            public Set<String> keySet() {
                Set<String> testSet = new HashSet<>();
                testSet.add("template");
                return testSet;
            }

            @Override
            public boolean isEmpty() {
                return false;
            }

            @Override
            public RequestParameter[] get(Object key) {

                RequestParameter reqParam = new RequestParameter() {

                    @Override
                    public boolean isFormField() {
                        return false;
                    }

                    @Override
                    public String getString(String arg0) throws UnsupportedEncodingException {
                        return null;
                    }

                    @Override
                    public String getString() {
                        return "test";
                    }

                    @Override
                    public long getSize() {
                        return 0;
                    }

                    @Override
                    public String getName() {
                        return null;
                    }

                    @Override
                    public InputStream getInputStream() throws IOException {
                        return null;
                    }

                    @Override
                    public String getFileName() {
                        return null;
                    }

                    @Override
                    public String getContentType() {
                        return null;
                    }

                    @Override
                    public byte[] get() {
                        return null;
                    }
                };

                return new RequestParameter[] { reqParam };
            }

            @Override
            public Set<Entry<String, RequestParameter[]>> entrySet() {
                return null;
            }

            @Override
            public boolean containsValue(Object value) {
                return false;
            }

            @Override
            public boolean containsKey(Object key) {
                return false;
            }

            @Override
            public void clear() {

            }

            @Override
            public RequestParameter[] getValues(String arg0) {
                return null;
            }

            @Override
            public RequestParameter getValue(String arg0) {
                return null;
            }
        });
        assertAll(() -> analyticsGetterServlet.doGet(req, resp));
    }

}
