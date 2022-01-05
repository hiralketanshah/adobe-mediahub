package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.services.AnalyticsGetterService;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class AnalyticsGetterServletTest {

    @InjectMocks
    AnalyticsGetterServlet analyticsGetterServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    PrintWriter printWriter;

    @Mock
    AnalyticsGetterService analyticsService;

    Value[] values;

    @BeforeEach
    public void setupMock() throws RepositoryException, IOException {
        MockitoAnnotations.initMocks(this);

        when(req.getParameter("startDate")).thenReturn("04-01-2022");
        when(req.getParameter("endDate")).thenReturn("06-01-2022");
        when(req.getParameter("path")).thenReturn("/content/dam/test.jpg");
        when(resp.getWriter()).thenReturn(printWriter);

    }

    @Test
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
                testSet.add("test");
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
