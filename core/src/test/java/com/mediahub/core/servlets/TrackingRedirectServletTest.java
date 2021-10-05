package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(AemContextExtension.class)
public class TrackingRedirectServletTest {

    @InjectMocks
    private TrackingRedirectServlet fixture;

    public AemContext context = new AemContext();

    @Mock
    SlingHttpServletRequest request;

    @Mock
    ResourceResolver resourceResolver;

    Resource res;
    Resource resWithoutRedirect;
    MockSlingHttpServletResponse response;

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
        res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset", "redirectTarget",
                "testTarget");
        resWithoutRedirect = context.create().resource("/content/test2", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
        response = context.response();
    }

    @Test
    void doGetWithRedirect(AemContext context) throws IOException {
        when(request.getResource()).thenReturn(res);
        fixture.doGet(request, response);
        assertEquals(302, response.getStatus());
    }

    @Test
    void doGetWithoutRedirect(AemContext context) throws IOException {
        when(request.getResource()).thenReturn(resWithoutRedirect);
        fixture.doGet(request, response);
        assertEquals(200, response.getStatus());
    }
}
