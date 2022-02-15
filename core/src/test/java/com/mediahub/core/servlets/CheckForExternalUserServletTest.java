package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;

import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ResourceResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class CheckForExternalUserServletTest {

    @InjectMocks
    CheckForExternalUserServlet checkForExternalUserServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    UserManager userManager;

    @Mock
    Session session;

    @Mock
    PrintWriter printWriter;

    @Mock
    User user;

    @Mock
    Group group;

    @Mock
    Value value;

    Value[] values;

    @BeforeEach
    public void setupMock() throws RepositoryException, IOException {
        MockitoAnnotations.initMocks(this);
        values = new Value[] { value };

        Mockito.when(req.getResourceResolver()).thenReturn(resourceResolver);
        Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);
        when(session.getUserID()).thenReturn("admin");
        when(userManager.getAuthorizable("admin")).thenReturn(user);
        when(user.getProperty(Mockito.anyString())).thenReturn(values);
        when(resp.getWriter()).thenReturn(printWriter);
    }

    @Test
    public void testDoGetInternal() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(value.getString()).thenReturn("internal");
        assertAll(() -> checkForExternalUserServlet.doGet(req, resp));
    }
    
    @Test
    public void testDoGetExternal() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(value.getString()).thenReturn("external");
        assertAll(() -> checkForExternalUserServlet.doGet(req, resp));
    }
}
