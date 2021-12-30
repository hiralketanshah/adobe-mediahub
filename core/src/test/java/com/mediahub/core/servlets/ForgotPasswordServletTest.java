package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.InputStream;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.security.Principal;
import java.util.Calendar;
import java.util.Collections;
import java.util.Map;

import javax.jcr.Binary;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;

import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.JobManager;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class ForgotPasswordServletTest {

    @InjectMocks
    ForgotPasswordServlet forgotPasswordServlet;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    UserManager userManager;

    @Mock
    JobManager jobManager;

    @Mock
    Session session;

    @Mock
    PrintWriter printWriter;

    @Mock
    User user;

    @Mock
    RequestParameterMap requestParameterMap;

    @Mock
    Resource resource;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Principal pricipal;

    @Mock
    ValueMap valueMap;

    @Mock
    RequestParameter requestParameter;

    @Mock
    SlingSettingsService slingSettingsService;

    @Mock
    I18nProvider provider;

    @Mock
    Externalizer externalizer;

    @Mock
    GenericEmailNotification genericEmailNotification;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    Value val;

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
        val = new Value() {

            @Override
            public int getType() {
                return 0;
            }

            @Override
            public String getString() throws ValueFormatException, IllegalStateException, RepositoryException {
                return "test-firstname";
            }

            @Override
            public InputStream getStream() throws RepositoryException {
                return null;
            }

            @Override
            public long getLong() throws RepositoryException {
                return 0;
            }

            @Override
            public double getDouble() throws RepositoryException {
                return 0;
            }

            @Override
            public BigDecimal getDecimal() throws RepositoryException {
                return null;
            }

            @Override
            public Calendar getDate() throws RepositoryException {
                return null;
            }

            @Override
            public boolean getBoolean() throws RepositoryException {
                return false;
            }

            @Override
            public Binary getBinary() throws RepositoryException {
                return null;
            }
        };
    }

    @Test
    public void testDoPost() throws LoginException, RepositoryException {
        Value[] values = { val };
        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);

        when(req.getRequestParameterMap()).thenReturn(requestParameterMap);
        when(requestParameterMap.getValue("j_username")).thenReturn(requestParameter);
        when(requestParameter.getString()).thenReturn("admin");

        when(userManager.getAuthorizable("admin")).thenReturn(user);
        when(user.getPath()).thenReturn("/home/user/test");
        when(user.getProperty(anyString())).thenReturn(values);

        when(req.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        assertAll(() -> forgotPasswordServlet.doPost(req, resp));
    }

}
