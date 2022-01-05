package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Session;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.JobManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class AcceptPrivcyPolicyServletTest {

    @InjectMocks
    AcceptPrivacyPolicyServlet acceptPrivacyPolicyServlet;

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
    User user;

    @Mock
    Resource resource;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Principal pricipal;

    @Mock
    ValueMap valueMap;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testDoPost() throws Exception {

        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);

        when(req.getUserPrincipal()).thenReturn(pricipal);
        when(userManager.getAuthorizable(pricipal)).thenReturn(user);
        when(user.getPath()).thenReturn("/home/user/test");

        when(req.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(modifiableValueMap.get(BnpConstants.WELCOME_EMAIL_SENT, Boolean.FALSE.toString())).thenReturn("false");

        when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);

        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.TYPE, StringUtils.EMPTY)).thenReturn(BnpConstants.BROADCAST_VALUE_INTERNAL);
        when(pricipal.getName()).thenReturn("test-principal");
        assertAll(() -> acceptPrivacyPolicyServlet.doPost(req, resp));
    }

}
