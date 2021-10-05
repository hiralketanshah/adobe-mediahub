package com.mediahub.core.servlets;

import com.adobe.granite.ui.components.Config;
import com.adobe.granite.ui.components.rendercondition.RenderCondition;
import com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import javax.jcr.RepositoryException;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(AemContextExtension.class)
public class ExcludeGroupRenderConditionServletTest {

    @InjectMocks
    @Spy
    private ExcludeGroupRenderConditionServlet fixture;

    public AemContext context = new AemContext();

    @Mock
    User user;

    @Mock
    SlingHttpServletRequest request;

    @Mock
    ResourceResolver resourceResolver;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    Config config;

    @Mock
    UserManager userManager;

    @Mock
    Group group;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
    MockSlingHttpServletResponse response;
    Group userGroup;

    @BeforeEach
    public void setupMock() throws RepositoryException {
        MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        response = context.response();
        final List<Group> groups = new ArrayList<>();
        userGroup = mock(Group.class);
        groups.add(userGroup);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(user.memberOf()).thenReturn(groups.iterator());
    }

    @Test
    void doGet(AemContext context)
            throws ServletException, IOException, RepositoryException, LoginException {
        List<Group> groups = new ArrayList<>();
        groups.add(group);

        Iterator<Group> currentUserGroups = groups.iterator();

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        when(request.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getUserID()).thenReturn("admin");
        when(resourceResolver.adaptTo(User.class)).thenReturn(user);
        when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        when(userManager.getAuthorizable("administrators")).thenReturn(group);

        when(user.memberOf()).thenReturn(currentUserGroups);
        Resource resource = context.resourceResolver().getResource("/content/sample/en");
        when(request.getResource()).thenReturn(resource);
        SimpleRenderCondition show = new SimpleRenderCondition(true);
        when(request.getAttribute(RenderCondition.class.getName())).thenReturn(show);
        doReturn(new String[]{"administrators"}).when(fixture).getPropertiesFromConfig(request);
        fixture.doGet(request, response);
        assertEquals(show, request.getAttribute(RenderCondition.class.getName()));
    }

}
