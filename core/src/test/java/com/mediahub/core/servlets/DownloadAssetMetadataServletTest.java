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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.request.RequestParameterMap;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
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

@ExtendWith(AemContextExtension.class)
public class DownloadAssetMetadataServletTest {

    @InjectMocks
    DownloadAssetMetadataServlet downloadAssetMetadataServlet;

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
    Session session;

    @Mock
    PrintWriter printWriter;

    @Mock
    User user;

    @Mock
    Group group;

    @Mock
    RequestParameterMap requestParameterMap;

    @Mock
    Resource resource;

    @Mock
    ModifiableValueMap modifiableValueMap;

    RequestParameter requestParameter;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setupMock() throws IOException, RepositoryException, LoginException {
        requestParameter = getRequestParameter();
        MockitoAnnotations.initMocks(this);

        List<Group> groups = new ArrayList<>();
        groups.add(group);
        Iterator<Group> iterator = groups.iterator();

        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resourceResolver);
        Mockito.when(resourceResolver.adaptTo(UserManager.class)).thenReturn(userManager);
        Mockito.when(resourceResolver.adaptTo(Session.class)).thenReturn(session);
        when(session.getUserID()).thenReturn("admin");
        when(userManager.getAuthorizable("admin")).thenReturn(user);
        when(user.memberOf()).thenReturn(iterator);
        when(group.getID()).thenReturn("administrators");
        when(req.getResourceResolver()).thenReturn(resourceResolver);
        when(resourceResolver.getUserID()).thenReturn("admin");

        when(req.getRequestParameterMap()).thenReturn(requestParameterMap);
        when(requestParameterMap.getValues(BnpConstants.GEOGRAPHICALAREA)).thenReturn(new RequestParameter[] {});
        when(requestParameterMap.getOrDefault(BnpConstants.PATH, new RequestParameter[] {}))
                .thenReturn(new RequestParameter[] { requestParameter });
        when(req.getRequestParameter("_charset_")).thenReturn(requestParameter);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
        when(resource.getChild(BnpConstants.METADATA)).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(resourceResolver.getResource(anyString())).thenReturn(resource);
        when(modifiableValueMap.get(BnpConstants.DOWNLOAD_COUNT, 0)).thenReturn(0);
        doNothing().when(resourceResolver).commit();
        // doNothing().when(modifiableValueMap).put(BnpConstants.DOWNLOAD_COUNT, 1);
        when(resp.getWriter()).thenReturn(printWriter);
    }

    @Test
    public void testDoPost() throws Exception {

        assertAll(() -> downloadAssetMetadataServlet.doPost(req, resp));
    }

    @Test
    public void testDoPostOne() throws Exception {
        when(req.getRequestParameter("dateOfUse")).thenReturn(requestParameter);
        when(modifiableValueMap.get(BnpConstants.DOWNLOAD_DETAILS, new String[]{})).thenReturn(new String[] {});
        when(modifiableValueMap.get(Mockito.anyString())).thenReturn(new String[] {});
        when(modifiableValueMap.containsKey(Mockito.anyString())).thenReturn(true);
        assertAll(() -> downloadAssetMetadataServlet.doPost(req, resp));
    }
    
    @Test
    public void testDoPostError() throws Exception {
        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
        downloadAssetMetadataServlet.doPost(req, resp);
    }


    private RequestParameter getRequestParameter() {

        return new RequestParameter() {
            @Override
            public String getName() {
                return null;
            }

            @Override
            public boolean isFormField() {
                return false;
            }

            @Override
            public String getContentType() {
                return null;
            }

            @Override
            public long getSize() {
                return 0;
            }

            @Override
            public byte[] get() {
                return new byte[0];
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
            public String getString() {
                return "utf-8";
            }

            @Override
            public String getString(String s) throws UnsupportedEncodingException {
                return "2022-01-12 12:16";
            }
        };
    }
}
