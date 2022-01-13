package com.mediahub.core.services.impl;

import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.Cookie;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.component.ComponentContext;

import com.day.cq.wcm.api.Page;
import com.day.cq.wcm.api.PageManager;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class SystemNotificationsImplTest {

    private final AemContext context = new AemContext();

    @InjectMocks
    SystemNotificationsImpl systemNotificationsImpl;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    Resource resource;

    @Mock
    ResourceResolver resolver;

    @Mock
    ValueMap valueMap;

    @Mock
    PageManager pageManager;

    @Mock
    Page page;

    @Mock
    Calendar calendar;

    @Mock
    ComponentContext cx;

    @Mock
    SlingSettingsService settingsService;

    @Mock
    BundleContext bundleContext;

    @Mock
    Cookie cookie;

    @Mock
    ServiceRegistration<ResourceChangeListener> serviceRegistration;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            (Object) "system-notifications");

    @BeforeEach
    public void setupMock() throws IOException, LoginException {
        MockitoAnnotations.initMocks(this);
        Cookie[] cookies = new Cookie[] { cookie };
        context.registerService(SlingSettingsService.class, settingsService);
        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        Set<String> runModes = new HashSet<>();
        runModes.add("author");
        when(settingsService.getRunModes()).thenReturn(runModes);
        when(cx.getBundleContext()).thenReturn(bundleContext);
        List<Resource> listOfResource = new ArrayList<>();
        when(req.getRequestURI()).thenReturn("/etc/acs-commons/test");
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("onTime", Calendar.class)).thenReturn(Calendar.getInstance());
        when(valueMap.get("offTime", Calendar.class)).thenReturn(Calendar.getInstance());
        when(valueMap.get("enabled", false)).thenReturn(true);
        when(valueMap.get(Mockito.anyString())).thenReturn("cq:Page");
        when(resolver.adaptTo(PageManager.class)).thenReturn(pageManager);
        when(pageManager.getContainingPage(resource)).thenReturn(page);
        when(page.getProperties()).thenReturn(valueMap);
        when(page.getLastModified()).thenReturn(Calendar.getInstance());
        when(req.getResource()).thenReturn(resource);
        when(resource.listChildren()).thenReturn(listOfResource.iterator());
        when(req.getResourceResolver()).thenReturn(resolver);
        when(req.getCookies()).thenReturn(cookies);
        when(cookie.getName()).thenReturn("acs-commons-system-notifications");
        when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
    }

    @Test
    public void testAccept() throws Exception {

        when(req.getResource()).thenReturn(resource);
        when(resource.getPath()).thenReturn("/etc/acs-commons/notifications");
        systemNotificationsImpl.accepts(req, resp);
    }

    @Test
    public void testAcceptOne() throws Exception {
        when(resource.getPath()).thenReturn("/etc/acs-commons/test");
        systemNotificationsImpl.activate(cx);
        systemNotificationsImpl.accepts(req, resp);
    }

    @Test
    public void testAcceptOnChange() throws Exception {
        List<ResourceChange> changes = new ArrayList<>();
        when(resource.getPath()).thenReturn("/etc/acs-commons/test");
        systemNotificationsImpl.onChange(changes);
    }

    @Test
    public void testInject() throws Exception {
        when(req.getContextPath()).thenReturn("testPath");
        systemNotificationsImpl.inject(req, resp, Mockito.mock(PrintWriter.class));
    }

    @Test
    public void testInjectIndex() throws Exception {
        systemNotificationsImpl.getInjectIndex("testString");
    }

    @Test
    public void testGetMessage() throws Exception {
        systemNotificationsImpl.getMessage("html:test", "testOnTime", "testOffTime");
    }

    @Test
    public void testGetMessageOne() throws Exception {
        systemNotificationsImpl.getMessage("", "testOnTime", "testOffTime");
    }

    @Test
    public void testGetMessageTwo() throws Exception {
        systemNotificationsImpl.getMessage("testMessage", "testOnTime", "testOffTime");
    }

    @Test
    public void testError() throws Exception {
        Mockito.when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenThrow(LoginException.class);
        when(pageManager.getContainingPage(resource)).thenReturn(null);
        systemNotificationsImpl.activate(cx);
        systemNotificationsImpl.accepts(req, resp);
    }
}
