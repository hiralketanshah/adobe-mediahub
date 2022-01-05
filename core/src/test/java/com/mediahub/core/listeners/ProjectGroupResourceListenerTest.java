package com.mediahub.core.listeners;

import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.math.BigDecimal;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.jcr.Binary;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;
import javax.jcr.Workspace;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.ObservationManager;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.JobManager;
import org.apache.sling.jcr.api.SlingRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.osgi.service.component.ComponentContext;
import javax.jcr.observation.Event;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class ProjectGroupResourceListenerTest {

    @InjectMocks
    private ProjectGroupResourceListener projectsResourceListener = new ProjectGroupResourceListener();

    private AemContext context;

    private ComponentContext componentContext;

    @Mock
    private ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private Resource resource;

    @Mock
    private JobManager jobManager;

    @Mock
    private EventIterator eventIterator;

    @Mock
    private SlingRepository slingRepository;

    @Mock
    private Session session;

    @Mock
    private Workspace workspace;

    @Mock
    private ObservationManager observationManager;

    Value[] values;
    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() throws javax.jcr.LoginException, RepositoryException {
        MockitoAnnotations.initMocks(this);

        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
        when(slingRepository.loginService(BnpConstants.WRITE_SERVICE, null)).thenReturn(session);
        when(session.getWorkspace()).thenReturn(workspace);
        when(workspace.getObservationManager()).thenReturn(observationManager);
        projectsResourceListener.activate(componentContext);

        Value firstValue = new Value() {
            @Override
            public String toString() {
                return "value";
            }

            @Override
            public String getString() throws ValueFormatException, IllegalStateException, RepositoryException {
                return null;
            }

            @Override
            public InputStream getStream() throws RepositoryException {
                return null;
            }

            @Override
            public Binary getBinary() throws RepositoryException {
                return null;
            }

            @Override
            public long getLong() throws ValueFormatException, RepositoryException {
                return 0;
            }

            @Override
            public double getDouble() throws ValueFormatException, RepositoryException {
                return 0;
            }

            @Override
            public BigDecimal getDecimal() throws ValueFormatException, RepositoryException {
                return null;
            }

            @Override
            public Calendar getDate() throws ValueFormatException, RepositoryException {
                return null;
            }

            @Override
            public boolean getBoolean() throws ValueFormatException, RepositoryException {
                return false;
            }

            @Override
            public int getType() {
                return 0;
            }
        };
        values = new Value[] { firstValue };
    }

    @Test
    public void onEventTest() throws Exception, RepositoryException {

        Event resourceEvent = new Event() {

            @Override
            public String getUserID() {
                return "groupone";
            }

            @Override
            public String getUserData() throws RepositoryException {
                return null;
            }

            @Override
            public int getType() {
                return 0;
            }

            @Override
            public String getPath() throws RepositoryException {
                return "/home/group/mediahub/groupone";
            }

            @Override
            public Map getInfo() throws RepositoryException {
                Map<String, Value[]> infoMap = new HashMap<>();
                infoMap.put(BnpConstants.AFTER_VALUE, values);

                return infoMap;
            }

            @Override
            public String getIdentifier() throws RepositoryException {
                return null;
            }

            @Override
            public long getDate() throws RepositoryException {
                return 0;
            }
        };
        when(eventIterator.hasNext()).thenReturn(true, false);
        when(eventIterator.nextEvent()).thenReturn(resourceEvent);
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        projectsResourceListener.onEvent(eventIterator);
        projectsResourceListener.deactivate();

    }

}
