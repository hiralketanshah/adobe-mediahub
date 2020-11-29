package com.mediahub.core.listeners;

import java.security.Principal;
import java.util.Collections;
import java.util.Map;
import static org.mockito.Mockito.*;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Workspace;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.ObservationManager;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.AccessControlPolicy;
import javax.jcr.security.Privilege;

import static org.mockito.Mockito.when;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.osgi.service.component.ComponentContext;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.constants.MediahubConstants;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import static org.mockito.Mockito.verify;
import mockit.MockUp;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
public class ProjectsResourceListenerTest {
    private static final String PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject/jcr:content";
    private static final String PROJECT_PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject";
    private static final String PARENT_PATH = "/content/projects/bnpfolder1/bnpfolder2";

    @InjectMocks
    private ProjectsResourceListener projectsResourceListener = new ProjectsResourceListener();

    private AemContext context;

    @Mock
    private ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private Resource resource;

    @Mock
    private Resource parentresource;

    @Mock
    Session session;

    @Mock
    ComponentContext componentContext;

    @Mock
    Workspace workspace;

    @Mock
    ObservationManager observationManager;

    @Mock
    Event event;

    @Mock
    Node node;

    @Mock
    EventIterator eventIterator;

    @Mock
    JackrabbitSession jackrabbitSession;

    @Mock
    Principal principal;

    @Mock
    Property property;

    @Mock
    AccessControlManager accessControlManager;

    @Mock
    JackrabbitAccessControlList jackrabbitAccessControlList;

    @Mock
    AccessControlUtils accessControlUtils;

    @Mock
    PrincipalManager principalManager;

    @Mock
    AccessControlPolicy accessControlPolicy;

    @Mock
    Privilege privilege;

    @Mock
    ModifiableValueMap modifiableValueMap;

    String[] nodeTypes = { MediahubConstants.NT_NODE_TYPE };

    private MockUp<AccessControlUtils> accessControlUtilsMockup;

    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setup() throws Exception, LoginException {
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
    }

    @Test
    public void activateTest() throws Exception {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(session.getWorkspace()).thenReturn(workspace);
        when(workspace.getObservationManager()).thenReturn(observationManager);
        projectsResourceListener.activate(componentContext);

        verify(observationManager).addEventListener(projectsResourceListener, Event.NODE_ADDED,
                MediahubConstants.AEM_PROJECTS_PATH, true, null, nodeTypes, false);
    }


    public void onEventTest() throws Exception, RepositoryException {
        onEventTest2();
        when(node.hasNode(MediahubConstants.REP_POLICY)).thenReturn(true);
        when(jackrabbitSession.getAccessControlManager()).thenReturn(accessControlManager);
        accessControlUtilsMockup = new MockUp<AccessControlUtils>() {
            @mockit.Mock
            JackrabbitAccessControlList getAccessControlList(Session session, String path) {
                return jackrabbitAccessControlList;
            }
        };

        Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };
        when(jackrabbitAccessControlList.addEntry(principal, privileges, true)).thenReturn(true);
        accessControlManager.setPolicy(PARENT_PATH, accessControlPolicy);
        projectsResourceListener.onEvent(eventIterator);

        verify(accessControlManager).setPolicy(PARENT_PATH, accessControlPolicy);

    }

    
    public void onEventTest1() throws Exception, RepositoryException {
        onEventTest2();
        when(node.hasNode(MediahubConstants.REP_POLICY)).thenReturn(false);
        when(parentresource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(modifiableValueMap.put(MediahubConstants.JCR_MIXINTYPES, MediahubConstants.REP_ACCESSCONTROLLABLE))
                .thenReturn(MediahubConstants.REP_ACCESSCONTROLLABLE);
        when(parentresource.getPath()).thenReturn(PROJECT_PATH);
        when(node.addNode(MediahubConstants.REP_POLICY, MediahubConstants.REP_ACL)).thenReturn(node);
        when(jackrabbitSession.getAccessControlManager()).thenReturn(accessControlManager);
        accessControlUtilsMockup = new MockUp<AccessControlUtils>() {
            @mockit.Mock
            JackrabbitAccessControlList getAccessControlList(Session session, String path) {
                return jackrabbitAccessControlList;
            }
        };

        Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };
        when(jackrabbitAccessControlList.addEntry(principal, privileges, true)).thenReturn(true);
        accessControlManager.setPolicy(PARENT_PATH, accessControlPolicy);
        projectsResourceListener.onEvent(eventIterator);

        verify(accessControlManager).setPolicy(PARENT_PATH, accessControlPolicy);

    }

    public void onEventTest2() throws Exception, RepositoryException {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
        when(eventIterator.nextEvent()).thenReturn(event);
        when(event.getPath()).thenReturn(PATH);
        when(resolver.getResource(PROJECT_PATH)).thenReturn(resource);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(parentresource.adaptTo(Node.class)).thenReturn(node);
        when(jackrabbitSession.getPrincipalManager()).thenReturn(principalManager);
        when((node.getProperty(MediahubConstants.ROLE_EDITOR))).thenReturn(property);
        when(node.getProperty(MediahubConstants.ROLE_OBSERVER)).thenReturn(property);
        when(node.getProperty(MediahubConstants.ROLE_OWNER)).thenReturn(property);
        when(node.getProperty(MediahubConstants.ROLE_PROJECTPUBLISHER)).thenReturn(property);
        when(node.getProperty(MediahubConstants.ROLE_EXTERNALCONTRIBUTEUR)).thenReturn(property);
        when(property.getString()).thenReturn("projects-bnpproject-editor");
        when(principalManager.getPrincipal("projects-bnpproject-editor")).thenReturn(principal);
        when(resource.getParent()).thenReturn(parentresource);
        when(parentresource.getName()).thenReturn("content1");
    }

    @AfterEach
    public void shouldTearDown() {
        if (accessControlUtilsMockup != null) {
            accessControlUtilsMockup.tearDown();
        }
    }

}
