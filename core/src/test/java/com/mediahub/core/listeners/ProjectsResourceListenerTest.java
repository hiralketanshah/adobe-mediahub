package com.mediahub.core.listeners;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.Workspace;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.ObservationManager;
import javax.jcr.security.AccessControlEntry;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.AccessControlPolicy;
import javax.jcr.security.Privilege;

import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChange.ChangeType;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.event.Event;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import mockit.MockUp;

@ExtendWith({ AemContextExtension.class })
public class ProjectsResourceListenerTest {
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
    private Session session;

    @Mock
    private ComponentContext componentContext;

    @Mock
    private Workspace workspace;

    @Mock
    private ObservationManager observationManager;

    @Mock
    private Node node;

    @Mock
    private EventIterator eventIterator;

    @Mock
    private JackrabbitSession jackrabbitSession;

    @Mock
    private Principal principal;

    @Mock
    private Property property;

    @Mock
    private AccessControlManager accessControlManager;

    @Mock
    private JackrabbitAccessControlList jackrabbitAccessControlList;

    @Mock
    private AccessControlUtils accessControlUtils;

    @Mock
    PrincipalManager principalManager;

    @Mock
    UserManager userManager;

    @Mock
    private AccessControlPolicy accessControlPolicy;

    @Mock
    private Privilege privilege;

    @Mock
    private Group group;

    @Mock
    ValueFactory vf;

    @Mock
    Value value;

    @Mock
    ValueMap valueMap;

    @Mock
    private ModifiableValueMap modifiableValueMap;

    @Mock
    private AccessControlEntry accessControlEntry;

    @Mock
    User user;

    private MockUp<AccessControlUtils> accessControlUtilsMockup;
    private MockUp<CreatePolicyNodeUtil> createPolicyNodeUtilMockup;

    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    Event event = new Event("event/topic", Collections.singletonMap(SlingConstants.PROPERTY_PATH, "/content/projects"));

    @BeforeEach
    void setup() throws Exception, LoginException {
        MockitoAnnotations.initMocks(this);
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
    }

    @Test
    public void onEventTest() throws Exception, RepositoryException {

        List<Authorizable> userList = new ArrayList<>();
        userList.add(user);

        AccessControlEntry entry = new AccessControlEntry() {

            @Override
            public Privilege[] getPrivileges() {
                // TODO Auto-generated method stub
                return null;
            }

            @Override
            public Principal getPrincipal() {
                // TODO Auto-generated method stub
                return new Principal() {

                    @Override
                    public String getName() {
                        // TODO Auto-generated method stub
                        return "Test Name";
                    }
                };
            }
        };

        AccessControlEntry[] entries = new AccessControlEntry[1];
        entries[0] = entry;

        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
        when(resolver.getResource(Mockito.any(String.class))).thenReturn(resource);
        when(resource.getResourceType()).thenReturn("cq/gui/components/projects/admin/card/projectcard");
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(resource.getChild("jcr:content")).thenReturn(resource);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("project.path", String.class)).thenReturn("Test Project Path");
        when(valueMap.get("damFolderPath", String.class)).thenReturn("Test DAM folder path");
        when(parentresource.adaptTo(Node.class)).thenReturn(node);
        when(jackrabbitSession.getPrincipalManager()).thenReturn(principalManager);
        when(jackrabbitSession.getUserManager()).thenReturn(userManager);
        when((node.getProperty(BnpConstants.ROLE_EDITOR))).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_OBSERVER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_OWNER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_PROJECTPUBLISHER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR)).thenReturn(property);
        when(node.hasProperty(BnpConstants.ROLE_EDITOR)).thenReturn(true);
        when(property.getString()).thenReturn("projects-bnpproject-editor");
        when(principalManager.getPrincipal("projects-bnpproject-editor")).thenReturn(principal);
        when(resource.getParent()).thenReturn(parentresource);
        when(parentresource.getPath()).thenReturn(BnpConstants.MEDIALIBRARY_PATH);
        when(userManager.getAuthorizable(Mockito.anyString())).thenReturn(group);
        when(group.getMembers()).thenReturn(userList.iterator());
        when(jackrabbitSession.getValueFactory()).thenReturn(vf);
        when(vf.createValue("")).thenReturn(value);

        when(node.hasNode(BnpConstants.REP_POLICY)).thenReturn(true);
        when(jackrabbitSession.getAccessControlManager()).thenReturn(accessControlManager);
        accessControlUtilsMockup = new MockUp<AccessControlUtils>() {

            @mockit.Mock
            JackrabbitAccessControlList getAccessControlList(Session session, String path) {
                return jackrabbitAccessControlList;
            }
        };

        createPolicyNodeUtilMockup = new MockUp<CreatePolicyNodeUtil>() {

            @mockit.Mock
            void createRepPolicyNodes(Session adminSession, String parentFolderPath, List<Principal> principalNameList,
                    Map<String, Value> restrictions) {
            }
        };

        when(jackrabbitAccessControlList.getAccessControlEntries()).thenReturn(entries);

        Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };
        when(jackrabbitAccessControlList.addEntry(principal, privileges, true)).thenReturn(true);
        accessControlManager.setPolicy(PARENT_PATH, accessControlPolicy);

        List<ResourceChange> changedResources = new ArrayList<>();
        ResourceChange change = new ResourceChange(ChangeType.ADDED, "/content/projects", true);
        changedResources.add(change);

        projectsResourceListener.onChange(changedResources);
        verify(accessControlManager).setPolicy(PARENT_PATH, accessControlPolicy);

    }

    @Test
    public void onEventTest1() throws Exception, RepositoryException {
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
        when(resolver.getResource("/content/projects")).thenReturn(resource);
        when(resource.getResourceType()).thenReturn("cq/gui/components/projects/admin/card/projectcard");
        when(resolver.getResource(PROJECT_PATH)).thenReturn(resource);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(parentresource.adaptTo(Node.class)).thenReturn(node);
        when(jackrabbitSession.getPrincipalManager()).thenReturn(principalManager);
        when((node.getProperty(BnpConstants.ROLE_EDITOR))).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_OBSERVER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_OWNER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_PROJECTPUBLISHER)).thenReturn(property);
        when(node.getProperty(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR)).thenReturn(property);
        when(property.getString()).thenReturn("projects-bnpproject-editor");
        when(principalManager.getPrincipal("projects-bnpproject-editor")).thenReturn(principal);
        when(resource.getParent()).thenReturn(parentresource);

        when(node.hasNode(BnpConstants.REP_POLICY)).thenReturn(false);
        when(parentresource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(modifiableValueMap.put(BnpConstants.JCR_MIXINTYPES, BnpConstants.REP_ACCESSCONTROLLABLE))
                .thenReturn(BnpConstants.REP_ACCESSCONTROLLABLE);
        when(parentresource.getPath()).thenReturn(PROJECT_PATH);
        when(node.addNode(BnpConstants.REP_POLICY, BnpConstants.REP_ACL)).thenReturn(node);
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

        List<ResourceChange> changedResources = new ArrayList<>();
        ResourceChange change = new ResourceChange(ChangeType.ADDED, "/content/projects", true);
        changedResources.add(change);

        projectsResourceListener.onChange(changedResources);
        verify(accessControlManager).setPolicy(PARENT_PATH, accessControlPolicy);

    }

    @AfterEach
    public void shouldTearDown() {
        if (accessControlUtilsMockup != null) {
            accessControlUtilsMockup.tearDown();
        }
    }

}
