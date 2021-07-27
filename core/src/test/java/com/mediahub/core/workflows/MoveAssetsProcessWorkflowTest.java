package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jcr.Node;
import javax.jcr.Session;
import javax.jcr.Workspace;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.AccessControlPolicy;
import javax.jcr.security.Privilege;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import mockit.MockUp;

@ExtendWith({ AemContextExtension.class })
public class MoveAssetsProcessWorkflowTest {
    @InjectMocks
    MoveAssetsProcessWorkflow workflowProcess = new MoveAssetsProcessWorkflow();

    @Mock
    MetaDataMap metadataMap;

    @Mock
    WorkflowSession workflowSession;

    @Mock
    WorkItem workItem;

    @Mock
    WorkflowData workflowData;

    @Mock
    Workflow workflow;

    @Mock
    Resource resource;

    @Mock
    Resource parentResource;

    @Mock
    Iterator<Resource> iterator;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    Session session;

    @Mock
    Workspace workspace;

    @Mock
    Payload payload;

    @Mock
    WorkflowSession wfSession;

    @Mock
    Set<String> set;

    @Mock
    Node node;

    @Mock
    ValueMap valueMap;

    @Mock
    JackrabbitSession jackrabbitSession;

    @Mock
    private AccessControlManager accessControlManager;

    @Mock
    private JackrabbitAccessControlList jackrabbitAccessControlList;

    List<Principal> principalNameList;

    @Mock
    PrincipalManager principalManager;

    @Mock
    private AccessControlPolicy accessControlPolicy;

    @Mock
    Principal principal;

    @Mock
    ModifiableValueMap modifiableValueMap;

    @Mock
    Map<String, Object> folderProperties;

    private MockUp<AccessControlUtils> accessControlUtilsMockup;

    private MockUp<StringUtils> StringUtils;

    private MockUp<WorkflowUtils> workflowUtils;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(workItem.getWorkflowData()).thenReturn(workflowData);
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
        when(workflowData.getPayload()).thenReturn(payload);
        when(payload.toString()).thenReturn("/content/dam/projects/");
    }

    @Test
    public void execute() throws Exception {

        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);

        when(resource.getParent()).thenReturn(parentResource);
        when(parentResource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(parentResource);
        when(parentResource.getChild(BnpConstants.METADATA)).thenReturn(parentResource);
        when(parentResource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get("bnpp-media", Boolean.FALSE.toString())).thenReturn(Boolean.TRUE.toString());
        when(resource.getPath()).thenReturn("/content/dam/projects/");
        when(parentResource.getParent()).thenReturn(resource);// change
        when(parentResource.getPath()).thenReturn("/content/dam/projects/");
        when(resolver.getResource("/content/projects/")).thenReturn(resource);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
        when(resource.getChild(BnpConstants.METADATA)).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(resource.adaptTo(ModifiableValueMap.class)).thenReturn(modifiableValueMap);
        when(valueMap.get("project.path", "")).thenReturn("/content/dam/medialibrary");
        when(resolver.getResource("/content/dam/medialibrary")).thenReturn(resource);
        when(valueMap.containsKey("projectPath")).thenReturn(true);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(node.getSession()).thenReturn(session);
        doNothing().when(session).save();
        when(resource.getChild(BnpConstants.REP_POLICY)).thenReturn(resource);
        when(resolver.getResource("/content/dam/projects/" + "/" + BnpConstants.REP_POLICY)).thenReturn(resource);
        List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(resource.listChildren()).thenReturn(userList.iterator());
        when(jackrabbitSession.getPrincipalManager()).thenReturn(principalManager);
        principalNameList = new LinkedList<>();
        when(resource.getName()).thenReturn("allow");
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(BnpConstants.REP_PRINCIPAL_NAME, "")).thenReturn("testProject");
        StringUtils = new MockUp<StringUtils>() {

            @mockit.Mock
            boolean startsWith(String str, String prefix) {
                return true;
            }
        };

        StringUtils = new MockUp<StringUtils>() {

            @mockit.Mock
            boolean endsWith(String str, String prefix) {
                return true;
            }
        };
        when(resource.getChild("allow")).thenReturn(null);
        when(principalManager.getPrincipal("testProject")).thenReturn(principal);
        when(jackrabbitSession.getAccessControlManager()).thenReturn(accessControlManager);
        accessControlUtilsMockup = new MockUp<AccessControlUtils>() {

            @mockit.Mock
            JackrabbitAccessControlList getAccessControlList(Session session, String path) {
                return jackrabbitAccessControlList;
            }
        };

        Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };
        when(jackrabbitAccessControlList.addEntry(principal, privileges, true)).thenReturn(true);
        accessControlManager.setPolicy("/content/dam/projects/", accessControlPolicy);

        when(parentResource.getName()).thenReturn("testName");
        when(resource.getChild("testName")).thenReturn(null);
        folderProperties = new HashMap<>();
        when(parentResource.getValueMap()).thenReturn(valueMap);
        when(valueMap.get(JcrConstants.JCR_PRIMARYTYPE, BnpConstants.SLING_FOLDER)).thenReturn("sling:Folder");
        folderProperties.put(JcrConstants.JCR_PRIMARYTYPE, "sling:Folder");
        when(resolver.create(resource, "testName", folderProperties)).thenReturn(resource);

        when(parentResource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(parentResource);
        when(jackrabbitSession.getWorkspace()).thenReturn(workspace);
        when(parentResource.getPath()).thenReturn("/content/dam/projects");
        workflowUtils = new MockUp<WorkflowUtils>() {

            @mockit.Mock
            void updateWorkflowPayload(WorkItem item, WorkflowSession wfSession, String contentPath) {

            }
        };

        when(parentResource.hasChildren()).thenReturn(true);
        List<Resource> userList1 = new LinkedList<>();
        userList1.add(parentResource);
        when(parentResource.listChildren()).thenReturn(userList1.iterator());
        when(resolver.isLive()).thenReturn(true);

        workflowProcess.execute(workItem, workflowSession, metadataMap);
        assertEquals("JCR_PATH", workflowData.getPayloadType());
    }

    @Test
    public void execute1() throws Exception {
        when(resource.getPath()).thenReturn("/content/dam/projects/asset");
        // when(session.adaptTo(JackrabbitSession.class)).thenReturn(jackrabbitSession);

        when(resolver.getResource(anyString())).thenReturn(resource);
        List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(resource.listChildren()).thenReturn(userList.iterator());

        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.containsKey(JcrConstants.JCR_MIXINTYPES)).thenReturn(Boolean.FALSE);
        when(resource.adaptTo(Node.class)).thenReturn(node);
        when(node.getSession()).thenReturn(session);
        when(resource.getName()).thenReturn("allow");
        when(valueMap.get(BnpConstants.REP_PRINCIPAL_NAME, "")).thenReturn("projects-publisher");
        when(resource.getParent()).thenReturn(parentResource);
        when(jackrabbitSession.getPrincipalManager()).thenReturn(principalManager);
        when(jackrabbitSession.getAccessControlManager()).thenReturn(accessControlManager);
        accessControlUtilsMockup = new MockUp<AccessControlUtils>() {

            @mockit.Mock
            JackrabbitAccessControlList getAccessControlList(Session session, String path) {
                return jackrabbitAccessControlList;
            }
        };

        Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };
        when(jackrabbitAccessControlList.addEntry(principal, privileges, true)).thenReturn(true);
        accessControlManager.setPolicy("/content/dam/projects/", accessControlPolicy);
        when(resolver.create(resource, BnpConstants.REP_POLICY,
                Collections.singletonMap(JcrConstants.JCR_PRIMARYTYPE, BnpConstants.REP_ACL))).thenReturn(resource);
        workflowProcess.copyRepolicyNode(resolver, jackrabbitSession, resource, "/content/dam/projects");
    }

    @Test
    public void executeWithoutPayloadType() throws WorkflowException, LoginException {
        when(workflowData.getPayloadType()).thenReturn("TEST_PAYLOAD");

        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    @Test
    public void executeWithoutPayload() throws WorkflowException, LoginException {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(null);

        workflowProcess.execute(workItem, workflowSession, metadataMap);

    }

    @Test
    public void executeWithoutMedia() throws WorkflowException, LoginException {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(resolver.getResource("/content/dam/projects/")).thenReturn(resource);
        when(resource.getParent()).thenReturn(parentResource);
        when(resource.getPath()).thenReturn("/content/dam/projects/");

        workflowProcess.execute(workItem, workflowSession, metadataMap);

    }

    @AfterEach
    public void shouldTearDown() {
        if (accessControlUtilsMockup != null) {
            accessControlUtilsMockup.tearDown();
        }
        if (StringUtils != null) {
            StringUtils.tearDown();
        }
        if (workflowUtils != null) {
            workflowUtils.tearDown();
        }
    }

}
