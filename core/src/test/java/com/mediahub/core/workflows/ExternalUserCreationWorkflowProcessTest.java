package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectMember;
import com.adobe.cq.projects.api.ProjectMemberRole;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.jcr.Binary;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.ValueFormatException;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
@MockitoSettings(strictness = Strictness.LENIENT)
public class ExternalUserCreationWorkflowProcessTest {
    private static final String PATH = "/content/projects/corporate_institutionalbankingcib";

    @Mock
    WorkItem workItem;

    @Mock
    GenericEmailNotification genericEmailNotification;

    @Mock
    WorkflowSession wfsession;

    @Mock
    WorkflowData workflowData;

    @Mock
    ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    Session session;

    @Mock
    Resource resource;

    @Mock
    JackrabbitSession jackrabbitSession;

    @Mock
    Workflow workflow;

    @Mock
    UserManager userManager;

    @Mock
    ValueFactory valueFactory;

    @Mock
    User user;

    @Mock
    Value value;

    @Mock
    private Value value2;

    @Mock
    private Value value3;

    @Mock
    private Externalizer externalizer;

    @Mock
    Group group;

    @Mock
    Project project;

    @Mock
    ProjectMember projectMember;

    @Mock
    Set<ProjectMember> setOfProjectMember;

    @Mock
    ProjectMemberRole projectMemberRole;

    @Mock
    Set<ProjectMemberRole> setOfProjectMemberRole;

    @Mock
    Collection<ProjectMember> collectionOfProjectMember;

    MetaDataMap metadataMap;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @InjectMocks
    ExternalUserCreationWorkflowProcess workflowProcess = new ExternalUserCreationWorkflowProcess();

    @BeforeEach
    public void setUp() throws Exception {
        metadataMap = new SimpleMetaDataMap();
        metadataMap.put("firstName", "Test First Name");
        metadataMap.put("lastName", "Test First Name");
        metadataMap.put("email", "test@gmail.com");
        metadataMap.put("expiryDate", "2020-10-11");
        metadataMap.put("project", "Mediahub");
        metadataMap.put("company", "Bnp Paribas");
        metadataMap.put("city", "Bangalore");
        metadataMap.put("country", "India");
        when(workItem.getWorkflowData()).thenReturn(workflowData);

    }

    @SuppressWarnings("unchecked")
    @Test
    public void execute() throws Exception {
        when(workflowData.getPayload()).thenReturn(PATH);
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);

        when(workItem.getWorkflow()).thenReturn(workflow);

        ValueMap map = mock(ValueMap.class);
        when(resource.getValueMap()).thenReturn(map);
        when(map.get(any())).thenReturn("");
        when(workflow.getMetaDataMap()).thenReturn(metadataMap);
        when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
        when(jackrabbitSession.getUserManager()).thenReturn(userManager);
        when(jackrabbitSession.getValueFactory()).thenReturn(valueFactory);
        when(userManager.createUser(any(String.class), any(String.class))).thenReturn(user);
        when(valueFactory.createValue(any(String.class), any(int.class))).thenReturn(value);
        when(userManager.getAuthorizable(BnpConstants.BASIC_GROUP)).thenReturn(group);
        when(group.addMember(user)).thenReturn(true);
        when(resolver.getResource("Mediahub")).thenReturn(resource);
        when(resource.adaptTo(Project.class)).thenReturn(project);
        setOfProjectMember = new LinkedHashSet<>();
        setOfProjectMemberRole = new HashSet<>();
        setOfProjectMember.add(projectMember);
        setOfProjectMemberRole.add(projectMemberRole);
        when(project.getMembers()).thenReturn(setOfProjectMember);
        when(projectMember.getId()).thenReturn("MemberId");
        when(projectMember.getRoles()).thenReturn(setOfProjectMemberRole);
        when(projectMemberRole.getId()).thenReturn("RoleID");
        when(value2.getString()).thenReturn("emp123");
        when(value3.getString()).thenReturn("2020-10-12");
        Value[] valueArray = { value2 };
        Value[] valueArray1 = { value3 };

        when(user.getProperty("./profile/givenName")).thenReturn(valueArray);
        when(value2.toString()).thenReturn("MediaUserName");
        when(project.getTitle()).thenReturn("Project Title");
        when(user.getProperty("./profile/expiry")).thenReturn(valueArray1);
        when(value3.toString()).thenReturn("2020-10-11");
        when(externalizer.authorLink(resolver, "/projects/details.html"
                + "/content/projects/corporate_institutionalbankingcib".replace("/dam", ""))).thenReturn(
                        "http://localhost:4502/projects/details.html/content/projects/corporate_institutionalbankingcib");
        when(workItem.getWorkflow()).thenReturn(workflow);
        when(workflow.getInitiator()).thenReturn("InitiatorUser");
        workflowProcess.execute(workItem, wfsession, metadataMap);
        verify(genericEmailNotification).sendEmail(any(String.class), any(String[].class), any(Map.class));
    }

    @Test
    public void setExpiryDateExistingUser() throws ParseException, RepositoryException {
        when(userManager.getAuthorizable("test@gmail.com")).thenReturn(user);
        Value firstValue = new Value() {
            public String toString() {
                return "2020-10-11";
            }

            @Override
            public String getString() throws ValueFormatException, IllegalStateException, RepositoryException {
                return "2020-10-11";
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
        Value[] strings = new Value[] { firstValue };
        when(user.getProperty("./profile/expiry")).thenReturn(strings);
        when(value.toString()).thenReturn("2020-10-11");

        User testUser = workflowProcess.setExpiryDateExistingUser("test@gmail.com", "2020-10-11", userManager,
                valueFactory);
        assertEquals("2020-10-11", testUser.getProperty("./profile/expiry")[0].toString());
    }

}
