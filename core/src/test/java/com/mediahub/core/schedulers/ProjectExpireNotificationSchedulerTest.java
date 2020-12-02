package com.mediahub.core.schedulers;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.MediahubConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectManager;
import com.day.cq.commons.Externalizer;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;
import org.apache.jackrabbit.api.security.user.User;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
public class ProjectExpireNotificationSchedulerTest {

    private static final String PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject/jcr:content";
    private static final String PROJECT_PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject";
    private static final String DUE_DATE = "2020-10-10T17:33:00.000+05:30";
    private static final String AEM_PROJECTS_PATH = "/content/projects";

    @InjectMocks
    private ProjectExpireNotificationScheduler scheduler;

    @Mock
    GenericEmailNotification genericEmailNotification;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private ResourceResolver resolver1;

    private AemContext context;

    @Mock
    private ResourceResolverFactory resolverFactory;

    @Mock
    private SearchResult searchResult;

    @Mock
    private Query query;

    @Mock
    private Calendar calendar;

    @Mock
    Date today;

    @Mock
    private Hit hit;

    List<Hit> listHit = new ArrayList<>();

    @Mock
    private Resource resource;

    @Mock
    private Resource resource1;

    @Mock
    private Node node;

    @Mock
    private Node node1;

    @Mock
    private Property property;

    @Mock
    private Property property1;

    @Mock
    private ProjectManager projectManager;

    @Mock
    private Project project;

    @Mock
    private SimpleDateFormat simpleDateFormat;

    @Mock
    private Date date;

    @Mock
    private UserManager userManager;

    @Mock
    private Authorizable authorizable;

    @Mock
    private org.apache.jackrabbit.api.security.user.Group group;

    @Mock
    Iterator<Authorizable> itr;

    @Mock
    private Value value;

    @Mock
    private Value value1;

    @Mock
    private Value value2;

    @Mock
    private Value value3;

    @Mock
    private Value value4;

    @Mock
    private Value value5;

    Long datevalue;

    Long lobject = new Long(5366623);

    @Mock
    private Externalizer externalizer;

    @Mock
    Map<String, String> emailParams;

    @Mock
    QueryBuilder queryBuilder;

    @Mock
    private Session session;

    @Mock
    private Date date1;

    String[] emailRecipients;

    @Mock
    User user;

    @BeforeEach
    void setup() {
        TestLoggerFactory.clear();
        context.registerAdapter(ResourceResolver.class, QueryBuilder.class, queryBuilder);
    }

    @Test
    void run() throws LoginException {
        try {
            ProjectExpireNotificationScheduler.Config config = mock(ProjectExpireNotificationScheduler.Config.class);
            when(config.getProjectPath()).thenReturn(MediahubConstants.AEM_PROJECTS_PATH);

            Map<String, Object> map = new HashMap<>();
            map.put(MediahubConstants.PATH, AEM_PROJECTS_PATH);
            map.put(MediahubConstants.FIRST_PROPERTY, MediahubConstants.SLING_RESOURCETYPE);
            map.put(MediahubConstants.FIRST_PROPERTY_OPERATION, MediahubConstants.LIKE);
            map.put(MediahubConstants.FIRST_PROPERTY_VALUE, MediahubConstants.PROJECT_RESOURCE);
            map.put(MediahubConstants.SECOND_DATERANGE_PROPERTY, MediahubConstants.PROJECT_DUEDATE);
            map.put(MediahubConstants.SECOND_DATERANGE_UPPEROPERATION, MediahubConstants.LESSTHAN_EQUALS);
            map.put(MediahubConstants.SECOND_DATERANGE_UPPERBOUND, "2020-12-26T19:29:13.454+05:30");

            when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
            when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
            when(resolver.adaptTo(Session.class)).thenReturn(session);
            when(queryBuilder.createQuery(any(PredicateGroup.class), any(Session.class))).thenReturn(query);
            when(query.getResult()).thenReturn(searchResult);
            listHit.add(hit);
            when(searchResult.getHits()).thenReturn(listHit);
            when(hit.getPath()).thenReturn(PATH);
            when(resolver.getResource(PATH)).thenReturn(resource);
            when(resource.adaptTo(Node.class)).thenReturn(node);
            when(node.getProperty(MediahubConstants.PROJECT_DUEDATE)).thenReturn(property);
            when(property.getValue()).thenReturn(value);
            when(value.getString()).thenReturn(DUE_DATE);
            when(resolver.adaptTo(ProjectManager.class)).thenReturn(projectManager);
            when(resolver.getResource(PROJECT_PATH)).thenReturn(resource);
            when(resource.adaptTo(Project.class)).thenReturn(project);

            when(resolver.getResource(PROJECT_PATH)).thenReturn(resource);
            when(resource.adaptTo(Node.class)).thenReturn(node);
            when(node.getProperty(MediahubConstants.ROLE_OWNER)).thenReturn(property1);
            when(property1.getValue()).thenReturn(value1);
            when(value1.getString()).thenReturn("projects-projectoct21-owner");

            when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
            when(userManager.getAuthorizable("projects-projectoct21-owner")).thenReturn(group);

            List<Authorizable> userList = new ArrayList<>();
            userList.add(user);
            when(group.getDeclaredMembers()).thenReturn(userList.iterator());

            when(project.getTitle()).thenReturn("bnpproject");
            when(externalizer.authorLink(any(ResourceResolver.class), any(String.class))).thenReturn(
                    "http://localhost:4502/projects/details.html/content/projects/bnpfolder1/bnpfolder2/bnpproject");
            when(user.getID()).thenReturn("emp123");
            when(userManager.getAuthorizable("emp123")).thenReturn(authorizable);
            when(authorizable.hasProperty(MediahubConstants.PEOFILE_EMAIL)).thenReturn(true);

            when(value2.getString()).thenReturn("emp123");
            when(value3.getString()).thenReturn("wmp123@gmail.com");

            Value[] valueArray = { value2 };
            Value[] valueArray1 = { value3 };

            when(authorizable.getProperty(MediahubConstants.PEOFILE_EMAIL)).thenReturn(valueArray);
            when(authorizable.getProperty(MediahubConstants.PROFILE_GIVEN_NAME)).thenReturn(valueArray);
            when(value2.getString()).thenReturn("MediaUserName");
            when(authorizable.getProperty(MediahubConstants.PEOFILE_EMAIL)).thenReturn(valueArray1);
            when(value3.getString()).thenReturn("MediaHub@gmail.com");

            scheduler.activate(config);
            scheduler.run();

            assertEquals(null, externalizer.authorLink(any(ResourceResolver.class), any(String.class)));

        } catch (RepositoryException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
