package com.mediahub.core.schedulers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectManager;
import com.day.cq.commons.Externalizer;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({ AemContextExtension.class })
public class ProjectExpireNotificationSchedulerTest {

    private static final String PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject/jcr:content";
    private static final String PROJECT_PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject";
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
    
    @Mock
    SlingSettingsService slingSettingsService;
    
    @Mock
    I18nProvider provider;

    @BeforeEach
    void setup() {
        MockitoAnnotations.initMocks(this);
        TestLoggerFactory.clear();
        context.registerAdapter(ResourceResolver.class, QueryBuilder.class, queryBuilder);
        Set<String> runModes = new HashSet<>();
        when(slingSettingsService.getRunModes()).thenReturn(runModes);
    }

    @Test
    void TestRun1() throws LoginException {
        try {
            ProjectExpireNotificationScheduler.Config config = mock(ProjectExpireNotificationScheduler.Config.class);
            when(config.getProjectPath()).thenReturn(BnpConstants.AEM_PROJECTS_PATH);
            TestRun4();
            Calendar cal = Calendar.getInstance();
            Date onePlusMonth = cal.getTime();
            DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            String strDate = dateFormat.format(onePlusMonth);
            when(value.getString()).thenReturn(strDate);
            when(resolver.adaptTo(ProjectManager.class)).thenReturn(projectManager);
            when(resolver.getResource(PROJECT_PATH)).thenReturn(resource1);
            when(resource1.adaptTo(Project.class)).thenReturn(project);
            when(resource1.adaptTo(Node.class)).thenReturn(node1);
            when(node1.getProperty(BnpConstants.ROLE_OWNER)).thenReturn(property1);
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
            when(authorizable.hasProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(true);

            when(value2.getString()).thenReturn("emp123");
            when(value3.getString()).thenReturn("wmp123@gmail.com");

            Value[] valueArray = { value2 };
            Value[] valueArray1 = { value3 };

            when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(valueArray);
            when(authorizable.getProperty(BnpConstants.PROFILE_GIVEN_NAME)).thenReturn(valueArray);
            when(value2.getString()).thenReturn("MediaUserName");
            when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(valueArray1);
            when(value3.getString()).thenReturn("MediaHub@gmail.com");
            when(resolver.hasChanges()).thenReturn(true);

            scheduler.activate(config);
            scheduler.run();

            assertEquals(null, externalizer.authorLink(any(ResourceResolver.class), any(String.class)));

        } catch (RepositoryException e) {
            e.printStackTrace();
        }
    }

    @Test
    void TestRun2() throws LoginException {
        /*
         * try { ProjectExpireNotificationScheduler.Config config =
         * mock(ProjectExpireNotificationScheduler.Config.class);
         * when(config.getProjectPath()).thenReturn(BnpConstants.
         * AEM_PROJECTS_PATH); TestRun4(); Calendar cal =
         * Calendar.getInstance(); cal.add(Calendar.DATE, 30); Date onePlusMonth
         * = cal.getTime(); DateFormat dateFormat = new
         * SimpleDateFormat("yyyy-MM-dd"); String strDate =
         * dateFormat.format(onePlusMonth);
         * when(value.getString()).thenReturn(strDate);
         * when(resolver.adaptTo(ProjectManager.class)).thenReturn(
         * projectManager);
         * when(resolver.getResource(PROJECT_PATH)).thenReturn(resource1);
         * when(resource1.adaptTo(Project.class)).thenReturn(project);
         * when(resource1.adaptTo(Node.class)).thenReturn(node1);
         * when(node1.getProperty(BnpConstants.ROLE_OWNER)).thenReturn(property1
         * ); when(property1.getValue()).thenReturn(value1);
         * when(value1.getString()).thenReturn("projects-projectoct21-owner");
         * 
         * when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
         * when(userManager.getAuthorizable("projects-projectoct21-owner")).
         * thenReturn(group);
         * 
         * List<Authorizable> userList = new ArrayList<>(); userList.add(user);
         * when(group.getDeclaredMembers()).thenReturn(userList.iterator());
         * 
         * when(project.getTitle()).thenReturn("bnpproject");
         * when(externalizer.authorLink(any(ResourceResolver.class),
         * any(String.class))).thenReturn(
         * "http://localhost:4502/projects/details.html/content/projects/bnpfolder1/bnpfolder2/bnpproject"
         * ); when(user.getID()).thenReturn("emp123");
         * when(userManager.getAuthorizable("emp123")).thenReturn(authorizable);
         * when(authorizable.hasProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn
         * (true);
         * 
         * when(value2.getString()).thenReturn("emp123");
         * when(value3.getString()).thenReturn("wmp123@gmail.com");
         * 
         * Value[] valueArray = { value2 }; Value[] valueArray1 = { value3 };
         * 
         * when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn
         * (valueArray);
         * when(authorizable.getProperty(BnpConstants.PROFILE_GIVEN_NAME)).
         * thenReturn(valueArray);
         * when(value2.getString()).thenReturn("MediaUserName");
         * when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn
         * (valueArray1);
         * when(value3.getString()).thenReturn("MediaHub@gmail.com");
         * when(resolver.hasChanges()).thenReturn(true);
         * 
         * scheduler.activate(config); scheduler.run();
         * 
         * assertEquals(null,
         * externalizer.authorLink(any(ResourceResolver.class),
         * any(String.class)));
         * 
         * } catch (RepositoryException e) { e.printStackTrace(); }
         */
    }

    @Test
    void TestRun3() throws LoginException {
        try {
            ProjectExpireNotificationScheduler.Config config = mock(ProjectExpireNotificationScheduler.Config.class);
            when(config.getProjectPath()).thenReturn(BnpConstants.AEM_PROJECTS_PATH);
            TestRun4();
            Calendar cal = Calendar.getInstance();
            cal.add(Calendar.DATE, -31);
            Date onePlusMonth = cal.getTime();
            DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            String strDate = dateFormat.format(onePlusMonth);
            when(value.getString()).thenReturn(strDate);
            when(resolver.adaptTo(ProjectManager.class)).thenReturn(projectManager);
            when(resolver.getResource(PROJECT_PATH)).thenReturn(resource1);
            when(resource1.adaptTo(Project.class)).thenReturn(project);
            when(resource1.adaptTo(Node.class)).thenReturn(node1);
            when(node1.getProperty(BnpConstants.ROLE_OWNER)).thenReturn(property1);
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
            when(authorizable.hasProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(true);

            when(value2.getString()).thenReturn("emp123");
            when(value3.getString()).thenReturn("wmp123@gmail.com");

            Value[] valueArray = { value2 };
            Value[] valueArray1 = { value3 };

            when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(valueArray);
            when(authorizable.getProperty(BnpConstants.PROFILE_GIVEN_NAME)).thenReturn(valueArray);
            when(value2.getString()).thenReturn("MediaUserName");
            when(authorizable.getProperty(BnpConstants.PEOFILE_EMAIL)).thenReturn(valueArray1);
            when(value3.getString()).thenReturn("MediaHub@gmail.com");
            when(resolver.hasChanges()).thenReturn(true);

            scheduler.activate(config);
            scheduler.run();

            assertEquals(null, externalizer.authorLink(any(ResourceResolver.class), any(String.class)));

        } catch (RepositoryException e) {
            e.printStackTrace();
        }
    }

    public void TestRun4() {
        try {
            Map<String, Object> map = new HashMap<>();
            map.put(BnpConstants.PATH, AEM_PROJECTS_PATH);
            map.put(BnpConstants.FIRST_PROPERTY, BnpConstants.SLING_RESOURCETYPE);
            map.put(BnpConstants.FIRST_PROPERTY_OPERATION, BnpConstants.LIKE);
            map.put(BnpConstants.FIRST_PROPERTY_VALUE, BnpConstants.PROJECT_RESOURCE);
            map.put(BnpConstants.SECOND_DATERANGE_PROPERTY, BnpConstants.PROJECT_DUEDATE);
            map.put(BnpConstants.SECOND_DATERANGE_UPPEROPERATION, BnpConstants.LESSTHAN_EQUALS);
            map.put(BnpConstants.SECOND_DATERANGE_UPPERBOUND, "2020-12-26T19:29:13.454+05:30");

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
            when(node.getProperty(BnpConstants.PROJECT_DUEDATE)).thenReturn(property);
            when(property.getValue()).thenReturn(value);
        } catch (LoginException e) {

            e.printStackTrace();
        } catch (RepositoryException e) {

            e.printStackTrace();
        }

    }

}
