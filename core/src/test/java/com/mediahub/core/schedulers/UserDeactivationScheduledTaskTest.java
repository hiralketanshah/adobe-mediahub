package com.mediahub.core.schedulers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.day.cq.search.QueryBuilder;
import com.mediahub.core.constants.MediahubConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.org.lidalia.slf4jext.Level;
import uk.org.lidalia.slf4jtest.LoggingEvent;
import uk.org.lidalia.slf4jtest.TestLogger;
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
public class UserDeactivationScheduledTaskTest {

  private UserDeactivationScheduledTask fixture = new UserDeactivationScheduledTask();

  private TestLogger logger = TestLoggerFactory.getTestLogger(fixture.getClass());

  @Mock
  private ResourceResolver resolver;

  private AemContext context;

  @Mock
  ResourceResolverFactory resolverFactory;

  @BeforeEach
  void setup() {
    TestLoggerFactory.clear();
  }

  @Test
  void run() throws LoginException {
    UserDeactivationScheduledTask.Config config = mock(UserDeactivationScheduledTask.Config.class);
    when(config.getUserType()).thenReturn(MediahubConstants.EXTERNAL);
    when(config.scheduler_expression()).thenReturn("0 1 0 1/1 * ? *");
    when(config.scheduler_concurrent()).thenReturn(Boolean.FALSE);
    QueryBuilder queryBuilder = mock(QueryBuilder.class);

    fixture.resolverFactory = resolverFactory;
    when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
    when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);


    fixture.activate(config);
    fixture.run();

    List<LoggingEvent> events = logger.getLoggingEvents();
    assertEquals(2, events.size());
    LoggingEvent event = events.get(0);
    assertEquals(Level.DEBUG, event.getLevel());
    assertEquals(1, event.getArguments().size());
    assertEquals(MediahubConstants.EXTERNAL, event.getArguments().get(0));
    assertEquals("0 1 0 1/1 * ? *", config.scheduler_expression());
    assertEquals(Boolean.FALSE, config.scheduler_concurrent());



  }

  @Test
  void getQuery(){
    Map<String,String> predicateMap = fixture.getPredicateMap();
    assertEquals(MediahubConstants.HOME_USERS, predicateMap.get(MediahubConstants.PATH));
    assertEquals(MediahubConstants.REP_USERS, predicateMap.get(MediahubConstants.TYPE));
    assertEquals(MediahubConstants.PROFILE_TYPE, predicateMap.get(MediahubConstants.FIRST_PROPERTY));
  }

  @Test
  void deactivateExpiredUsers() throws ParseException, RepositoryException {
    UserManager userManager = mock(UserManager.class);
    Resource user = mock(Resource.class);
    String expiryDate = "2019/06/09";
    fixture.deactivateExpiredUsers(userManager, user, expiryDate);

    SimpleDateFormat sdf = new SimpleDateFormat(MediahubConstants.YYYY_MM_DD);
    Date date = sdf.parse(expiryDate);
    Calendar expiry = Calendar.getInstance();
    expiry.setTime(date);
    assertEquals(true,Calendar.getInstance().after(expiry));
  }

}
