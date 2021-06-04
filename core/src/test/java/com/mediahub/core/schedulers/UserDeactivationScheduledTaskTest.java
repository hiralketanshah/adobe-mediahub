package com.mediahub.core.schedulers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.UnsupportedRepositoryOperationException;

import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
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
import uk.org.lidalia.slf4jtest.TestLoggerFactory;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
@MockitoSettings(strictness = Strictness.LENIENT)
public class UserDeactivationScheduledTaskTest {

  @InjectMocks  
  private UserDeactivationScheduledTask fixture;
  
  @Mock
  GenericEmailNotification genericEmailNotification;

  @Mock
  private ResourceResolver resolver;

  @Mock
  ResourceResolverFactory resolverFactory;
  
  @Mock
  private SearchResult searchResult;
  
  @Mock
  ValueMap valueMap;

  @Mock
  private Query query;
  
  @Mock
  private Session session;
  
  @Mock
  Resource resource;
  
  @Mock
  User user;
  
  @Mock
  UserManager userManager;
  
  Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
          BnpConstants.WRITE_SERVICE);

  @BeforeEach
  void setup() {
    TestLoggerFactory.clear();
  }

  @Test
  void run() throws LoginException {
      try {
    UserDeactivationScheduledTask.Config config = mock(UserDeactivationScheduledTask.Config.class);
    when(config.getUserType()).thenReturn(BnpConstants.EXTERNAL);
    when(config.scheduler_expression()).thenReturn("0 1 0 1/1 * ? *");
    when(config.scheduler_concurrent()).thenReturn(Boolean.FALSE);
    QueryBuilder queryBuilder = mock(QueryBuilder.class);    
    when(resolverFactory.getServiceResourceResolver(any())).thenReturn(resolver);
    when(resolver.adaptTo(Session.class)).thenReturn(session);
    when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
    when(resolver.adaptTo(Session.class)).thenReturn(session);
    when(queryBuilder.createQuery(any(PredicateGroup.class), any(Session.class))).thenReturn(query);
    when(query.getResult()).thenReturn(searchResult);
    when(resolver.adaptTo(UserManager.class)).thenReturn(userManager);
    List<Resource> userList = new ArrayList<>();
    userList.add(resource);
    when(searchResult.getResources()).thenReturn(userList.iterator());
    when(resource.getChild(BnpConstants.PROFILE)).thenReturn(resource);
    when(resource.getValueMap()).thenReturn(valueMap);
    when(valueMap.get(BnpConstants.EXPIRY,String.class)).thenReturn("2019/06/09");
    when(resource.getPath()).thenReturn("/etc/home/user");
    when(userManager.getAuthorizableByPath("/etc/home/user")).thenReturn(user);
    when(user.isDisabled()).thenReturn(false);
    when(valueMap.get(BnpConstants.EMAIL,String.class)).thenReturn("MediaHub@gmail.com");
    when(valueMap.get(BnpConstants.FIRST_NAME,String.class)).thenReturn("TestUser");
    when(resolver.hasChanges()).thenReturn(true);
    fixture.activate(config);
    fixture.run();


    assertEquals("0 1 0 1/1 * ? *", config.scheduler_expression());
    assertEquals(Boolean.FALSE, config.scheduler_concurrent());


    } catch (UnsupportedRepositoryOperationException e) {
        e.printStackTrace();
    } catch (RepositoryException e) {
        e.printStackTrace();
    }
  }

}
