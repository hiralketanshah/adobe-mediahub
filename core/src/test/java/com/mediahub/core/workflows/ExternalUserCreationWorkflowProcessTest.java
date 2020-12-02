package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Collections;
import java.util.Map;
import javax.jcr.Binary;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.ValueFormatException;
import org.apache.jackrabbit.api.JackrabbitSession;
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

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
@MockitoSettings(strictness = Strictness.LENIENT)
public class ExternalUserCreationWorkflowProcessTest {

  @Mock
  WorkItem workItem;

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

  MetaDataMap metadataMap;

  final Map<String, Object> authInfo = Collections
      .singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

  @InjectMocks
  ExternalUserCreationWorkflowProcess workflowProcess = new ExternalUserCreationWorkflowProcess();

  @BeforeEach
  public void setUp() throws Exception {
    metadataMap = new SimpleMetaDataMap();
    metadataMap.put("firstName","Test First Name");
    metadataMap.put("lastName","Test First Name");
    metadataMap.put("email","test@gmail.com");
    metadataMap.put("expiryDate","2020-10-11");
    metadataMap.put("project","Mediahub");
    metadataMap.put("company","Bnp Paribas");
    metadataMap.put("city","Bangalore");
    metadataMap.put("country","India");
    when(workItem.getWorkflowData()).thenReturn(workflowData);

  }

  @Test
  public void execute() throws Exception {
    when(workflowData.getPayload()).thenReturn("/content/projects/corporate_institutionalbankingcib");
    workflowProcess.resourceResolverFactory = resourceResolverFactory;

    when(workItem.getWorkflow()).thenReturn(workflow);

    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);
    when(map.get(any())).thenReturn("");
    when(workflow.getMetaDataMap()).thenReturn(metadataMap);
    when(resolver.adaptTo(Session.class)).thenReturn(jackrabbitSession);
    when(jackrabbitSession.getUserManager()).thenReturn(userManager);
    when(session.getValueFactory()).thenReturn(valueFactory);

    workflowProcess.execute(workItem, wfsession, metadataMap);
  }

  @Test
  public void setExpiryDateExistingUser() throws ParseException, RepositoryException {
    when(userManager.getAuthorizable("test@gmail.com")).thenReturn(user);
    Value firstValue = new Value(){
      public String toString() {
        return "2020-10-11";
      }

      @Override
      public String getString()
          throws ValueFormatException, IllegalStateException, RepositoryException {
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
    Value[] strings = new Value[]{firstValue};
    when(user.getProperty("./profile/expiry")).thenReturn(strings);
    when(value.toString()).thenReturn("2020-10-11");

    User testUser = workflowProcess.setExpiryDateExistingUser("test@gmail.com","2020-10-11", userManager, valueFactory);
    assertEquals("2020-10-11", testUser.getProperty("./profile/expiry")[0].toString());
  }

}
