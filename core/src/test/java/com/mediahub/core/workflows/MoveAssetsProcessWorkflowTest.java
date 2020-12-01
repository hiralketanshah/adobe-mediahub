package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.PayloadMap;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import javax.jcr.ItemExistsException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Workspace;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import com.adobe.granite.workflow.WorkflowSession;

@ExtendWith({AemContextExtension.class, MockitoExtension.class})
@MockitoSettings(strictness = Strictness.LENIENT)
public class MoveAssetsProcessWorkflowTest {

  MetaDataMap metadataMap;

  @Mock
  WorkflowSession workflowSession;

  @Mock
  WorkItem workItem;

  @Mock
  WorkflowData workflowData;

  @Mock
  Resource resource;

  @Mock
  Iterator<Resource> iterator;

  @Mock
  ResourceResolverFactory resolverFactory;

  @InjectMocks
  MoveAssetsProcessWorkflow workflowProcess = new MoveAssetsProcessWorkflow();

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

  final Map<String, Object> authInfo = Collections
      .singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

  @BeforeEach
  public void setUp() throws Exception {
    metadataMap = new SimpleMetaDataMap();
    metadataMap.put("","");
    when(workItem.getWorkflowData()).thenReturn(workflowData);
  }

  @Test
  public void executeWorkflowException() throws Exception {
    when(workflowData.getPayloadType()).thenReturn("dummy");
    Assertions.assertThrows(WorkflowException.class, () ->{
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

  @Test
  public void executeLoginException() throws Exception {
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenThrow(new LoginException());
    when(resolver.isLive()).thenReturn(Boolean.TRUE);
    doNothing().when(resolver).close();
    Assertions.assertThrows(WorkflowException.class, () ->{
      workflowProcess.execute(workItem, workflowSession, metadataMap);
    });
  }

  @Test
  public void execute() throws Exception {
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.adaptTo(Session.class)).thenReturn(session);
    when(resolver.isLive()).thenReturn(Boolean.TRUE);
    when(workItem.getWorkflowData()).thenReturn(workflowData);
    when(resolver.getResource(any())).thenReturn(resource);

    when(resource.getParent()).thenReturn(resource);
    when(resource.getChild(any())).thenReturn(null);


    when(workflowData.getPayload()).thenReturn(payload);
    when(resource.getPath()).thenReturn("/dam/projects/");
    doNothing().when(resolver).close();

    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);
    when(map.get(any(),any())).thenReturn("");
    when(workflowData.getPayload()).thenReturn(metadataMap);
    when(wfSession.newWorkflowData(any(),any())).thenReturn(workflowData);
    when(workflowData.getMetaDataMap()).thenReturn(metadataMap);

    when(workflowProcess.findMediaFolderPath(resolver,"")).thenReturn(resource);
    when(session.getWorkspace()).thenReturn(workspace);
    when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);
    assertEquals("JCR_PATH", workflowData.getPayloadType());
  }

  @Test
  public void isFolderEmpty() {
    when(resource.hasChildren()).thenReturn(Boolean.FALSE);
    assertEquals(Boolean.TRUE, workflowProcess.isFolderEmpty(resource));
  }

  @Test
  public void isFolderNotEmpty() {
    when(resource.hasChildren()).thenReturn(Boolean.TRUE);
    when(resource.listChildren()).thenReturn(iterator);
    when(iterator.hasNext()).thenReturn(Boolean.TRUE);
    when(iterator.next()).thenReturn(resource);
    when(resource.getName()).thenReturn(JcrConstants.NT_UNSTRUCTURED);
    assertEquals(Boolean.FALSE, workflowProcess.isFolderEmpty(resource));
  }

  @Test
  public void findMediaFolder() throws LoginException {
    String payloadPath = "";
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.getResource(any())).thenReturn(resource);
    when(resource.getParent()).thenReturn(resource);
    when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);
    assertEquals(null, workflowProcess.findMediaFolderPath(resolver, payloadPath));
  }

  @Test
  public void findMediaFolderWithBnppMedia() throws LoginException {
    String payloadPath = "";
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.getResource(any())).thenReturn(resource);
    when(resource.getParent()).thenReturn(resource);
    when(resource.getChild(any())).thenReturn(resource);

    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);
    when(map.get("bnpp-media", Boolean.FALSE.toString())).thenReturn(Boolean.TRUE.toString());

    assertEquals(resource, workflowProcess.findMediaFolderPath(resolver, payloadPath));
  }


  @Test
  public void moveProjectDamAsset() throws LoginException, RepositoryException, PersistenceException {
    String newPath = "/";
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.create(any(), any(), any())).thenReturn(resource);
    doNothing().when(resolver).commit();
    when(resource.getChild(any())).thenReturn(resource);
    when(session.getWorkspace()).thenReturn(workspace);
    doNothing().when(workspace).copy(any(),any());
    doNothing().when(session).move(any(),any());
    when(resource.getName()).thenReturn("");
    when(resource.getPath()).thenReturn("");
    String path = workflowProcess.moveProjectDamAsset(resolver, session, resource, resource, "", resource);
    assertEquals(newPath, path);
  }

}
