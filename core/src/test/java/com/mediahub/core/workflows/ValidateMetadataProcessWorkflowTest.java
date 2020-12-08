package com.mediahub.core.workflows;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import com.adobe.granite.workflow.model.WorkflowNode;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.junit.jupiter.api.Assertions;
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
public class ValidateMetadataProcessWorkflowTest {

  MetaDataMap metadataMap;

  @Mock
  WorkflowSession workflowSession;

  @Mock
  WorkItem workItem;

  @Mock
  WorkflowData workflowData;

  @Mock
  ResourceResolverFactory resolverFactory;

  @Mock
  ResourceResolver resolver;

  @Mock
  Resource resource;

  @InjectMocks
  ValidateMedataProcessWorkflow workflowProcess = new ValidateMedataProcessWorkflow();

  @Mock
  ValidateMedataProcessWorkflow workflowProcessMock;

  @Mock
  QueryBuilder queryBuilder;

  @Mock
  Session session;

  @Mock
  Query query;

  @Mock
  SearchResult result;

  @Mock
  Iterator<Resource> requiredFields;

  @Mock
  Payload payload;

  @Mock
  WorkflowNode node;

  final Map<String, Object> authInfo = Collections
      .singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

  @BeforeEach
  public void setUp() throws Exception {
    metadataMap = new SimpleMetaDataMap();
    when(workItem.getWorkflowData()).thenReturn(workflowData);
  }

  @Test
  public void execute() throws Exception {

    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.isLive()).thenReturn(Boolean.TRUE);

    when(workflowData.getPayload()).thenReturn(payload);
    when(resource.getPath()).thenReturn("/dam/projects/");
    when(resolver.getResource(any())).thenReturn(resource);

    when(resource.getResourceType()).thenReturn(BnpConstants.DAM_ASSET);
    when(resource.getParent()).thenReturn(resource);
    when(resource.getChild(any())).thenReturn(resource);

    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);
    when(map.get(any(),any())).thenReturn("");

    List<String> missedMetaData = new ArrayList<>();
    missedMetaData.add(JcrConstants.JCR_TITLE);
    when(workItem.getNode()).thenReturn(node);
    when(map.get("bnpp-status", StringUtils.EMPTY)).thenReturn("validated");

    when(workflowProcessMock.checkMissingMetadata(any(), any(), any(), any(), any())).thenReturn(missedMetaData);

    workflowProcess.execute(workItem, workflowSession, metadataMap);
  }

  @Test
  public void executeEmptyContent() throws Exception {

    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    when(resolver.isLive()).thenReturn(Boolean.TRUE);

    when(workflowData.getPayload()).thenReturn(payload);
    when(resource.getPath()).thenReturn("/dam/projects/");
    when(resolver.getResource(any())).thenReturn(resource);
    when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(null);

    when(resource.getResourceType()).thenReturn(BnpConstants.DAM_ASSET);
    when(resource.getParent()).thenReturn(resource);
    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);

    workflowProcess.execute(workItem, workflowSession, metadataMap);
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
  public void checkMissingMetadata() throws LoginException {
    workflowProcess.resolverFactory = resolverFactory;
    when(workflowProcess.resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
    List<String> missedMetaData = new ArrayList<>();
    when(resource.getChild(any())).thenReturn(resource);

    ValueMap map = mock(ValueMap.class);
    when(resource.getValueMap()).thenReturn(map);
    when(map.get(any(),any())).thenReturn("");

    when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
    when(resolver.adaptTo(Session.class)).thenReturn(session);
    when(queryBuilder.createQuery(any(),any())).thenReturn(query);
    when(query.getResult()).thenReturn(result);
    when(result.getResources()).thenReturn(requiredFields);

    assertEquals(missedMetaData,workflowProcess.checkMissingMetadata(resolver, missedMetaData, resource, resource, "/"));
  }

}
