package com.mediahub.core.workflows;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jcr.Session;

import org.apache.commons.lang.StringUtils;
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
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.metadata.SimpleMetaDataMap;
import com.adobe.granite.workflow.model.WorkflowNode;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
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
    ValueMap map;

    @Mock
    WorkflowNode node;

    @Mock
    Set<String> keySet;

    @Mock
    private SearchResult searchResult;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        metadataMap = new SimpleMetaDataMap();
        when(workItem.getWorkflowData()).thenReturn(workflowData);
    }

    @Test
    public void execute() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        execute2();
        when(map.get("bnpp-status", StringUtils.EMPTY)).thenReturn("validated");
        when(workItem.getNode()).thenReturn(node);
        when(resolver.isLive()).thenReturn(Boolean.TRUE);
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    @Test
    public void execute3() throws Exception {
        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        execute2();
        when(map.get("bnpp-status", StringUtils.EMPTY)).thenReturn("Not-validated");
        when(workItem.getNode()).thenReturn(node);
        when(resolver.isLive()).thenReturn(Boolean.TRUE);
        Assertions.assertThrows(WorkflowException.class, () -> {
            workflowProcess.execute(workItem, workflowSession, metadataMap);
        });
    }

    public void execute2() {
        when(workflowData.getPayloadType()).thenReturn("JCR_PATH");
        when(workflowData.getPayload()).thenReturn(payload);
        when(payload.toString()).thenReturn("/dam/projects/");
        when(resolver.getResource("/dam/projects/")).thenReturn(resource);
        when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
        when(resource.getResourceType()).thenReturn(BnpConstants.DAM_ASSET);
        when(resource.getParent()).thenReturn(resource);
        when(resource.getChild(BnpConstants.METADATA)).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(map);
        when(map.get(BnpConstants.METADATA_SCHEMA, StringUtils.EMPTY)).thenReturn("MediaHubSchema");
        when(resolver.adaptTo(QueryBuilder.class)).thenReturn(queryBuilder);
        when(resolver.adaptTo(Session.class)).thenReturn(session);
        when(queryBuilder.createQuery(any(PredicateGroup.class), any(Session.class))).thenReturn(query);
        when(query.getResult()).thenReturn(searchResult);
        List<Resource> userList = new ArrayList<>();
        userList.add(resource);
        when(searchResult.getResources()).thenReturn(userList.iterator());
        when(map.get("cq-msm-lockable", StringUtils.EMPTY)).thenReturn("/dam/projects/./metadata/");
        keySet = new HashSet<>();
        keySet.add("/media/worflow/");
        when(map.keySet()).thenReturn(keySet);
        when(map.get(BnpConstants.FOLDER_METADATA_SCHEMA, StringUtils.EMPTY)).thenReturn("FolderMediaHubSchema");
        when(workItem.getNode()).thenReturn(node);
    }

}
