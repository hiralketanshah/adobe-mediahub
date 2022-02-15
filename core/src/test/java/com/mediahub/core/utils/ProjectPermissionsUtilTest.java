package com.mediahub.core.utils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.adobe.acs.commons.workflow.bulk.execution.model.Payload;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class ProjectPermissionsUtilTest {

    @InjectMocks
    ProjectPermissionsUtil fixture = new ProjectPermissionsUtil();

    @Mock
    private ResourceResolver resolver;


    @Mock
    Resource resource;


    @Mock
    ValueMap valueMap;
    
    @Mock
    UserManager um;
    
    @Mock
    Group group;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        when(resolver.getResource(Mockito.anyString())).thenReturn(resource);
        when(resource.getChild(any())).thenReturn(resource);
        when(resource.getValueMap()).thenReturn(valueMap);
        when(valueMap.containsKey(Mockito.any())).thenReturn(true);
        
        when(resource.getPath()).thenReturn("/content/dam/test/projects");
    }

    @Test
    public void execute() throws Exception {
        when(valueMap.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)).thenReturn(Boolean.TRUE);
        when(valueMap.get(BnpConstants.BNPP_BROADCAST_STATUS, new String[] {}))
                .thenReturn(new String[] { "not-broadcast" });
        when(valueMap.containsKey("dam:scene7ID")).thenReturn(Boolean.TRUE);
        
        when(resolver.adaptTo(UserManager.class)).thenReturn(um);
        when(um.getAuthorizable(Mockito.anyString())).thenReturn(group);
        when(group.isMember(Mockito.any())).thenReturn(true);
        
        when(valueMap.get("projectPath", new String[]{""})).thenReturn(new String[] {"test"});
        when(valueMap.get(Mockito.anyString())).thenReturn("a|562043580");
        fixture.isAuthorizedForProject(resolver, "/content/dam/projects", new String[] {"group1", "group2"}, "test123");
    }

  

}
