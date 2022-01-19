package com.mediahub.core.utils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.security.AccessControlManager;

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

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class CreatePolicyNodeUtilTest {
    
    AemContext context = new AemContext();

    @InjectMocks
    CreatePolicyNodeUtil fixture = new CreatePolicyNodeUtil();

    @Mock
    private ResourceResolver resolver;


    @Mock
    Resource resource;


    @Mock
    AccessControlManager accessControlManager;
    
    @Mock
    Principal principal;
    
    @Mock
    Session session;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
      // session = context.resourceResolver().adaptTo(Session.class);

        when(session.getAccessControlManager()).thenReturn(accessControlManager);
    }

    @Test
    public void execute() throws Exception {
        Map<String, Value> restrictions = new HashMap<>();
        List<Principal> listOfPrinciPals = new ArrayList<>();
        listOfPrinciPals.add(principal);
        fixture.createRepPolicyNode(session, "/content/dam/test", true, principal, new String[] {"read", "write"});
        fixture.createRepPolicyNode(session, "/content/dam/test", principal, new String[] {"read", "write"});
        fixture.createRepPolicyNode(session, "/content/dam/test", principal, restrictions, new String[] {"read", "write"});
        fixture.createRepPolicyNodes(session, "/content/dam/test", listOfPrinciPals);
        fixture.createRepPolicyNodes(session, "/content/dam/test", listOfPrinciPals, restrictions);
    }

  

}
