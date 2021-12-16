package com.mediahub.core.listeners;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.InputStream;
import java.math.BigDecimal;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import java.util.Set;

import javax.jcr.Binary;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.ValueFormatException;
import javax.jcr.Workspace;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.ObservationManager;
import javax.jcr.security.AccessControlEntry;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.AccessControlPolicy;
import javax.jcr.security.Privilege;

import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChange.ChangeType;
import org.apache.sling.event.jobs.JobManager;
import org.apache.sling.jcr.api.SlingRepository;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.osgi.service.component.ComponentContext;
import javax.jcr.observation.Event;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import mockit.MockUp;

@ExtendWith({ AemContextExtension.class })
public class ProjectGroupResourceListenerTest {
    private static final String PROJECT_PATH = "/content/projects/bnpfolder1/bnpfolder2/bnpproject";
    private static final String PARENT_PATH = "/content/projects/bnpfolder1/bnpfolder2";

    @InjectMocks
    private ProjectGroupResourceListener projectsResourceListener = new ProjectGroupResourceListener();

    private AemContext context;

    @Mock
    private ResourceResolverFactory resourceResolverFactory;

    @Mock
    private ResourceResolver resolver;

    @Mock
    private Resource resource;
    
    @Mock
    private JobManager jobManager;
    
    @Mock
    private EventIterator eventIterator;

   

    @Mock
    SlingRepository slingRepository;
    
    Value[] values;


    Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);


    @BeforeEach
    void setup() throws Exception, LoginException {
        MockitoAnnotations.initMocks(this);
       
        context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
 
    }

    @Test
    public void onEventTest() throws Exception, RepositoryException {
        
        

        Event resourceEvent = new Event() {
            
            @Override
            public String getUserID() {
                // TODO Auto-generated method stub
                return "groupone";
            }
            
            @Override
            public String getUserData() throws RepositoryException {
                // TODO Auto-generated method stub
                return null;
            }
            
            @Override
            public int getType() {
                // TODO Auto-generated method stub
                return 0;
            }
            
            @Override
            public String getPath() throws RepositoryException {
                // TODO Auto-generated method stub
                return "/home/group/mediahub/groupone";
            }
            
            @Override
            public Map getInfo() throws RepositoryException {
                Map<String, Value[]> infoMap = new HashMap<>();
                infoMap.put(BnpConstants.AFTER_VALUE, null);
                
                return infoMap;
            }
            
            @Override
            public String getIdentifier() throws RepositoryException {
                // TODO Auto-generated method stub
                return null;
            }
            
            @Override
            public long getDate() throws RepositoryException {
                // TODO Auto-generated method stub
                return 0;
            }
        };
        when(eventIterator.hasNext()).thenReturn(true);
        when(eventIterator.nextEvent()).thenReturn(resourceEvent);
        when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        //when(resourceEvent.getInfo().get(BnpConstants.AFTER_VALUE)).thenReturn(values);


        projectsResourceListener.onEvent(eventIterator);

    }



}
