package com.mediahub.core.listeners;

import java.security.Principal;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;

/**
 * @author Shipra Arora
 *
 *         Listener class to provide read permissions to the parent project folders until projects.
 *
 */

@Component(service = {
	    ResourceChangeListener.class},
	    immediate = true,
	    property = {
	        ResourceChangeListener.CHANGES + "=ADDED",
	        ResourceChangeListener.PATHS + "=glob:/content/projects/**"// for handling custom invalidation for AF, AFF
	    }
	)

@ServiceDescription("listen on changes in the resource tree")
public class ProjectsResourceListener implements ResourceChangeListener {

private final Logger log = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

	/** The repository. */
	@Reference
	org.apache.sling.jcr.api.SlingRepository repository;
    @Reference
    private ConfigurationAdmin configAdmin;
    List<Principal> principalNameList;


@Override
public void onChange(List<ResourceChange> arg0) {
	  final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
	            BnpConstants.WRITE_SERVICE);
	    Session adminSession = null;
	    try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
	    	
	        // getting session of System User
	        adminSession = adminResolver.adaptTo(Session.class);
	        for(ResourceChange my : arg0) {
    	
		        String projectPath = my.getPath();
		        if("cq/gui/components/projects/admin/card/projectcard".equals(adminResolver.getResource(projectPath).getResourceType())) {
		            Resource adminResource = adminResolver.getResource(projectPath);
		            principalNameList = new LinkedList<>();
		            Node projectNode = adminResource.adaptTo(Node.class);
		            
		            JackrabbitSession js = (JackrabbitSession) adminSession;
		            PrincipalManager principalMgr = js.getPrincipalManager();
		            Principal groupEditorPrincipal = principalMgr
		                    .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EDITOR).getString());
		            Principal groupObserverPrincipal = principalMgr
		                    .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OBSERVER).getString());
		            Principal groupOwnerPrincipal = principalMgr
		                    .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_OWNER).getString());
		            Principal groupOwnerProjectPublisher = principalMgr
		                    .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_PROJECTPUBLISHER).getString());
		            Principal groupExternalContribPrincipal = principalMgr
		                    .getPrincipal(projectNode.getProperty(BnpConstants.ROLE_EXTERNALCONTRIBUTEUR).getString());
		            principalNameList.add(groupEditorPrincipal);
		            principalNameList.add(groupObserverPrincipal);
		            principalNameList.add(groupOwnerPrincipal);
		            principalNameList.add(groupOwnerProjectPublisher);
		            principalNameList.add(groupExternalContribPrincipal);
		         
		            while (adminResource.getParent() != null
		                    && !StringUtils.equals(	adminResource.getParent().getPath(), BnpConstants.AEM_PROJECTS_PATH)) {
		                adminResource = adminResource.getParent();
		                String parentFolderPath = adminResource.getPath();
		                Node parentFldrNode = adminResource.adaptTo(Node.class);
		                if (parentFldrNode.hasNode(BnpConstants.REP_POLICY)) {
		                    CreatePolicyNodeUtil.creatrepPolicyeNodes(adminSession, parentFolderPath, principalNameList);
		                } else {

		                    ModifiableValueMap mvp = adminResource.adaptTo(ModifiableValueMap.class);
		                    mvp.put(BnpConstants.JCR_MIXINTYPES, BnpConstants.REP_ACCESSCONTROLLABLE);
		                    adminResolver.commit();
		                    String parentProjectPath = adminResource.getPath();
		                    Resource reResource = adminResolver.getResource(parentProjectPath);
		                    Node createPolicyNode = reResource.adaptTo(Node.class);
		                    createPolicyNode.addNode(BnpConstants.REP_POLICY, BnpConstants.REP_ACL);
		                    adminResolver.commit();
		                    CreatePolicyNodeUtil.creatrepPolicyeNodes(adminSession, parentFolderPath, principalNameList);
		                }
		            }
		       }
	        }
	    } catch (LoginException | RepositoryException | PersistenceException e) {
			log.error("RepositoryException while Executing events", e);
		}finally {
	        if(adminSession!=null)
	        {
	        	adminSession.logout();
	        }
		}
	
}
}
