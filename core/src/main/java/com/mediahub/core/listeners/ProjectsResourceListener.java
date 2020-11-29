package com.mediahub.core.listeners;

import java.security.Principal;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.constants.MediahubConstants;
import com.mediahub.core.utils.CreatePolicyNodeUtil;

/**
 * @author Shipra Arora
 *
 *         Listener class to provide read permissions to the parent project folders until projects.
 *
 */
@Component(immediate = true, service = EventListener.class)
public class ProjectsResourceListener implements EventListener {
    /**
     * Logger
     */
    Logger log = LoggerFactory.getLogger(this.getClass());
    @Reference
    private ResourceResolverFactory resolverFactory;
	/** The admin session. */
	private Session session;

	/** The repository. */
	@Reference
	org.apache.sling.jcr.api.SlingRepository repository;
    @Reference
    private ConfigurationAdmin configAdmin;
    List<Principal> principalNameList;

    /**
     * Activate method of ProjectsResourceListener
     *
     * @param context
     * @throws Exception
     */
    @Activate
    public void activate(ComponentContext context) throws Exception {
    	try {
			session = repository.loginService("datawrite", null);
		     final String[] nodeTypes = { MediahubConstants.NT_NODE_TYPE };
			session.getWorkspace().getObservationManager().addEventListener(this, Event.NODE_ADDED,
					  MediahubConstants.AEM_PROJECTS_PATH, true, null, nodeTypes, false);
		} catch (RepositoryException e) {
			log.error("RepositoryException occurred in activate method", e);
		}      	
    }
    

    /**
     * Deactivate method to close the resources
     */
    @Deactivate
    public void deactivate() {
    	
        
        
    }

    /*
     * This method will be from listener to update parent folder permissions
     *
     * @see javax.jcr.observation.EventListener#onEvent(javax.jcr.observation. EventIterator)
     */
    public void onEvent(EventIterator eventIterator) {
            log.info("System User Id is:\" + adminSession.getUserID()");
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            Session adminSession = null;
            try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            	log.info("insde");
	            // getting session of System User
	            adminSession = adminResolver.adaptTo(Session.class);
	            String projectPath = eventIterator.nextEvent().getPath();
	            log.info("Project Created : {}", projectPath);
	            int index = projectPath.lastIndexOf("/");
	            if(projectPath.contains("rep:policy")) {
		            log.info("dCreation");
		            Resource adminResource = adminResolver.getResource(projectPath.substring(0, index));
		            principalNameList = new LinkedList<>();
		            Node projectNode = adminResource.adaptTo(Node.class);
		            JackrabbitSession js = (JackrabbitSession) adminSession;
		            PrincipalManager principalMgr = js.getPrincipalManager();
		            if(projectNode.getProperty(MediahubConstants.ROLE_EDITOR)!=null){
			            Principal groupEditorPrincipal = principalMgr
			                    .getPrincipal(projectNode.getProperty(MediahubConstants.ROLE_EDITOR).getString());
			            Principal groupObserverPrincipal = principalMgr
			                    .getPrincipal(projectNode.getProperty(MediahubConstants.ROLE_OBSERVER).getString());
			            Principal groupOwnerPrincipal = principalMgr
			                    .getPrincipal(projectNode.getProperty(MediahubConstants.ROLE_OWNER).getString());
			            Principal groupOwnerProjectPublisher = principalMgr
			                    .getPrincipal(projectNode.getProperty(MediahubConstants.ROLE_PROJECTPUBLISHER).getString());
			            Principal groupExternalContribPrincipal = principalMgr
			                    .getPrincipal(projectNode.getProperty(MediahubConstants.ROLE_EXTERNALCONTRIBUTEUR).getString());
			            principalNameList.add(groupEditorPrincipal);
			            principalNameList.add(groupObserverPrincipal);
			            principalNameList.add(groupOwnerPrincipal);
			            principalNameList.add(groupOwnerProjectPublisher);
			            principalNameList.add(groupExternalContribPrincipal);
			            log.info("adminResource.getParent().getName()) : "+adminResource.getParent().getName() );
			            while (adminResource.getParent() != null
			                    && !StringUtils.equals((adminResource.getParent().getName()), MediahubConstants.CONSTANT)) {
			                adminResource = adminResource.getParent();
			                String parentFolderPath = adminResource.getPath();
			                log.info("path :" +parentFolderPath);
			                if(!parentFolderPath.equals(MediahubConstants.CONSTANT)){
			                    log.info("Create :" +parentFolderPath);
				                Node parentFldrNode = adminResource.adaptTo(Node.class);
				                if (parentFldrNode.hasNode(MediahubConstants.REP_POLICY)) {
				                    CreatePolicyNodeUtil.creatrepPolicyeNodes(adminSession, parentFolderPath, principalNameList);
				                } else {
				                    ModifiableValueMap mvp = adminResource.adaptTo(ModifiableValueMap.class);
				                    mvp.put(MediahubConstants.JCR_MIXINTYPES, MediahubConstants.REP_ACCESSCONTROLLABLE);
				                    adminResolver.commit();
				                    String parentProjectPath = adminResource.getPath();
				                    Resource reResource = adminResolver.getResource(parentProjectPath);
				                    Node createPolicyNode = reResource.adaptTo(Node.class);
				                    createPolicyNode.addNode(MediahubConstants.REP_POLICY, MediahubConstants.REP_ACL);
				                    adminResolver.commit();
				                    CreatePolicyNodeUtil.creatrepPolicyeNodes(adminSession, parentFolderPath, principalNameList);
				                }
			                }
			            }
		            }
		            log.info("End of activating Project creation Observation in ProjectsResourceListener");
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
