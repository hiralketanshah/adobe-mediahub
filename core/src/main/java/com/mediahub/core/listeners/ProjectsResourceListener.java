package com.mediahub.core.listeners;

import com.adobe.cq.projects.api.Project;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.Map;

import javax.jcr.ItemExistsException;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.lock.LockException;
import javax.jcr.nodetype.ConstraintViolationException;
import javax.jcr.version.VersionException;

import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = EventHandler.class,
    immediate = true,
    property = {
        EventConstants.EVENT_TOPIC + "=" + BnpConstants.TOPIC_RESOURCE_ADDED,
        
        EventConstants.EVENT_FILTER + "path=/content/projects/"
    })
@ServiceDescription("listen on changes in the Mediahub Projects Group")
public class ProjectsResourceListener implements EventHandler {

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Reference
  ResourceResolverFactory resourceResolverFactory;

  public void handleEvent(final Event event) {
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    ResourceResolver resolver = null;

    try {
      resolver = resourceResolverFactory.getResourceResolver(authInfo);
      String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();
      Resource contentResourse;
      if(StringUtils.contains(path, JcrConstants.JCR_CONTENT)){
        path = StringUtils.replace(path, "/"+  JcrConstants.JCR_CONTENT + "/(.*)", StringUtils.EMPTY);
      }
      resolver.adaptTo(Session.class);
      contentResourse = resolver.getResource(path).getChild(JcrConstants.JCR_CONTENT);
     // Resource contentResourse = resolver.getResource(path);
      if(contentResourse != null){
    	  Resource parent = contentResourse.getParent();
    	  ModifiableValueMap parentVM = parent.adaptTo(ModifiableValueMap.class);
    	  if(StringUtils.equals(parentVM.get("jcr:primaryType").toString(), "sling:OrderedFolder")){
    		  

    		parentVM.put(JcrConstants.JCR_MIXINTYPES, "rep:AccessControllable");
    		Node node = parent.adaptTo(Node.class);
    		Node node1 = node.addNode("rep:policy");
    		node1.setPrimaryType("rep:ACL");
    		Node newNode = node1.addNode("allow1");
    		newNode.setPrimaryType("rep:GrantACE");
    		newNode.setProperty("rep:principalName", "everyone");
    		newNode.setProperty("rep:privileges", "jcr:read");
    		
          }
    	  
    	  
        
      }

      if(resolver.hasChanges()){
        resolver.commit();
      }
    } catch (LoginException e) {
      logger.error("Error while Creating resource resolver {}", e.getMessage());
    } catch (PersistenceException e) {
      logger.error("Error while saving the resource {}", e.getMessage());
    } catch (ItemExistsException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (PathNotFoundException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (VersionException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (ConstraintViolationException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (LockException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (RepositoryException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} finally {
      if(null != resolver){
        resolver.close();
      }
    }
    logger.debug("Resource event: {} at: {}", event.getTopic(), event.getProperty(SlingConstants.PROPERTY_PATH));
  }

  
  
}
