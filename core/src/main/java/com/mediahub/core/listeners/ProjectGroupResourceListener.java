package com.mediahub.core.listeners;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.JobManager;
import org.apache.sling.jcr.api.SlingRepository;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Abuthahir Ibrahim
 *
 * Event Listener that listens to JCR events
 */
@Component(service = EventListener.class, immediate = true)
public class ProjectGroupResourceListener implements EventListener {

  /**
   * Logger
   */
  private static final Logger log = LoggerFactory.getLogger(ProjectGroupResourceListener.class);

  /**
   * Resource Resolver Factory
   */
  @Reference
  private ResourceResolverFactory resolverFactory;

  /**
   * Resource Resolver
   */
  private ResourceResolver resolver;

  @Reference
  private SlingRepository repository;

  /**
   * Session object
   */
  private Session session;

  @Reference
  JobManager jobManager;

  /**
   * Activate method to initialize stuff
   */
  @Activate
  protected void activate(ComponentContext componentContext) {

    log.info("Activating the observation");

    try {

      /**
       * This map will be used to get session via getServiceResourceResolver() method
       */
      Map<String, Object> params = new HashMap<>();

      /**
       * Adding the subservice name in the param map
       */
      params.put(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

      /**
       * Getting resource resolver from the service factory
       */
      resolver = resolverFactory.getServiceResourceResolver(params);

      /**
       * Adapting the resource resolver to session object
       */
      session = resolver.adaptTo(Session.class);

      log.info("Session created");

      /**
       * Adding the event listener
       */
      session.getWorkspace().getObservationManager().addEventListener(this,
          Event.PROPERTY_ADDED | Event.NODE_ADDED | Event.PROPERTY_CHANGED, "/home/groups/projects/admin", true, null, null, false);

    } catch (Exception e) {
      log.error(e.getMessage(), e);
    }
  }

  @Deactivate
  protected void deactivate() {
    if(session != null) {
      session.logout();
    }
  }

  @Override
  public void onEvent(EventIterator events) {
    try {
      while(events.hasNext()) {
        Event event = events.nextEvent();
        final Map<String, Object> properties = new HashMap<>();
        properties.put("userID", event.getUserID());
        properties.put(BnpConstants.AFTER_VALUE, convertArrayToList((Value[])event.getInfo().get(BnpConstants.AFTER_VALUE)));
        properties.put(BnpConstants.BEFORE_VALUE, convertArrayToList((Value[])event.getInfo().get(BnpConstants.BEFORE_VALUE)));
        properties.put(BnpConstants.PATH, event.getPath());
        jobManager.addJob("user/project/access/email", properties);
        log.debug("Something has been added: {} ", event.getPath() );
      }
    } catch (Exception e) {
      log.error("Error while sending user notification mail", e);
    }
  }

  private List<String> convertArrayToList(Value[] values){
    List<String> valueList = new ArrayList<>();
    if(null != values){
      for(Value value : values){
        try {
          valueList.add(value.getString());
        } catch (RepositoryException e) {
          log.error("Error while sending user notification mail", e);
        }
      }
    }
    return valueList;
  }

}