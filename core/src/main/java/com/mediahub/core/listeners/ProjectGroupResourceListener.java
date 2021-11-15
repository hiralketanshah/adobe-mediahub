package com.mediahub.core.listeners;

import com.mediahub.core.constants.BnpConstants;
import javax.jcr.observation.ObservationManager;
import org.apache.sling.api.resource.LoginException;
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

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
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

    @Reference
    private SlingRepository repository;

    @Reference
    JobManager jobManager;

    private Session session;

    private ObservationManager observationManager;

    /**
     * Activate method to initialize stuff
     */
    @Activate
    protected void activate(ComponentContext componentContext) {

        /**
         * This map will be used to get session via getServiceResourceResolver() method
         */
        Map<String, Object> params = new HashMap<>();

        /**
         * Adding the subservice name in the param map
         */
        params.put(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

        log.info("Activating the observation");

        try {

            session  = repository.loginService(BnpConstants.WRITE_SERVICE,null);
            observationManager = session.getWorkspace().getObservationManager();
            log.info("Session created");
            /**
             * Adding the event listener
             */
            observationManager.addEventListener(this,
                Event.NODE_MOVED | Event.PERSIST | Event.PROPERTY_ADDED | Event.NODE_ADDED | Event.PROPERTY_CHANGED, "/home/groups/projects", true, null, new String[]{"rep:Group"}, false);

        } catch (RepositoryException e) {
            log.error("Error while accessing repository : {}", e);
        }
    }

    @Deactivate
    protected void deactivate() {
        try {
            if (observationManager != null) {
                observationManager.removeEventListener(this);
                log.info("*************removed JCR event listener");
            }
        }
        catch (RepositoryException re) {
            log.error("*************error removing the JCR event listener ", re);
        }
        finally {
            if (session != null) {
                session.logout();
                session = null;
            }
        }
    }

    @Override
    public void onEvent(EventIterator events) {
        try {
            while (events.hasNext()) {
                Event event = events.nextEvent();
                final Map<String, Object> properties = new HashMap<>();
                log.debug("Something has been added: {} ", event.getPath());
                properties.put("userID", event.getUserID());
                properties.put(BnpConstants.AFTER_VALUE, convertArrayToList((Value[]) event.getInfo().get(BnpConstants.AFTER_VALUE)));
                properties.put(BnpConstants.BEFORE_VALUE, convertArrayToList((Value[]) event.getInfo().get(BnpConstants.BEFORE_VALUE)));
                properties.put(BnpConstants.PATH, event.getPath());
                jobManager.addJob("user/project/access/email", properties);
            }
        } catch (RepositoryException e) {
            log.error("Error while sending user notification mail", e);
        }
    }

    private List<String> convertArrayToList(Value[] values) {
        List<String> valueList = new ArrayList<>();
        if (null != values) {
            for (Value value : values) {
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