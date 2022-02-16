package com.mediahub.core.listeners;

import com.mediahub.core.constants.BnpConstants;

import org.apache.sling.api.resource.*;
import org.apache.sling.jcr.api.SlingRepository;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import java.util.*;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;

import javax.jcr.observation.ObservationManager;

@Component(service = EventListener.class, immediate = true)
@ServiceDescription("listen on changes in the /home/groups/projects resource tree")
public class UpdateProjectGroupsListener implements EventListener {

    private ObservationManager observationManager;

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference
    private SlingRepository repository;

    private Session session;

    private static final Logger log = LoggerFactory.getLogger(UpdateProjectGroupsListener.class);

    @Activate
    protected void activate() {
        Map<String, Object> params = new HashMap<>();
        params.put(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try {
            session = repository.loginService(BnpConstants.WRITE_SERVICE, null);
            observationManager = session.getWorkspace().getObservationManager();
            observationManager.addEventListener(this, Event.PROPERTY_CHANGED | Event.PROPERTY_ADDED,
                    "/home/groups/projects", true, null, new String[] { "rep:Group" }, false);

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
        } catch (RepositoryException re) {
            log.error("*************error removing the JCR event listener ", re);
        } finally {
            if (session != null) {
                session.logout();
                session = null;
            }
        }
    }

    @Override
    public void onEvent(EventIterator events) {

        while (events.hasNext()) {
            Event event = events.nextEvent();
            try {
                String path = event.getPath();
                String propertyName = path.substring(path.lastIndexOf("/") + 1);
                if (propertyName.equalsIgnoreCase("rep:members")) {
                    log.info(path);
                }

            } catch (RepositoryException e) {
                log.error("*************error accessing the repository ", e);
            }

        }

    }

}
