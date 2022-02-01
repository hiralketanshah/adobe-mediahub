package com.mediahub.core.listeners;

import com.mediahub.core.constants.BnpConstants;
import javax.jcr.observation.ObservationManager;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.jcr.api.SlingRepository;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

@Component(service = EventListener.class, immediate = true)
@Designate(ocd = RemoveRichMediaListener.Config.class)
public class RemoveRichMediaListener implements EventListener {

    private static final Logger log = LoggerFactory.getLogger(RemoveRichMediaListener.class);

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference
    private SlingRepository repository;

    @Reference
    private SlingSettingsService slingSettings;

    @SuppressWarnings("AEM Rules:AEM-3")
    private Session session;

    private ObservationManager observationManager;

    private static String destinationPath;

    @Activate
    protected void activate(Config config) {
        Map<String, Object> params = new HashMap<>();
        try {
            if (isPublisher()) {
                this.destinationPath = config.destinationPath();
                params.put(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
                log.info("Activating the observation");
                session = repository.loginService(BnpConstants.WRITE_SERVICE, null);
                observationManager = session.getWorkspace().getObservationManager();
                log.info("Session created");
                observationManager.addEventListener(this, Event.NODE_REMOVED, "/content/dam/medialibrary", true, null,
                        new String[] { "dam:Asset", "nt:unstructured", "nt:folder", "sling:Folder" }, false);
            }
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

    private boolean isPublisher() {
        return slingSettings.getRunModes().contains("publish");
    }

    @Override
    public void onEvent(EventIterator events) {
        try {
            while (events.hasNext()) {
                Event event = events.nextEvent();
                if(event.getPath().contains(".zip")) {
                    removeFile(event.getIdentifier());
                }
            }
        } catch (RepositoryException e) {
            log.error("Error while listening to events", e);
        }
    }

    @SuppressWarnings("squid:S899")
    private static void removeFile(String assetName) {

        File folderFile = new File(destinationPath);
        File entry = new File(folderFile.getAbsolutePath(), assetName);
        if (entry.exists()) {
            File[] files = entry.listFiles();
            for (int i = 0; i < files.length; i++) {
                files[i].delete();
            }
        }
        boolean deleted = entry.delete();
        log.info("File removed : {}", deleted);
    }

    @ObjectClassDefinition(name = "Mediahub Remove Media To File System", description = "Configuration for removing rich media to file system")
    public static @interface Config {
        @AttributeDefinition(name = "Destination path", description = "Destination path for removing the media")
        String destinationPath() default "/";
    }
}
