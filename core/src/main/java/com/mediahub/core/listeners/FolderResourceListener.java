package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.UserUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.Map;
import java.util.UUID;

import static com.mediahub.core.constants.BnpConstants.MEDIALIBRARY_PATH;

@Component(service = EventHandler.class,
        immediate = true,
        property = {
                EventConstants.EVENT_TOPIC + "=" + BnpConstants.TOPIC_RESOURCE_ADDED,
                EventConstants.EVENT_TOPIC + "=" + BnpConstants.TOPIC_RESOURCE_CHANGED,
                EventConstants.EVENT_FILTER + "=(|(path=/content/dam/medialibrary/*)(path=/content/dam/projects/*)(path=/content/projects/*))"
        })
@ServiceDescription("listen on changes in the resource tree")
public class FolderResourceListener implements EventHandler {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    ResourceResolverFactory resourceResolverFactory;

    public void handleEvent(final Event event) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        ResourceResolver resolver = null;

        try {
            resolver = resourceResolverFactory.getServiceResourceResolver(authInfo);
            String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();
            Resource contentResourse;
            if (StringUtils.contains(path, JcrConstants.JCR_CONTENT)) {
                path = StringUtils.replace(path, "/" + JcrConstants.JCR_CONTENT + "/(.*)", StringUtils.EMPTY);
            }
            contentResourse = resolver.getResource(path).getChild(JcrConstants.JCR_CONTENT);
            if (contentResourse != null) {
                captureFolderChanges(event, resolver, contentResourse);
                captureDamAssetChanges(event, resolver, contentResourse);
            } else {
                captureProjectFolderChanges(event, resolver, resolver.getResource(path));
            }


            if (resolver.hasChanges()) {
                resolver.commit();
            }
        } catch (LoginException e) {
            logger.error("Error while Creating resource resolver {}", e.getMessage());
        } catch (PersistenceException e) {
            logger.error("Error while saving the resource {}", e.getMessage());
        } finally {
            if (null != resolver) {
                resolver.close();
            }
        }
        logger.debug("Resource event: {} at: {}", event.getTopic(), event.getProperty(SlingConstants.PROPERTY_PATH));
    }

    protected void captureDamAssetChanges(Event event, ResourceResolver resolver, Resource contentResourse)
            throws PersistenceException {
        if (StringUtils.equals(contentResourse.getParent().getValueMap().get(JcrConstants.JCR_PRIMARYTYPE, String.class), BnpConstants.DAM_ASSET)) {
            contentResourse = contentResourse.getParent().getParent().getChild(JcrConstants.JCR_CONTENT);
            if (contentResourse != null) {
                Resource metadata = contentResourse.getChild(BnpConstants.METADATA);
                if (metadata == null) {
                    metadata = resolver.create(contentResourse, BnpConstants.METADATA, null);
                    resolver.commit();
                }
                ModifiableValueMap adpatableResource = metadata.adaptTo(ModifiableValueMap.class);
                if (StringUtils.equals(event.getTopic(), BnpConstants.TOPIC_RESOURCE_ADDED)) {
                    adpatableResource.put(JcrConstants.JCR_LASTMODIFIED, new GregorianCalendar());
                    if (!StringUtils.equals(event.getProperty(BnpConstants.USER_ID).toString(), BnpConstants.USER_DEACTIVATION_SERVICE)) {
                        adpatableResource.put(JcrConstants.JCR_LAST_MODIFIED_BY, event.getProperty(BnpConstants.USER_ID).toString());
                    }
                }
            }
        }
    }

    protected void captureProjectFolderChanges(Event event, ResourceResolver resolver, Resource contentResourse) throws PersistenceException {
        if (StringUtils.equals(event.getTopic(), BnpConstants.TOPIC_RESOURCE_ADDED) && contentResourse.getPath().startsWith(BnpConstants.AEM_PROJECTS_PATH) && !StringUtils.contains(contentResourse.getPath(), JcrConstants.JCR_CONTENT)) {
            String parentFolderPath = contentResourse.getParent().getPath();
            String[] folderSegments = parentFolderPath.substring(1).split("/");
            //We limit user creation to level 5 max (after /content/projects)
            if (folderSegments.length <= 6) {
                UserUtils.createProjectFolderGroups(resolver, contentResourse);
            }
        }
    }


    protected void captureFolderChanges(Event event, ResourceResolver resolver, Resource contentResourse) throws PersistenceException {
        if (StringUtils.equals(contentResourse.getParent().getValueMap().get(JcrConstants.JCR_PRIMARYTYPE, String.class), BnpConstants.SLING_FOLDER)) {
            Resource metadata = contentResourse.getChild(BnpConstants.METADATA);
            if (metadata == null) {
                metadata = resolver.create(contentResourse, BnpConstants.METADATA, null);
                resolver.commit();
            }
            ModifiableValueMap adpatableResource = metadata.adaptTo(ModifiableValueMap.class);

            if (StringUtils.equals(event.getTopic(), BnpConstants.TOPIC_RESOURCE_CHANGED)) {
                adpatableResource.put(JcrConstants.JCR_LASTMODIFIED, new GregorianCalendar());
                if (!StringUtils.equals(event.getProperty(BnpConstants.USER_ID).toString(),
                        BnpConstants.USER_DEACTIVATION_SERVICE)) {
                    adpatableResource.put(JcrConstants.JCR_LAST_MODIFIED_BY, event.getProperty(BnpConstants.USER_ID).toString());
                }

            }
            if (StringUtils.equals(event.getTopic(), BnpConstants.TOPIC_RESOURCE_ADDED)) {
                adpatableResource.put(JcrConstants.JCR_CREATED, new GregorianCalendar());
                if (!StringUtils.equals(event.getProperty(BnpConstants.USER_ID).toString(), BnpConstants.USER_DEACTIVATION_SERVICE)) {
                    adpatableResource.put(JcrConstants.JCR_CREATED_BY, event.getProperty(BnpConstants.USER_ID).toString());
                }
                // MED-491 Assign UUID to folder not Media
                if (!StringUtils.equals(adpatableResource.get(BnpConstants.BNPP_MEDIA, StringUtils.EMPTY), Boolean.TRUE.toString())) {
                    String parentFolderPath = contentResourse.getParent().getPath();
                    String[] folderSegments = parentFolderPath.substring(1).split("/");
                    //We limit user creation to level 5 max (after /content/dam/medialibrary)
                    if (folderSegments.length <= 7 && contentResourse.getPath().startsWith(MEDIALIBRARY_PATH)) {
                        String uuid = UUID.randomUUID().toString();
                        adpatableResource.put("uuid", uuid);
                        // MED-493 Create user group while creating folders
                        UserUtils.createFolderGroups(resolver, uuid, contentResourse.getParent());
                    }
                }
            }
        }
    }

}
