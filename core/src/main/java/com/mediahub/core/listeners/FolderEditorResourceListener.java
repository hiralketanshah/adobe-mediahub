package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.Map;
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
        EventConstants.EVENT_TOPIC + "=org/apache/sling/api/resource/resource/changed",
        EventConstants.EVENT_FILTER + "path=/content/dam/mediahub"
    })
@ServiceDescription("Demo to listen on changes in the resource tree")
public class FolderEditorResourceListener implements EventHandler {

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Reference
  ResourceResolverFactory resourceResolverFactory;

  public void handleEvent(final Event event) {
    final Map<String, Object> authInfo = Collections
        .singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    ResourceResolver resolver = null;
    try {
      resolver = resourceResolverFactory.getResourceResolver(authInfo);
      String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();

      Resource contentResourse;
      if(StringUtils.endsWith(path, JcrConstants.JCR_CONTENT)){
        contentResourse = resolver.getResource(event.getProperty(SlingConstants.PROPERTY_PATH).toString());
      } else {
        contentResourse = resolver.getResource(event.getProperty(SlingConstants.PROPERTY_PATH).toString()).getChild(JcrConstants.JCR_CONTENT);
      }

      if(contentResourse != null && StringUtils.equals(contentResourse.getParent().getValueMap().get(JcrConstants.JCR_PRIMARYTYPE, String.class), "sling:Folder")){

        Resource metadata = contentResourse.getChild(BnpConstants.METADATA);
        if(metadata == null) {
          metadata = resolver.create(contentResourse, BnpConstants.METADATA, null);
          resolver.commit();
        }

        ModifiableValueMap adpatableResource = metadata.adaptTo(ModifiableValueMap.class);
        adpatableResource.put(JcrConstants.JCR_LASTMODIFIED, new GregorianCalendar());
        adpatableResource.put(JcrConstants.JCR_LAST_MODIFIED_BY,  event.getProperty(BnpConstants.USER_ID).toString());
      }

      if(resolver.hasChanges()){
        resolver.commit();
      }
    } catch (LoginException e) {
      logger.error("Error while Creating resource resolver {}", e.getMessage());
    } catch (PersistenceException e) {
      logger.error("Error while saving the resource {}", e.getMessage());
    } finally {
      if(null != resolver){
        resolver.close();
      }
    }
    logger.debug("Resource event: {} at: {}", event.getTopic(), event.getProperty(SlingConstants.PROPERTY_PATH));
  }
}
