package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.oak.spi.security.user.UserConstants;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.apache.sling.event.jobs.JobManager;
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
        EventConstants.EVENT_TOPIC + "=org/apache/sling/api/resource/Resource/CHANGED",
        EventConstants.EVENT_FILTER +  "=(& (path=/content/dam/projects/*/metadata)    )"
    })
@ServiceDescription("listen on changes in the resource tree")
public class InternalFolderListener implements EventHandler {

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Reference
  ResourceResolverFactory resourceResolverFactory;

  @Reference
  JobManager jobManager;

  public void handleEvent(final Event event) {
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    if(StringUtils.equals(event.getProperty("resourceType").toString(), JcrConstants.NT_UNSTRUCTURED)){
      try (ResourceResolver resolver = resourceResolverFactory.getServiceResourceResolver(authInfo)) {

        String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();
        Resource metadata = resolver.getResource(path);

        ValueMap profileProperties = metadata.getValueMap();
        final Map<String, Object> properties = new HashMap<>();
        String isInternal = profileProperties.get("internalfolder", Boolean.FALSE.toString());
        properties.put("internalfolder", isInternal);
        properties.put("path", metadata.getParent().getParent().getPath());

        Resource renderCondition = metadata.getParent().getParent().getChild("granite:rendercondition");

        if( (renderCondition == null && StringUtils.equals( Boolean.TRUE.toString(),isInternal)) || (renderCondition != null && StringUtils.equals( Boolean.FALSE.toString(),isInternal))  ){
          jobManager.addJob("render/condition/node", properties);
        }
      } catch (LoginException e) {
        logger.error("Error while fetching system user {0}", e);
      }
    }

    logger.debug("Resource event: {} at: {}", event.getTopic(), event.getProperty(SlingConstants.PROPERTY_PATH));
  }

}
