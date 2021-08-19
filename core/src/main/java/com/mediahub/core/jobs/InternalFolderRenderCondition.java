package com.mediahub.core.jobs;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = JobConsumer.class, immediate = true,
    property = {Constants.SERVICE_DESCRIPTION + "=Job to create internal render condition node",
        JobConsumer.PROPERTY_TOPICS + "=" + "render/condition/node"})
/**
 * This job will send welcome email to user.
 *
 */
public class InternalFolderRenderCondition implements JobConsumer {

  private final Logger logger = LoggerFactory.getLogger(getClass());

  @Reference
  ResourceResolverFactory resourceResolverFactory;

  @Override
  public JobResult process(Job job) {
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    try(ResourceResolver resolver = resourceResolverFactory.getServiceResourceResolver(authInfo)){
      Resource folder = resolver.getResource(job.getProperty("path", StringUtils.EMPTY));
      String internalfolder = job.getProperty("internalfolder", Boolean.FALSE.toString());
      if(StringUtils.equals(Boolean.TRUE.toString(), internalfolder)){
        Map<String, Object> properties = new HashMap<>();
        properties.put("jcr:primaryType", "nt:unstructured");
        properties.put("sling:resourceType", "/apps/mediahub/components/renderconditions/groups");
        resolver.create(folder,"granite:rendercondition",properties);
      } else {
        resolver.delete(folder.getChild("granite:rendercondition"));
      }
      resolver.commit();
    } catch (LoginException e) {
      logger.error("Error while fetching system user {0}", e);
    } catch (PersistenceException e) {
      logger.error("Error while saving changes {0}", e);
    }

    return JobResult.OK;
  }


}
