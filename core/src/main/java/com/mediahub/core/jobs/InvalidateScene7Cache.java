package com.mediahub.core.jobs;

import com.day.cq.contentsync.handler.util.RequestResponseFactory;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.wcm.api.WCMMode;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.engine.SlingRequestProcessor;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = JobConsumer.class, immediate = true,
    property = {Constants.SERVICE_DESCRIPTION + "=Replication Job In NT",
        JobConsumer.PROPERTY_TOPICS + "=" + "scene7/cache/invalidation"})
/**
 * This job will invalidate scene7 cache
 * JOB.
 *
 */
public class InvalidateScene7Cache implements JobConsumer {

  private static final Logger LOGGER = LoggerFactory.getLogger(InvalidateScene7Cache.class);

  @Reference
  ResourceResolverFactory resolverFactory;

  @Reference
  Scene7DeactivationService scene7DeactivationService;

  @Reference
  SlingRequestProcessor requestProcessor;

  @Reference
  RequestResponseFactory requestResponseFactory;

  @Override
  public JobResult process(Job job) {
    String payload = job.getProperty("offloading.input.payload").toString();

    if(StringUtils.isBlank(payload)){
      return JobConsumer.JobResult.OK;
    }

    if(payload.contains("|")){
      payload = job.getProperty("offloading.input.payload").toString().split("\\|")[0];
    }

    LOGGER.debug("Asset path {}", payload);

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      Resource payloadResource = resourceResolver.resolve(payload);

      Asset asset = DamUtil.resolveToAsset(payloadResource);
      String scene7Path = asset.getMetadata().getOrDefault(BnpConstants.BNPP_EXTERNAL_FILE_URL, StringUtils.EMPTY).toString();
      if(StringUtils.isNotBlank(scene7Path)){
        Map<String, Object> params = new HashMap<>();
        params.put("urls", scene7Path);

        HttpServletRequest req = requestResponseFactory.createRequest("POST", scene7DeactivationService.getCdnCacheInvalidationPath(), params);
        WCMMode.DISABLED.toRequest(req);

        //Setup response
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        HttpServletResponse resp = requestResponseFactory.createResponse(out);

        //Process request through Sling
        requestProcessor.processRequest(req, resp, resourceResolver);
        String html = out.toString();
        LOGGER.info("Cdn cache response : " + html);
      }

    } catch (LoginException | ServletException | IOException e) {
      LOGGER.error("Error while Scene 7 cache invalidation", e);
      return JobResult.FAILED;
    }
    return JobConsumer.JobResult.OK;
  }
}
