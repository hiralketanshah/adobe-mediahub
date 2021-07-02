package com.mediahub.core.utils;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.event.jobs.JobManager;

public class AssetUtils {

  /**
   * @param resourceResolver   - Resolver object
   * @param asset         - Assest moved from projects to medialibrary
   * @param scene7DeactivationService - value map containing properties
   */
  public static void slingJobForActivateOrDeactiveAsset(ResourceResolver resourceResolver, Resource asset,
      Scene7DeactivationService scene7DeactivationService, JobManager jobManager, Scene7Service scene7Service, String action) {
    S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);

    Resource metadata = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
    ModifiableValueMap modifiableValueMap = metadata.adaptTo(ModifiableValueMap.class);

    List<Scene7Asset> scene7Assets = scene7Service.getAssets(new String[]{modifiableValueMap.get("dam:scene7ID", StringUtils.EMPTY)}, null, null, s7Config);

    for (Scene7Asset scene7Asset : scene7Assets) {
        final Map<String, Object> props = new HashMap<String, Object>();
        props.put("action", action);
        props.put("path", asset.getPath());
        props.put("user", "");
        jobManager.addJob("dam/scene7/asset/activation", props);
    }
  }

}
