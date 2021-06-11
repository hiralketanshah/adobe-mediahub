package com.mediahub.core.services.impl;

import com.mediahub.core.services.Scene7DeactivationService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;

@Component(service = Scene7DeactivationService.class)
@Designate(ocd = Scene7DeactivationServiceConfig.class)
public class Scene7DeactivationServiceImpl implements Scene7DeactivationService {

  private String configurationPath;

  private String cdnCacheInvalidationPath;

  @Activate
  protected void activate(final Scene7DeactivationServiceConfig config) {
    configurationPath = config.getCloudConfigurationPath();
    cdnCacheInvalidationPath = config.getCdnCacheInvalidationPath();
  }

  @Override
  public String getCloudConfigurationPath() {
    return configurationPath;
  }

  @Override
  public String getCdnCacheInvalidationPath() { return cdnCacheInvalidationPath; }
}

@ObjectClassDefinition(name = "Scene 7 Deactivation Service",
    description = "Scene 7 Deactivation Service")
@interface Scene7DeactivationServiceConfig {

  @AttributeDefinition(name = "Cloud Configuration Path",
      description = "Cloud Configuration Path")
  String getCloudConfigurationPath() default "/conf/global/settings/cloudconfigs/dmscene7";

  @AttributeDefinition(name = "CDN cache Invalidation request URL",
      description = "CDN cache Invalidation request URL")
  String getCdnCacheInvalidationPath() default "/content/mediahub/us/en.s7cdninvalidation.json";

}