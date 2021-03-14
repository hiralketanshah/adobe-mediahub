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

  private int connectionTimeOut;

  private int socketTimeOut;

  private String scene7Endpoint;

  private String domainName;

  private String deactivationUser;

  @Override
  public int getConnectionTimeOut() {
    return connectionTimeOut;
  }

  @Override
  public int getSocketTimeOut() {
    return socketTimeOut;
  }

  @Override
  public String getScene7Endpoint() {
    return scene7Endpoint;
  }

  @Override
  public String getDomainName() {
    return domainName;
  }

  @Override
  public String getDeactivationUser() {
    return deactivationUser;
  }

  @Activate
  protected void activate(final Scene7DeactivationServiceConfig config) {
    connectionTimeOut = config.getConnectionTimeOut();
    socketTimeOut = config.getSocketTimeOut();
    scene7Endpoint = config.getScene7Endpoint();
    domainName = config.getDomainName();
    deactivationUser = config.getDeactivationUser();
  }
}

@ObjectClassDefinition(name = "Scene 7 Deactivation Service",
    description = "Scene 7 Deactivation Service")
@interface Scene7DeactivationServiceConfig {

  @AttributeDefinition(name = "Connection Timeout in Milli Seconds",
      description = "Connection Timeout in Milli Seconds for Scene 7 Deactivation")
  int getConnectionTimeOut() default 5000;

  @AttributeDefinition(name = "Socket Timeout in Milli Seconds",
      description = "Socket Timeout in Milli Seconds for Scene 7 Deactivation")
  int getSocketTimeOut() default 5000;

  @AttributeDefinition(name = "Scene 7 Deactivation End Point",
      description = "Scene 7 Deactivation End Point ")
  String getScene7Endpoint() default "/content/dam/medialibrary/mmeeddiiaa/cover1.dmpublish.json";

  @AttributeDefinition(name = "Domain Name",
      description = "Domain Name")
  String getDomainName() default "mediahub.bnpparibas";

  @AttributeDefinition(name = "Deactivation User",
      description = "Deactivation User")
  String getDeactivationUser() default "admin:admin";

}