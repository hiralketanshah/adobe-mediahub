package com.mediahub.core.models;

import com.adobe.cq.export.json.ComponentExporter;
import java.security.Principal;
import java.util.Collections;
import java.util.Map;
import javax.annotation.PostConstruct;
import javax.jcr.RepositoryException;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.models.annotations.DefaultInjectionStrategy;
import org.apache.sling.models.annotations.Exporter;
import org.apache.sling.models.annotations.Model;
import org.apache.sling.models.annotations.injectorspecific.OSGiService;
import org.apache.sling.models.annotations.injectorspecific.Self;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Model(adaptables = {SlingHttpServletRequest.class, Resource.class} ,
    adapters = {PrivacyPolicy.class, ComponentExporter.class},
    resourceType = PrivacyPolicy.RESOURCE_TYPE,
    defaultInjectionStrategy = DefaultInjectionStrategy.OPTIONAL)
@Exporter(name = "jackson", extensions = "json")
public class PrivacyPolicy implements ComponentExporter {

  static final String RESOURCE_TYPE = "mediahub/components/privacypolicy";

  private static final Logger LOGGER = LoggerFactory.getLogger(PrivacyPolicy.class);

  @Self
  private SlingHttpServletRequest request;

  @OSGiService
  private ResourceResolverFactory resolverFactory;

  final Map<String, Object> authInfo = Collections
      .singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");

  private String language;

  private String currentUserPath;

  @PostConstruct
  protected void init() {
    Principal userPrincipal = request.getUserPrincipal();
    try(ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      Authorizable authorizable = resolver.adaptTo(UserManager.class).getAuthorizable(userPrincipal);
      if(authorizable.getProperty("./preferences/language") != null && authorizable.getProperty("./preferences/language").length > 0){
        this.language = authorizable.getProperty("./preferences/language")[0].getString();
        LOGGER.info("{0} language Preference is ", userPrincipal.getName(), this.language);
        this.currentUserPath = authorizable.getPath();
      } else {
        this.language = "en";
        this.currentUserPath = StringUtils.EMPTY;
      }

    } catch (LoginException e) {
      LOGGER.error("Error while fecthing system user : {0}", e);
    } catch (RepositoryException e) {
      LOGGER.error("Error while fecthing current user : {0}", e);
    }

  }

  @Override
  public String getExportedType() {
    return RESOURCE_TYPE;
  }

  public String getLanguage() {
    return language;
  }

  public String getCurrentUserPath() {
    return currentUserPath;
  }

}
