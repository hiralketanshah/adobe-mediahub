package com.mediahub.core.servlets;

import com.adobe.granite.ui.components.Config;
import com.adobe.granite.ui.components.rendercondition.RenderCondition;
import com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = Servlet.class, property = {
    Constants.SERVICE_DESCRIPTION + "= Custom RenderConditions Servlet",
    "sling.servlet.methods=" + HttpConstants.METHOD_GET,
    "sling.servlet.resourceTypes=" + "utils/granite/rendercondition/exclude/groups" })
public class ExcludeGroupRenderConditionServlet extends SlingSafeMethodsServlet {

  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(ExcludeGroupRenderConditionServlet.class);

  @Reference
  private transient ResourceResolverFactory resolverFactory;

  @Override
  protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response) {
    boolean render = true;
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    try(ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)){
      String[] groups = getPropertiesFromConfig(request);
      if(groups != null && groups.length > 0){
        UserManager userManager = adminResolver.adaptTo(UserManager.class);
        for (String group : groups) {
          if( userManager.getAuthorizable(group) != null && userManager.getAuthorizable(group).isGroup() && ((Group)userManager.getAuthorizable(group)).isMember((User)userManager.getAuthorizable(request.getResourceResolver().getUserID())) ){
            render = false;
          }
        }
      }
    } catch (LoginException e) {
      logger.error("Error while fetching resolver object from system user", e);
    } catch (RepositoryException e) {
      logger.error("Error while accessing repository", e);
    }
    request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(render));
  }

  protected String[] getPropertiesFromConfig(SlingHttpServletRequest request) {
    Config config = new Config(request.getResource());
    return config.get("groups", String[].class);
  }

}
