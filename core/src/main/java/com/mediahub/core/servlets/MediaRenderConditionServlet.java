package com.mediahub.core.servlets;

import com.adobe.granite.ui.components.rendercondition.RenderCondition;
import com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
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
    "sling.servlet.resourceTypes=" + "utils/granite/rendercondition/simple/media" })
public class MediaRenderConditionServlet extends SlingSafeMethodsServlet {

  private static final long serialVersionUID = 1L;

  private static final Logger logger = LoggerFactory.getLogger(MediaRenderConditionServlet.class);

  @Reference
  private transient ResourceResolverFactory resolverFactory;

  @Override
  protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response)
      throws ServletException, IOException {
    boolean render = false;
    String assetPath = request.getRequestPathInfo().getSuffix();
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    try(ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)){
      if(StringUtils.isNotBlank(assetPath)){
        Resource asset = adminResolver.getResource(assetPath);
        if(null != asset.getChild(JcrConstants.JCR_CONTENT) && null != asset.getChild(JcrConstants.JCR_CONTENT).getChild("metadata")){
          String isMedia = asset.getChild(JcrConstants.JCR_CONTENT).getChild("metadata").getValueMap().get("bnpp-media", Boolean.FALSE.toString());
          render = StringUtils.equals(isMedia, Boolean.TRUE.toString());
        }
      }
    } catch (LoginException e) {
      logger.error("Error while fetching resolver object from system user", e);
    }
    request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(render));
  }

}
