package com.mediahub.core.servlets;

import com.adobe.granite.ui.components.Config;
import com.adobe.granite.ui.components.rendercondition.RenderCondition;
import com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ProjectPermissionsUtil;
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

import javax.servlet.Servlet;
import java.util.Collections;
import java.util.Map;

@Component(service = Servlet.class, property = {
        Constants.SERVICE_DESCRIPTION + "= Custom RenderConditions Servlet",
        "sling.servlet.methods=" + HttpConstants.METHOD_GET,
        "sling.servlet.resourceTypes=" + "/apps/mediahub/components/renderconditions/projectmember"})
public class ProjectMemberRenderConditionServlet extends SlingSafeMethodsServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger logger = LoggerFactory.getLogger(ProjectMemberRenderConditionServlet.class);

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response) {
        boolean render = false;
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String[] groups = getPropertiesFromConfig(request);
            String projectDamPath = request.getRequestPathInfo().getSuffix();
            render = ProjectPermissionsUtil.isAuthorizedForProject(resourceResolver, projectDamPath, groups, request.getResourceResolver().getUserID());
        } catch (LoginException e) {
            logger.error("Error while fetching resolver object from system user", e);
        }
        request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(render));
    }

    protected String[] getPropertiesFromConfig(SlingHttpServletRequest request) {
        Config config = new Config(request.getResource());
        return config.get("groups", String[].class);
    }

}
