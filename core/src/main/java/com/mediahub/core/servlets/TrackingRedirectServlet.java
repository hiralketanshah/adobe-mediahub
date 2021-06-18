package com.mediahub.core.servlets;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.Servlet;
import java.io.IOException;

import static org.apache.sling.api.servlets.ServletResolverConstants.*;

@Component(
        service = {Servlet.class},
        property = {
                SLING_SERVLET_RESOURCE_TYPES + "=mediahub/assets/tracking",
                SLING_SERVLET_METHODS + "=GET",
                SLING_SERVLET_EXTENSIONS + "=html",
        }
        // Registering multiple values simply requires multiple key/value pairs for the same key.
)
public class TrackingRedirectServlet extends SlingSafeMethodsServlet {
    private static final Logger log = LoggerFactory.getLogger(TrackingRedirectServlet.class);

    /**
     * Add overrides for other SlingSafeMethodsServlet here (doGeneric, doHead, doOptions, doTrace) *
     */

    @Override
    protected final void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) throws IOException {

        Resource asset = request.getResource();
        String redirect = asset.getValueMap().get("redirectTarget", String.class);

        if (!StringUtils.isEmpty(redirect)) {
            response.sendRedirect(redirect);
        } else {
            // Write a standard text/html response
            response.setContentType("text/html;charset=UTF-8");
            response.setCharacterEncoding("UTF-8");
            response.getWriter().write("<html><body>Unable to redirect asset</body></html>");
        }
    }
}