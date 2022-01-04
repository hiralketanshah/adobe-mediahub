package com.mediahub.core.servlets;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.eclipse.jetty.util.URIUtil;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.Servlet;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.StringJoiner;

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
            String domain;
            String uri;
            String query = null;
            if (redirect.indexOf("?") != -1) {
                query = redirect.substring(redirect.indexOf("?") + 1);
            }
            if (redirect.startsWith("http")) {
                domain = redirect.substring(0, redirect.indexOf("/", redirect.indexOf("://") + 3));
                uri = redirect.substring(domain.length() + 1, redirect.indexOf("?") != -1 ? redirect.indexOf("?") : redirect.length());
            } else {
                domain = "";
                uri = redirect;
            }
            String[] uriParts = uri.split("/");
            StringJoiner joiner = new StringJoiner("/");
            for (String part : uriParts) {
                joiner.add(URIUtil.encodePath(part));
            }
            String url = domain + "/" + joiner.toString();
            if (!StringUtils.isEmpty(query)) {
                url = url + "?";
                final StringJoiner joinerQuery = new StringJoiner("&");
                for (Map.Entry<String, String> entry : getQueryMap(query).entrySet()) {
                    joinerQuery.add(entry.getKey() + "=" + URIUtil.encodePath(entry.getValue()));
                }
                url = url + joinerQuery.toString();
            }
            response.sendRedirect(url);
        } else {
            // Write a standard text/html response
            response.setContentType("text/html;charset=UTF-8");
            response.setCharacterEncoding("UTF-8");
            response.getWriter().write("<html><body>Unable to redirect asset</body></html>");
        }
    }

    //Does work for multivalued parameters
    public static Map<String, String> getQueryMap(String query) {
        String[] params = query.split("&");
        Map<String, String> map = new HashMap<String, String>();
        for (String param : params) {
            String name = param.split("=")[0];
            String value = param.split("=")[1];
            map.put(name, value);
        }
        return map;
    }
}