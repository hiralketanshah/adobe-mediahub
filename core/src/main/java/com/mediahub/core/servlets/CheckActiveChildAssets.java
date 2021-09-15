package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.google.gson.Gson;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

@Component(service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET,
                "sling.servlet.resourceTypes=" + "cq/Page", "sling.servlet.selectors=" + "active.asset",
                "sling.servlet.extensions=" + "json"})
@ServiceDescription("Check Active Child Asset")
public class CheckActiveChildAssets extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(CheckActiveChildAssets.class);
    private static final long serialVersionUID = 1L;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest request,
                         final SlingHttpServletResponse response) throws IOException {
        LOGGER.debug("Check Active Asset...");
        response.setContentType("application/json");
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            boolean hasAdminPrivileges = hasAdminPrivileges(request, adminResolver);
            Resource resource = adminResolver.getResource(request.getRequestParameter("paths").toString());
            if (resource != null && resource.hasChildren()) {
                Iterator<Resource> resources = resource.listChildren();
                while (resources.hasNext()) {
                    Resource childResource = resources.next();
                    if (childResource.getChild(JcrConstants.JCR_CONTENT) != null && childResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null) {
                        ValueMap metadata = childResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
                        if (checkChildAssetStatus(response, metadata, hasAdminPrivileges)) {
                            return;
                        }
                    }
                }
                Map<String, Object> responseMap = new HashMap<>();
                responseMap.put("hasActiveAsset", false);
                setJsonResponse(200, response, responseMap);
            } else {
                Map<String, Object> responseMap = new HashMap<>();
                responseMap.put("hasActiveAsset", false);
                setJsonResponse(200, response, responseMap);
            }
        } catch (RepositoryException | LoginException e) {
            LOGGER.error("repo error :", e);
        }
    }

    /**
     * Method to check whether user has admin privilege
     *
     * @param request
     * @param adminResolver
     * @return
     * @throws RepositoryException
     */
    private boolean hasAdminPrivileges(SlingHttpServletRequest request, ResourceResolver adminResolver)
            throws RepositoryException {
        boolean hasAdminPrivileges = false;
        UserManager userManager = adminResolver.adaptTo(UserManager.class);
        // administrators group
        Authorizable administrators = userManager.getAuthorizable("administrators");
        Group administratorsGroup = (Group) administrators;

        // mediahub-super-administrators group
        Authorizable superAdministrators = userManager.getAuthorizable("mediahub-super-administrators");
        Group superAdministratorsGroup = (Group) superAdministrators;

        // mediahub-administrators group
        Authorizable mediahubAdministrators = userManager.getAuthorizable("mediahub-administrators");
        Group mediahubAdministratorsGroup = (Group) mediahubAdministrators;

        Authorizable authorizable = userManager.getAuthorizable(request.getResourceResolver().getUserID());

        if (authorizable != null && !authorizable.isGroup()) {
            if ((((User) authorizable).isAdmin() || administratorsGroup.isDeclaredMember(authorizable))) {
                hasAdminPrivileges = true;
            }
            if (superAdministratorsGroup != null && superAdministratorsGroup.isDeclaredMember(authorizable)) {
                hasAdminPrivileges = true;
            }
            if (mediahubAdministratorsGroup != null && mediahubAdministratorsGroup.isDeclaredMember(authorizable)) {
                hasAdminPrivileges = true;
            }

        }
        return hasAdminPrivileges;
    }

    /**
     * Method to check the status of child Asset
     *
     * @param response
     * @param metadata
     * @return
     */
    private boolean checkChildAssetStatus(SlingHttpServletResponse response, ValueMap metadata, boolean hasAdminPrivileges) {
        try {
            if ((!metadata.containsKey(BnpConstants.BNPP_INTERNAL_FILE_URL) || StringUtils
                    .equals(metadata.get(BnpConstants.BNPP_INTERNAL_FILE_URL).toString(), StringUtils.EMPTY)) &&
                    (!metadata.containsKey(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL) || StringUtils.equals(metadata.get(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL).toString(), StringUtils.EMPTY))) {
                // Do nothing - for future requirements
            } else {
                Map<String, Object> responseMap = new HashMap<>();
                responseMap.put("hasAdminPrivilege", hasAdminPrivileges);
                setJsonResponse(400, response, responseMap);
                return true;
            }
        } catch (IOException e) {
            LOGGER.error("Exception while setting JSON response : {}",e);
        }
        return false;
    }

    /**
     * Method to set Json Response
     *
     * @param status
     * @param response
     * @param responseMap
     * @throws IOException
     */
    private void setJsonResponse(int status, SlingHttpServletResponse response, Map<String, Object> responseMap)
            throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        PrintWriter out = response.getWriter();
        if (out != null) {
            out.write(new Gson().toJson(responseMap));
            out.flush();
            out.close();
        }
    }

}
