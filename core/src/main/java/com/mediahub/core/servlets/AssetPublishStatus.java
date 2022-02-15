package com.mediahub.core.servlets;

import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.google.gson.Gson;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.lang3.StringUtils;
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

import javax.servlet.Servlet;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

@SuppressWarnings("CQRules:CQBP-75")
@Component(service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET,
                "sling.servlet.paths=" + "/bin/mediahub/asset/processed"})
@ServiceDescription("Check processed state of Assets")
public class AssetPublishStatus extends SlingAllMethodsServlet {

    public static final String IS_IN_RUNNING_WORKFLOW = "isInRunningWorkflow";
    private static final Logger LOGGER = LoggerFactory.getLogger(AssetPublishStatus.class);
    private static final long serialVersionUID = 1L;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest request,
                         final SlingHttpServletResponse response) throws IOException {
        LOGGER.debug("Check Active Asset...");
        response.setContentType("application/json");
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        Map<String, Object> responseMap = new HashMap<>();
        try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String assetPath = request.getRequestParameter("paths").getString();
            Resource resource = adminResolver.getResource(assetPath);
            if (DamUtil.isAsset(resource)) {
                Asset asset = DamUtil.resolveToAsset(resource);
                externalPublicationProcessed(responseMap, asset);
            } else {
                Iterator<Asset> assets = DamUtil.getAssets(resource);
                while (assets.hasNext()) {
                    Asset subAsset = assets.next();
                    externalPublicationProcessed(responseMap, subAsset);
                    if (responseMap.containsKey(IS_IN_RUNNING_WORKFLOW)) {
                        break;
                    }
                }
            }
            if (!responseMap.containsKey(IS_IN_RUNNING_WORKFLOW)) {
                responseMap.put(IS_IN_RUNNING_WORKFLOW, Boolean.FALSE);
            }
        } catch (LoginException e) {
            responseMap.put(IS_IN_RUNNING_WORKFLOW, Boolean.FALSE);
            LOGGER.error("repo error :", e);
        }
        setJsonResponse(200, response, responseMap);
    }

    private void externalPublicationProcessed(Map<String, Object> responseMap, Asset subAsset) {
        Resource metadataResource = subAsset.adaptTo(Resource.class).getChild("jcr:content/metadata");
        ValueMap properties = ResourceUtil.getValueMap(metadataResource);
        String broadcastStatus = properties.get(BnpConstants.BNPP_BROADCAST_STATUS, StringUtils.EMPTY);
        String scene7FileStatus = subAsset.getMetadataValueFromJcr(BnpConstants.S7_FILE_STATUS_PROPERTY);

        if (!(StringUtils.equals(scene7FileStatus, BnpConstants.S7_FILE_STATUS_COMPLETE) || StringUtils.equals(scene7FileStatus, BnpConstants.S7_FILE_STATUS_INCOMPLETE) || StringUtils.equals(scene7FileStatus, BnpConstants.S7_FILE_STATUS_NOT_SUPPORTED))) {
            responseMap.put(IS_IN_RUNNING_WORKFLOW, Boolean.TRUE);
        }
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
