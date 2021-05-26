package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.util.Iterator;
import javax.servlet.Servlet;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET,
                "sling.servlet.resourceTypes=" + "cq/Page", "sling.servlet.selectors=" + "active.asset",
                "sling.servlet.extensions=" + "json"})
@ServiceDescription("Check Active Child Asset")
public class CheckActiveChildAssets extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(CheckActiveChildAssets.class);

    private static final long serialVersionUID = 1L;

    @Override
    protected void doGet(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws IOException {
        LOGGER.debug("Check Active Asset...");
        response.setContentType("text/plain");
        Resource resource = request.getResourceResolver().getResource(request.getRequestParameter("paths").toString());
        if(resource != null && resource.hasChildren()){
            Iterator<Resource> resources = resource.listChildren();
            while(resources.hasNext()){
                Resource childResource = resources.next();
                if(childResource.getChild(JcrConstants.JCR_CONTENT) != null && childResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null){
                    ValueMap metadata = childResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
                    if (checkChildAssetStatus(response, metadata)) {
                        return;
                    }

                }
            }
            response.setStatus(200);
            response.getWriter().write("No Active Asset");
        } else {
            response.setStatus(200);
            response.getWriter().write("No Active Asset");
        }
    }

    /**
     * Method to check the status of child Asset
     *
     * @param response
     * @param metadata
     * @return
     */
    private boolean checkChildAssetStatus(SlingHttpServletResponse response, ValueMap metadata) {
        try {
            if((!metadata.containsKey("bnpp-internal-file-url") || StringUtils
                .equals(metadata.get("bnpp-internal-file-url").toString(), StringUtils.EMPTY)) &&
                (!metadata.containsKey("bnpp-external-file-url") || StringUtils.equals(metadata.get("bnpp-external-file-url").toString(), StringUtils.EMPTY))){
                // Do nothing - for future requirements
            } else {
                response.setStatus(400);
                response.getWriter().write("Has Childeren");
                return true;
            }
        } catch (IOException e) {
            LOGGER.error(e.getMessage());
        }
        return false;
    }

}
