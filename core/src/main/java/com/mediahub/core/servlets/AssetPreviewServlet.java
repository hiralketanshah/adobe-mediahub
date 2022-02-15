package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.DamConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;

import static com.mediahub.core.constants.BnpConstants.IS_CONTENT;

@SuppressWarnings("CQRules:CQBP-75")
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/player",
                "sling.servlet.extensions=" + "jsp"})
@ServiceDescription("Asset Preview Servlet ")
public class AssetPreviewServlet extends SlingSafeMethodsServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger logger = LoggerFactory.getLogger(AssetPreviewServlet.class);

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest req, final SlingHttpServletResponse resp)
            throws ServletException, IOException {
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);

            String contentPath = req.getParameter("content");

            String title = contentPath.split("/")[contentPath.split("/").length - 1];

            Resource assetResource = resourceResolver.getResource(contentPath);
            Asset asset = DamUtil.resolveToAsset(assetResource);
            if (asset != null) {
                if (DamUtil.isVideo(asset)) {
                    Map<String, Object> metadata = assetResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
                    String videoUrl = metadata.getOrDefault(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_HD, "").toString();
                    if (!StringUtils.isEmpty(videoUrl)) {
                        String videoPath = videoUrl.substring(videoUrl.indexOf(IS_CONTENT) + IS_CONTENT.length());
                        PrintWriter out = resp.getWriter();
                        out.println("<html lang=\"en\">");
                        out.println("<head><title>" + title
                                + "</title></head>");
                        out.println("<body>");
                        out.println("<iframe src=\"/bin/mediahub/videoviewer.html?uuid=" + assetResource.getValueMap().get(JcrConstants.JCR_UUID, String.class)  + "\" frameborder=\"0\" allowfullscreen=\"\" style=\"width:100%;height:100%\"></iframe>");
                        out.println("</body></html>");
                        resp.setContentType("text/html");
                        resp.setCharacterEncoding("UTF-8");
                        out.flush();
                    }
                } else {
                    String mimeType = asset.getMetadataValue(DamConstants.DC_FORMAT);
                    resp.setContentType(mimeType);
                    InputStream data = asset.getOriginal().getStream();
                    byte[] bytes = IOUtils.toByteArray(data);
                    resp.getOutputStream().write(bytes);
                }
            }
        } catch (LoginException e) {
            logger.error("Error when getting resource", e);
        }

    }

}
