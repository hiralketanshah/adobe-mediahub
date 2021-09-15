package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.DamConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.io.IOUtils;
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

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;

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
            ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo);

            String contentPath = req.getParameter("content");

            String title = contentPath.split("/")[contentPath.split("/").length - 1];

            Resource metaDataResource = adminResolver.getResource(contentPath);
            if (metaDataResource != null) {
                Node jcrContentNode = getJcrContentNode(metaDataResource);
                Node metaDataNode = jcrContentNode.getNode("metadata");

                String mimeType = metaDataNode.getProperty(DamConstants.DC_FORMAT).getString();
                int index = mimeType.lastIndexOf("/");
                String imageVideo = null;
                imageVideo = mimeType.substring(0, index);

                if (imageVideo != null && "video".equals(imageVideo)) {
                    PrintWriter out = resp.getWriter();
                    out.println("<html lang=\"en\">");
                    out.println("<head><title>" + title
                            + "</title></head>");
                    out.println("<body>");
                    out.println("<video  style=\"width:100%; height:100%;\" >");
                    out.println("<source src=\"" + contentPath + "\">");
                    out.println("</video>");
                    out.println("</body></html>");
                    resp.setContentType("text/html");
                    resp.setCharacterEncoding("UTF-8");
                    out.flush();
                } else {
                    resp.setContentType(mimeType);
                    Asset asset = metaDataResource.adaptTo(Asset.class);
                    InputStream data = asset.getOriginal().getStream();
                    byte[] bytes = IOUtils.toByteArray(data);
                    resp.getOutputStream().write(bytes);
                }
            }
        } catch (LoginException | RepositoryException e) {
            logger.error("Error when getting resource", e);
        }

    }

    public Node getJcrContentNode(Resource resource) {
        Node titleNode = null;
        try {

            Node node = resource.adaptTo(Node.class);
            titleNode = node.getNode(JcrConstants.JCR_CONTENT);

        } catch (RepositoryException e) {
            logger.error("Error when getting jcr content", e);
        }

        return titleNode;
    }

}
