package com.mediahub.core.servlets;

import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.eclipse.jetty.util.URIUtil;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(
    service = Servlet.class,
    property = { "sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.resourceTypes=" + "cq/Page",
            "sling.servlet.extensions=" + "jsp" })
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
            Resource resource = req.getResource();
            Node titleNode = getJcrContentNode(resource);
            String contentPath = req.getParameter("content");

            Resource metaDataResource = adminResolver.getResource(contentPath);
            Node jcrContentNode = getJcrContentNode(metaDataResource);
            Node metaDataNode = jcrContentNode.getNode("metadata");

            String mimeType = metaDataNode.getProperty("dc:format").getString();
            int index = mimeType.lastIndexOf("/");
            String imageVideo = null;
            imageVideo = mimeType.substring(0, index);
            logger.debug("Type of asset MIMETYPE  $$$$$$ ------ : {} ", imageVideo);

            resp.setContentType("text/html");
            resp.setCharacterEncoding("UTF-8");

            PrintWriter out = resp.getWriter();
            if (imageVideo != null && "image".equals(imageVideo)) {
                out.println("<html lang=\"en\">");
                out.println("<head><title>" + titleNode.getProperty(BnpConstants.JCR_TITLE).getString()
                        + "</title></head>");
                out.println("<body>");
                out.println("<img src=" + URIUtil.encodePath(contentPath) + " style=\"width:100%; height:100%;\" >");
                out.println("</img>");
                out.println("</body></html>");
                out.flush();
            } else if (imageVideo != null && "video".equals(imageVideo)) {

                out.println("<html lang=\"en\">");
                out.println("<head><title>" + titleNode.getProperty(BnpConstants.JCR_TITLE).getString()
                        + "</title></head>");
                out.println("<body>");
                out.println("<video  style=\"width:100%; height:100%;\" controls>");
                out.println("<source src=" + contentPath + ">");
                out.println("</video>");
                out.println("</body></html>");
                out.flush();
            }
        } catch (PathNotFoundException e) {

            logger.debug("Excetion occured when No Path found  $$$$$$ ------ : {} ", e.getMessage());
        } catch (RepositoryException e) {
            logger.debug("Repository Exception $$$$$$ ------ : {} ", e.getMessage());
        } catch (LoginException e) {
            logger.debug("Exception ocuured during Authentication $$$$$$ ------ : {} ", e.getMessage());
        }

    }

    public Node getJcrContentNode(Resource resource) {
        Node titleNode = null;
        try {

            Node node = resource.adaptTo(Node.class);
            titleNode = node.getNode("jcr:content");

        } catch (PathNotFoundException e) {
            logger.debug("Excetion occured when No Path found  $$$$$$ ------ : {} ", e.getMessage());
        } catch (RepositoryException e) {
            logger.debug("Repository Exception $$$$$$ ------ : {} ", e.getMessage());
        }

        return titleNode;
    }

}
