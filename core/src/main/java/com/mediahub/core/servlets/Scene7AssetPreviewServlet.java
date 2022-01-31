package com.mediahub.core.servlets;

import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.Map;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * URL to view vidoejs html for interactive video preview
 *
 * http://localhost:4502/bin/mediahub/videoviewer.html?asset=/bnpparibasstage/SRE_video-12-0x1080-2200k&videoserverurl=/is/content/&serverUrl=/is/image/&aemmode=0&contentUrl=/is/content/&playback=progressive&posterimage=/bnpparibasstage/SRE_video-12-0x1080-2200k
 */
@SuppressWarnings("CQRules:CQBP-75")
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/videoviewer",
                "sling.servlet.extensions=" + "html"})
@ServiceDescription("Asset Preview Servlet ")
public class Scene7AssetPreviewServlet extends SlingSafeMethodsServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger logger = LoggerFactory.getLogger(Scene7AssetPreviewServlet.class);

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest req, final SlingHttpServletResponse resp)
            throws ServletException, IOException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
        try(ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String asset = req.getParameter("asset");
            String videoServerUrl = req.getParameter("videoserverurl");
            String serverUrl = req.getParameter("serverUrl");
            String posterImage = req.getParameter("posterimage");
            if (!StringUtils.isEmpty(asset) && !StringUtils.isEmpty(videoServerUrl) ) {
                String videoPath = videoServerUrl.substring(0, BnpConstants.IS_CONTENT.length());
                PrintWriter out = resp.getWriter();
                out.println("<html lang=\"en\">");
                out.println("<head> <link href=\"https://vjs.zencdn.net/7.17.0/video-js.css\" rel=\"stylesheet\" /> </head>");
                out.println("<body>");
                out.println("<video id=\"my-video\" class=\"video-js\" controls preload=\"auto\" poster=\"" + "/is/image"+  posterImage + "\"data-setup=\"{}\">");
                out.println("<source src=\"" + videoPath + asset + "\" type=\"video/mp4\" />");
                out.println("<p class=\"vjs-no-js\"> To view this video please enable JavaScript, and consider upgrading to a web browser that <a href=\"https://videojs.com/html5-video-support/\" target=\"_blank\">supports HTML5 video</a></p>");
                out.println("</video>");
                out.println("<script src=\"https://vjs.zencdn.net/7.17.0/video.min.js\"></script>");
                out.println("</body>");
                out.println("</html>");
                resp.setContentType("text/html");
                resp.setCharacterEncoding("UTF-8");
                out.flush();
            }
        } catch (LoginException e) {
            logger.error("Error when getting resource resolver", e);
        }

    }

}
