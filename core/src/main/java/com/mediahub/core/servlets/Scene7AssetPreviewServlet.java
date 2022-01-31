package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import com.mediahub.core.utils.AssetUtils;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Session;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
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

/**
 * URL to view vidoejs html for interactive video preview
 *
 * http://localhost:4502/bin/mediahub/videoviewer.html?uuid=3046347f-ea6b-4bf1-a9eb-31cc1229e458
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

    @Reference
    transient Scene7Service scene7Service;

    @Reference
    transient Scene7DeactivationService scene7DeactivationService;

    @Override
    protected void doGet(final SlingHttpServletRequest req, final SlingHttpServletResponse resp)
            throws ServletException, IOException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
        try(ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String uuid = req.getParameter("uuid");

            QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = new HashMap<>();
            map.put(BnpConstants.PATH, "/content/dam");
            map.put(BnpConstants.FIRST_PROPERTY, JcrConstants.JCR_UUID);
            map.put(BnpConstants.FIRST_PROPERTY_VALUE, uuid);
            map.put("p.limit", "-1");

            Query query = builder.createQuery(PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            Iterator<Resource> userResources = result.getResources();

            List<String> scene7Assets = new ArrayList<>();

            S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);

            userResources.forEachRemaining(resource -> {
                Asset asset = DamUtil.resolveToAsset(resource);
                String scene7File = DamUtil.resolveToAsset(resource).getMetadataValue("dam:scene7File");
                String scene7ID = DamUtil.resolveToAsset(resource).getMetadataValue("dam:scene7ID");

                String scene7MediumDefinition = AssetUtils.getVideoShareLinkId(s7Config, scene7Service, scene7ID);
                if(StringUtils.isNotEmpty(scene7File) && DamUtil.isVideo(asset)){
                    String environment = scene7File.split("/")[0];
                    scene7Assets.add(environment + "/" + scene7MediumDefinition);
                }
            });

            if (!scene7Assets.isEmpty()) {
                PrintWriter out = resp.getWriter();
                out.println("<html lang=\"en\">");
                out.println("<head> <link href=\"https://vjs.zencdn.net/7.17.0/video-js.css\" rel=\"stylesheet\" /> </head>");
                out.println("<body>");
                out.println("<video id=\"my-video\" width=\"640\" height=\"264\" class=\"video-js\" controls preload=\"auto\" poster=\"" + "/is/image/"+  scene7Assets.get(0) + "\" data-setup=\"{}\">");
                out.println("<source src=\"" + "/" +BnpConstants.IS_CONTENT + scene7Assets.get(0) + "\" type=\"video/mp4\" />");
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
