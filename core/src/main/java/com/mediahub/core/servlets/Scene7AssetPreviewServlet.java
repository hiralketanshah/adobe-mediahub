package com.mediahub.core.servlets;

import com.day.cq.commons.LanguageUtil;
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
import com.mediahub.core.utils.QueryUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Session;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

/**
 * URL to view vidoejs html for interactive video experience
 * <p>
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
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String uuid = req.getParameter("uuid");

            QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = QueryUtils.getPredicateMapNodeByUUID("/content/dam", uuid);
            Query query = builder.createQuery(PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            Iterator<Resource> userResources = result.getResources();
            List<Resource> scene7Assets = new ArrayList<>();

            S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
            userResources.forEachRemaining(resource -> {
                scene7Assets.add(resource);
            });

            if (!scene7Assets.isEmpty()) {
                Resource assetResource = scene7Assets.get(0);
                Asset asset = DamUtil.resolveToAsset(assetResource);
                if (DamUtil.isVideo(asset)) {
                    String scene7File = asset.getMetadataValue("dam:scene7File");
                    String scene7ID = asset.getMetadataValue("dam:scene7ID");
                    String scene7MediumDefinition = AssetUtils.getVideoShareLinkId(s7Config, scene7Service, scene7ID);

                    Resource metadataResource = assetResource.getChild("jcr:content/metadata");
                    ValueMap properties = ResourceUtil.getValueMap(metadataResource);

                    String fileName = getScene7FileName(asset, scene7File, scene7MediumDefinition);

                    PrintWriter out = resp.getWriter();
                    out.println("<html lang=\"en\">");
                    out.println("<head> <link href=\"https://vjs.zencdn.net/7.17.0/video-js.css\" rel=\"stylesheet\" /> </head>");
                    out.println("<body>");
                    out.println("<video id=\"my-video\" class=\"video-js vjs-16-9 vjs-fluid vjs-big-play-centered\" controls preload=\"auto\" poster=\"" + "/is/image/" + fileName + "\" data-setup=\"{}\">");

                    if (properties.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)) {
                        String[] status = AssetUtils.getBroadcastStatus(properties, BnpConstants.BNPP_BROADCAST_STATUS);
                        if (Arrays.asList(status).contains(BnpConstants.EXTERNAL)) {
                            String domain = properties.get(BnpConstants.S7_DOMAIN_PROPERTY, String.class);
                            out.println("<source src=\"" + domain + BnpConstants.IS_CONTENT + fileName + "\" type=\"video/mp4\" />");
                        } else {
                            out.println("<source src=\"" + "/" + BnpConstants.IS_CONTENT + fileName + "\" type=\"video/mp4\" />");
                        }
                    } else {
                        out.println("<source src=\"" + "/" + BnpConstants.IS_CONTENT + fileName + "\" type=\"video/mp4\" />");
                    }
                    setSubtitleHtmlTags(assetResource, out);
                    out.println("<p class=\"vjs-no-js\"> To view this video please enable JavaScript, and consider upgrading to a web browser that <a href=\"https://videojs.com/html5-video-support/\" target=\"_blank\">supports HTML5 video</a></p>");
                    out.println("</video>");
                    out.println("<script src=\"https://vjs.zencdn.net/7.17.0/video.min.js\"></script>");
                    out.println("</body>");
                    out.println("</html>");
                    resp.setContentType("text/html");
                    resp.setCharacterEncoding("UTF-8");
                    out.flush();
                }
            }
        } catch (LoginException e) {
            logger.error("Error when getting resource resolver", e);
        }

    }

    /**
     * Method to add track tags for video - Subtitles
     *
     * @param assetResource
     * @param out
     */
    private void setSubtitleHtmlTags(Resource assetResource,
                                     PrintWriter out) {
        if ((assetResource.getChild(JcrConstants.JCR_CONTENT) != null) && (assetResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null)) {
            String videoName = assetResource.getName().substring(0, assetResource.getName().lastIndexOf('.'));
            ValueMap metadataProperties = assetResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
            if (StringUtils.equals(metadataProperties.get("bnpp-subtitles", "no"), "yes")) {
                String[] subtitleLanguages = metadataProperties.get("bnpp-subtitlelanguage", new String[]{});
                assetResource.getParent().listChildren().forEachRemaining(resource -> {
                    if (DamUtil.isAsset(resource)) {
                        String childName = resource.getName().substring(0, resource.getName().lastIndexOf('.'));
                        Arrays.asList(subtitleLanguages).forEach(language -> {
                            if ((videoName + "_" + language).equalsIgnoreCase(childName)) {
                                out.println("<track label=\"" + LanguageUtil.getLocale(language).getDisplayLanguage(new Locale("fr")) + "\" kind=\"subtitles\" srclang=\"" + language.toLowerCase() + "\" src=\"" + resource.getPath() + "\">");
                            }
                        });
                    }
                });
            }
        }
    }

    /**
     * Method to fetch file name to be placed in src of video tag
     *
     * @param asset
     * @param scene7File
     * @param scene7MediumDefinition
     * @return
     */
    private String getScene7FileName(Asset asset, String scene7File, String scene7MediumDefinition) {
        String fileName = StringUtils.EMPTY;
        if (StringUtils.isNotEmpty(scene7File) && DamUtil.isVideo(asset)) {
            String environment = scene7File.split("/")[0];
            fileName = environment + "/" + scene7MediumDefinition;
        }
        return fileName;
    }

}
