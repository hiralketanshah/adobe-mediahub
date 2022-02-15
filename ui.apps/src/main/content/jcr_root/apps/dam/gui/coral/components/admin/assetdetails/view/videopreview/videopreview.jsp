<%@page session="false"
        import="java.util.Iterator,
com.adobe.granite.ui.components.Config,
				com.day.cq.dam.api.Asset,
                com.day.cq.dam.api.Rendition,
				com.day.cq.dam.commons.util.DynamicMediaServicesConfigUtil,
                com.day.cq.dam.commons.util.UIHelper,
				com.day.cq.dam.entitlement.api.EntitlementConstants,
				org.apache.commons.lang.StringUtils,
                org.apache.jackrabbit.util.Text,
				org.apache.sling.api.resource.Resource,
				org.apache.sling.api.resource.ResourceResolver,
                org.apache.sling.api.resource.ValueMap,
                org.apache.sling.featureflags.Features,
                com.day.cq.dam.scene7.api.S7Config,
                com.day.cq.dam.scene7.api.Scene7Service,
                com.mediahub.core.utils.AssetUtils,
                com.mediahub.core.services.Scene7DeactivationService,
                javax.jcr.Node, com.adobe.granite.ui.components.ComponentHelper, com.adobe.granite.ui.components.ExpressionHelper"%>
<%@taglib prefix="sling" uri="http://sling.apache.org/taglibs/sling/1.0"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0" %><%
%><%@taglib prefix="ui" uri="http://www.adobe.com/taglibs/granite/ui/1.0"%><%
%><%
%><cq:defineObjects />
<%
    Resource currentResource = UIHelper.getCurrentSuffixResource(slingRequest);
    ComponentHelper cmp = new ComponentHelper(pageContext);
    ExpressionHelper ex = cmp.getExpressionHelper();
    Node childNode = currentResource.adaptTo(Node.class);
    Asset asset = currentResource.adaptTo(Asset.class);
    Config cfg = new Config(resource);
    String className = cfg.get("class",String.class);
    className +=" video-js vjs-default-skin vjs-big-play-centered";
    String renditionPath = "";
    String renditionMimeType = "";
    String  requestPrefix = cfg.get("com.adobe.cq.item.requestPrefix", "");
    String  requestSuffix = ex.get(cfg.get("com.adobe.cq.item.requestSuffix", ""), String.class);
    boolean allowOriginal = ((null == request.getAttribute("allowOriginal")) ? true : ((Boolean)request.getAttribute("allowOriginal")));

    String childPath = childNode.getPath();
    long ck = 0;
    if(asset == null) {
        //This is the case when we have passed it using item parameter
        //TODO Ideally we will have only one way to do this.
        String contentPath = request.getParameter("item");
        asset = slingRequest.getResourceResolver().getResource(contentPath).adaptTo(Asset.class);
        childNode = slingRequest.getResourceResolver().getResource(contentPath).adaptTo(Node.class);
        childPath = childNode.getPath();
        ck = UIHelper.getCacheKiller(childNode);
    }
    ck = UIHelper.getCacheKiller(childNode);
    String thumbnailUrl ="";
    Rendition thumbnailRendition = UIHelper.getBestfitRendition(asset, 319);
    if(thumbnailRendition != null) {
        //default thumbnail
        thumbnailUrl = asset.getPath() + ".thumb.319.319.png";
    }

    ResourceResolver resolver = slingRequest.getResourceResolver();
    boolean isDynamicMediaEnabled = false;
    final Features featureManager = sling.getService(Features.class);
    if (featureManager.getFeature(EntitlementConstants.ASSETS_DYNAMICMEDIA_FEATURE_FLAG_PID)!=null &&
            featureManager.isEnabled(EntitlementConstants.ASSETS_DYNAMICMEDIA_FEATURE_FLAG_PID)) {
        isDynamicMediaEnabled = true;
    }

    for (Iterator<Rendition> it = asset.listRenditions(); it.hasNext();) {
        Rendition rendition = it.next();
        //if original rendition is not allowed in link share, don't set it as source
        if(!allowOriginal && "original".equals(rendition.getName())){
            continue;
        }
        // check if its a proxy rendition by its proxyurl. So making sure the amazon video will work
        String proxyUrl = null;
        if (isDynamicMediaEnabled) {
            proxyUrl = getProxyRenditionProperty(rendition, "dam:proxyUrl");
        }

        if (StringUtils.isNotEmpty (proxyUrl)) {
            String serverUrl = DynamicMediaServicesConfigUtil.getServiceUrl(resolver);
            renditionPath = serverUrl + "private/" + DynamicMediaServicesConfigUtil.getRegistrationId(resolver) + proxyUrl;
            renditionMimeType = rendition.getMimeType();
            thumbnailUrl = request.getContextPath()+ requestPrefix + "/is/image" + Text.escapePath(asset.getPath());

        } else {
            if (rendition.getMimeType().equals("video/ogg")) {
                renditionPath = request.getContextPath() + requestPrefix + Text.escapePath(rendition.getPath()) + "?ch_ck=" + ck + requestSuffix;
                renditionMimeType = "video/ogg";
            } else if (rendition.getMimeType().equals("video/mp4")) {
                renditionPath = request.getContextPath() + requestPrefix + Text.escapePath(rendition.getPath()) + "?ch_ck=" + ck + requestSuffix;
                renditionMimeType = "video/mp4";
            } else if (rendition.getMimeType().startsWith("video") && rendition.getMimeType().endsWith("m4v")) {
                // handling mime types video/m4v, video/x-m4v
                renditionPath = request.getContextPath() + requestPrefix + Text.escapePath(rendition.getPath()) + "?ch_ck=" + ck + requestSuffix;
                renditionMimeType = "video/x-m4v"; // to make sure standard mime type goes to HTML5 video player
            }
        }

    }

    String mimeType = asset.getMimeType();
    if (mimeType.equals("application/x-shockwave-flash")) {
%>
<object class="asset-detail-views-image" data="<%= xssAPI.encodeForHTMLAttr(childPath) %>" style="width: 100%; height: 100%;"><param name="wmode" value="transparent"></object>


<%} else {

    String scene7File = asset.getMetadataValue("dam:scene7File");
    String scene7ID = asset.getMetadataValue("dam:scene7ID");
    Scene7Service scene7Service = sling.getService(Scene7Service.class);
    Scene7DeactivationService scene7DeactivationService = sling.getService(Scene7DeactivationService.class);
    S7Config s7Config = resource.getResourceResolver().getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
    String assetMediumRendition = AssetUtils.getVideoShareLinkId(s7Config, scene7Service, scene7ID);
    String iframeUrl = "/etc/dam/viewers/s7viewers/html5/VideoViewer.html?asset=/" + scene7File + "&amp;videoserverurl=/is/content/&amp;serverUrl=/is/image/&amp;aemmode=0&amp;contentUrl=/is/content/&amp;playback=progressive&amp;posterimage=/" + scene7File;
    if(StringUtils.isNotEmpty(assetMediumRendition) && StringUtils.isNotEmpty(scene7File)){
        String environment = scene7File.split("/")[0];
        assetMediumRendition = environment + "/" + assetMediumRendition;
        iframeUrl = "/etc/dam/viewers/s7viewers/html5/VideoViewer.html?asset=/" + assetMediumRendition + "&amp;videoserverurl=/is/content/&amp;serverUrl=/is/image/&amp;aemmode=0&amp;contentUrl=/is/content/&amp;playback=progressive&amp;posterimage=/" + assetMediumRendition;
    }

%>

<iframe src="<%= iframeUrl %>" frameborder="0" allowfullscreen="" style="width:100%; height:800px"></iframe>

<% if(!cfg.get("useHtml5", false)) { %>
<cq:includeClientLib categories="dam.gui.coral.videopicker" />
<% } %>
<% }%>

<%!
    private String getProxyRenditionProperty(Rendition rendition, String propName){
        final ValueMap map = rendition.getProperties();
        String propVal = null;
        if (map != null){
            propVal = map.get(propName, "");
        }

        return propVal;
    }
%>
