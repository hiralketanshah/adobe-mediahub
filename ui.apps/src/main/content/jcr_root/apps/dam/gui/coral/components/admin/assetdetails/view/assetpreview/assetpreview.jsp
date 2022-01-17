<%@page session="false"%>
<%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/base.jsp"%>
<%@page session="false"
        import="com.adobe.granite.ui.components.AttrBuilder,
                com.adobe.granite.ui.components.ComponentHelper,
                com.adobe.granite.ui.components.ExpressionHelper,
                com.adobe.granite.ui.components.Config,
                com.adobe.granite.xss.XSSAPI,
                com.day.cq.commons.jcr.JcrConstants,
                com.day.cq.dam.api.Asset,
                com.day.cq.dam.api.Rendition,
                com.day.cq.dam.commons.util.UIHelper,
                com.day.cq.dam.commons.util.S73DHelper,
                org.apache.sling.featureflags.Features,
                com.day.cq.i18n.I18n,
                com.day.cq.wcm.api.Page,
                org.apache.jackrabbit.util.Text,
                org.apache.sling.api.resource.Resource,
                org.apache.sling.api.resource.ValueMap,
                org.apache.sling.api.resource.ResourceResolver,
                org.apache.commons.lang.StringUtils,
                javax.jcr.Node,
                javax.jcr.NodeIterator,
                javax.jcr.Property,
                java.util.Arrays,
                com.day.cq.dam.commons.util.DamUtil,
                java.util.Iterator,
                javax.jcr.security.AccessControlManager,
                javax.jcr.lock.LockManager,
                org.apache.jackrabbit.api.security.user.User,
                javax.jcr.security.Privilege,
                javax.jcr.Session,
                com.day.cq.dam.api.s7dam.constants.S7damConstants,
				        com.adobe.granite.ui.components.AttrBuilder,
                java.util.Collection,
                java.util.List,
                java.util.Set,
                java.util.HashSet,
				        java.util.Calendar,
                com.day.cq.dam.api.checkout.AssetCheckoutService,
                org.apache.jackrabbit.api.security.user.Authorizable,
                com.adobe.granite.security.user.util.AuthorizableUtil,
                com.adobe.dam.print.ids.StringConstants,
                com.mediahub.core.utils.AssetUtils,
                com.mediahub.core.services.Scene7DeactivationService,
                com.day.cq.dam.scene7.api.S7Config,
                com.day.cq.dam.scene7.api.Scene7Service"%>
<%@taglib prefix="sling" uri="http://sling.apache.org/taglibs/sling/1.0"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0" %><%
%><%@taglib prefix="ui" uri="http://www.adobe.com/taglibs/granite/ui/1.0"%><%
%><cq:defineObjects />

<%
    I18n i18n = new I18n(slingRequest);
    ComponentHelper cmp = new ComponentHelper(pageContext);
    ExpressionHelper ex = cmp.getExpressionHelper();

    AccessControlManager acm = resourceResolver.adaptTo(Session.class).getAccessControlManager();
    String assetDetailsVanity = "/assetdetails.html";
    ValueMap contentVm = resource.adaptTo(ValueMap.class);
    if (null != contentVm) {
        assetDetailsVanity = contentVm.get("assetDetailsVanity", assetDetailsVanity);
        request.setAttribute ("assetDetailsVanity",  assetDetailsVanity);
        String requestPrefix = contentVm.get("com.adobe.cq.item.requestPrefix", "");
        String requestSuffix = ex.get(contentVm.get("com.adobe.cq.item.requestSuffix", ""), String.class);
        request.setAttribute ("requestPrefix",  requestPrefix);
        request.setAttribute ("requestSuffix",  requestSuffix);
    }
	boolean isVideo = false;

//todo: Get the title from the contet/dam/* node
//Page contentPage = slingRequest.getResourceResolver().getResource(contentPath).adaptTo(Asset.class);
    Resource currentResource = UIHelper.getCurrentSuffixResource(slingRequest);
    if (currentResource == null) {
        return;
    }
    Node childNode = currentResource.adaptTo(Node.class);
    if(!childNode.isNodeType("dam:Asset")) {
        return;
    }

    Asset asset = currentResource.adaptTo(Asset.class);
    String subAssetPath = "";

    Set<String> suppporteTypes = new HashSet<String>();
    //Populate HashMap with mappings for File mimeType vs First Page Name
    suppporteTypes.add("application/pdf");
    suppporteTypes.add("application/x-indesign");
    suppporteTypes.add("application/postscript");
    suppporteTypes.add("application/vnd.openxmlformats-officedocument.presentationml.presentation");
    suppporteTypes.add("application/vnd.ms-powerpoint");

    boolean isAssetSubAsset = false;
    String subAssetParentURL = "";
    Asset parentAsset = null;
    if (asset.isSubAsset()) {
        isAssetSubAsset = true;
        Resource parentResource = currentResource.getParent().getParent(); //since immediate parent is subaasets
        parentAsset = parentResource.adaptTo(Asset.class);

        subAssetParentURL = request.getContextPath() + assetDetailsVanity + Text.escapePath(childNode.getParent().getParent().getPath());
    }
    com.adobe.granite.asset.api.Asset graniteAsset = currentResource.adaptTo(com.adobe.granite.asset.api.Asset.class);
    Iterator<?> pages = graniteAsset.listRelated("pages");
    if (suppporteTypes.contains(asset.getMimeType()) && pages.hasNext() ){
        Asset page1 = ((com.adobe.granite.asset.api.Asset)pages.next()).adaptTo(Asset.class);
        subAssetPath = page1.getPath();
    }
    String firstPageURL = !subAssetPath.equals("")? request.getContextPath() + assetDetailsVanity + Text.escapePath(subAssetPath)+"?pageViewer=true" : "";

    Config cfg = new Config(resource);
    AttrBuilder attrs = new AttrBuilder(request, xssAPI);
    attrs.addClass(cfg.get("class", String.class));

    String prop = "jcr:content/metadata/dc:format";
    String editorMimeTypes = null;
    String dataMimeType = asset.getMimeType();
    if(dataMimeType == null || dataMimeType.trim().equals("")) {
        if(childNode.hasProperty(prop)){
            dataMimeType = childNode.getProperty("jcr:content/metadata/dc:format").getString();
        }
    }
    if(dataMimeType == null) {
        dataMimeType = "";
    }
    boolean canAnnotate = false;
    String[] annotatableMimeTypes = { "image/jpeg", "image/jpg", "image/png", "image/gif" };
    if (UIHelper.hasPermission(acm, currentResource, Privilege.JCR_ADD_CHILD_NODES)) {
        if (contains(annotatableMimeTypes, dataMimeType) || UIHelper.getBestfitRendition(asset, 319) != null) {
            canAnnotate = true;
        }
    }

    boolean canEdit = false;

    boolean isLocked = isLocked(resourceResolver, childNode);
    if (!isLocked && UIHelper.hasPermission(acm, currentResource, Privilege.JCR_WRITE)) {
        Resource editors = resource.getResourceResolver().getResource("/libs/dam/gui/content/assets/editors");
        for (Iterator<Resource> it = editors.listChildren(); it.hasNext();) {
            Resource child = it.next();
            Config editorPropCfg = new Config(child);
            String[] mimeTypes = editorPropCfg.get("mimetypes", String[].class);
            Boolean isPrintAssetEditor = editorPropCfg.get("printasseteditor", Boolean.class);
            if (mimeTypes != null && contains(mimeTypes, dataMimeType)|| (isPrintAssetEditor != null && isPrintAssetEditor == true && isEditablePrintAsset(childNode))) {
                canEdit = true;
                break;
            }
        }
    }

    //Checkout Status
    Authorizable authUser = resourceResolver.adaptTo(Authorizable.class);
    boolean isCheckedOut = false;
    boolean canCheckIn = false;
    boolean canCheckOut = false;
    boolean parentIsCheckedOut = false;
    AssetCheckoutService assetCheckoutService = sling.getService(AssetCheckoutService.class);
    if (assetCheckoutService != null) {
        isCheckedOut = assetCheckoutService.isCheckedOut(asset);
        canCheckOut = assetCheckoutService.canCheckOut(asset) && !asset.isSubAsset();
        canCheckIn = assetCheckoutService.canCheckIn(asset) && !asset.isSubAsset();

        if (parentAsset != null) {
            parentIsCheckedOut = assetCheckoutService.isCheckedOut(parentAsset);
        }
    }
    String checkedOutBy = "";
    String checkedOutByFormatted = "";
    boolean checkedOutByCurrentUser = false;
    boolean isParentAssetCheckedOutByCurrentUser = false;
    if (isCheckedOut) {
        checkedOutBy = assetCheckoutService.getCheckedOutBy(asset);
        checkedOutByFormatted = AuthorizableUtil.getFormattedName(resource.getResourceResolver(), checkedOutBy);
        checkedOutByCurrentUser = authUser.getID().equals(checkedOutBy);
    }
    if (parentIsCheckedOut) {
        String parentAssetCheckedOutBy = assetCheckoutService.getCheckedOutBy(parentAsset);
        isParentAssetCheckedOutByCurrentUser = authUser.getID().equals(parentAssetCheckedOutBy);
    }

    boolean isAdmin = false;
    String tenantAssetsRoot = DamUtil.getTenantAssetsRoot(resourceResolver, currentResource.getPath());
    Resource tenantAssetsRootResource = resourceResolver.getResource(tenantAssetsRoot);
    if (tenantAssetsRootResource != null && acm.hasPrivileges(tenantAssetsRoot, new Privilege[]{acm.privilegeFromName(Privilege.JCR_ALL)})) {
        isAdmin = true;
    }

    boolean isAssetExpired = false;
	boolean isSubAssetExpired = false;
	Calendar now = Calendar.getInstance();
	Calendar assetExpiryTime = DamUtil.getExpiryTime(asset);
	if (null != assetExpiryTime) {
		isAssetExpired = DamUtil.isExpiredAsset(asset);
	}

	if (!isAssetExpired) {
		isSubAssetExpired = DamUtil.isExpiredSubAsset(resource);
	}

	canEdit = isAdmin || (canEdit && !isAssetExpired && !isSubAssetExpired);

	boolean canDwld = isAdmin || (!isAssetExpired && !isSubAssetExpired);

    boolean canUploadRendition = UIHelper.hasPermission(acm, currentResource, Privilege.JCR_ADD_CHILD_NODES);

    boolean is3D = S73DHelper.isS73D(currentResource);

    // If we have a 3D asset then allow editing provided the asset is not locked and the user has write access
    canEdit = canEdit || (is3D && UIHelper.hasPermission(acm, currentResource, Privilege.JCR_WRITE) && !isLocked);


    attrs.add("data-can-edit", canEdit);
    attrs.add("data-can-data-can-download", canDwld);
    attrs.add("data-can-data-can-annotate", canAnnotate);
    attrs.add("data-can-upload-rendition", canUploadRendition);
    attrs.add("data-asset-is-sub-asset", isAssetSubAsset);
    attrs.add("data-asset-can-check-in", canCheckIn);
    attrs.add("data-asset-can-check-out", canCheckOut);
    attrs.add("data-asset-is-checked-out", isCheckedOut);
    attrs.add("data-asset-checked-out-by", checkedOutBy);
    attrs.add("data-asset-checked-out-by-current-user", checkedOutByCurrentUser);
    attrs.add("data-parent-asset-is-checked-out", parentIsCheckedOut);
    attrs.add("data-parent-asset-checked-out-by-current-user", isParentAssetCheckedOutByCurrentUser);

    if("text/csv".equals(asset.getMimeType())) {
%>
<div <%= attrs.build() %> >
<sling:include path="<%= asset.getPath() %>" resourceType="dam/gui/components/admin/csv" />
</div>
<%
}
else {
// attempt to find a editor that can handle this resource if has the
// permission to edit
    if (isAdmin || UIHelper.hasPermission(acm, currentResource, Privilege.JCR_WRITE)) {
        Resource editors = resource.getResourceResolver().getResource("/libs/dam/gui/content/assets/editors");
        for (Iterator<Resource> it = editors.listChildren(); it.hasNext();) {
            Resource child = it.next();
            Config editorPropCfg = new Config(child);
            String[] mimeTypes = editorPropCfg.get("mimetypes", String[].class);
            Boolean isPrintAssetEditor = editorPropCfg.get("printasseteditor", Boolean.class);
            if (mimeTypes != null && contains(mimeTypes, dataMimeType)|| (isPrintAssetEditor != null && isPrintAssetEditor == true && isEditablePrintAsset(childNode))) {
                canEdit = true;
                break;
            }
        }
    }

    if (dataMimeType.contains("video") || "application/x-shockwave-flash".equals(dataMimeType)) {

        isVideo = isSupportedVideo(asset,sling);
    }

    boolean isAudio = false;
    if (dataMimeType.contains("audio")) {
        isAudio = isSupportedAudio(asset,sling);
    }

    boolean isSlingResourceType = false;
    String slingRenderPath = "";
    if (childNode.hasProperty("jcr:content/sling:resourceType")) {
        isSlingResourceType = true;
        slingRenderPath = asset.getPath() + "/jcr:content";
        Resource jcrContentRes = resourceResolver.getResource(slingRenderPath);
        if (jcrContentRes.isResourceType("fd/fm/af/render")) {
            slingRenderPath = slingRenderPath + "?wcmmode=disabled";
        }
    }

%>

<div id="videocommentinfo" class="alert right info" style="position: absolute; right: 3rem; top: 6rem; display: none;">
    <button class="close" data-dismiss="alert">x</button>
    <strong>INFO</strong><div></div>
</div>
<% if (isSlingResourceType) {
%>
<iframe src="<%= request.getContextPath() + slingRenderPath %>" height=100% width=100%> </iframe>
<% } %>

<div <%= attrs.build() %> data-first-page-url="<%= xssAPI.encodeForHTMLAttr(firstPageURL) %>" isSubAsset="<%= asset.isSubAsset() %>" subasset-parent-url="<%=xssAPI.encodeForHTMLAttr(subAssetParentURL) %>">
    <div id="comments" class="commentinfo"></div>

    <%
        boolean hadS7damType = childNode.hasProperty("jcr:content/dam:s7damType");
        boolean hadPTiff = (asset.getRendition("cqdam.pyramid.tiff") != null); //check for PTiff

    //FixMe: Move Assets specific viewer like video, page viewer etc. here
            Resource renditionPicker = resource.getChild("renditionpicker");
            Resource audioPreview = resource.getChild("audiopicker");
            Resource videoPreview = resource.getChild("videopreview");
            Resource v3DPicker = (is3D)? resource.getChild("webGLViewer") : null;

    if (is3D && v3DPicker != null) {
    %>
    <sling:include path="<%= v3DPicker.getPath() %>" resourceType="<%= v3DPicker.getResourceType()%>" />
    <ui:includeClientLib categories="apps.cq-scene7-v3D" />
    <% } else if (isAudio && audioPreview != null) {%>
    <sling:include path="<%= audioPreview.getPath() %>" resourceType="<%= audioPreview.getResourceType()%>" />
    <%} else if(isVideo && videoPreview != null) {%>
    <sling:include path="<%= videoPreview.getPath() %>" resourceType="<%= videoPreview.getResourceType()%>" />
    <% } else if (dataMimeType.contains("video")) {
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
    <iframe src="<%= iframeUrl %>" frameborder="0" allowfullscreen="" width="1670" height="843" style="max-width:100%; max-height:100%"></iframe>
    <% } else if (!isSlingResourceType) { %>
    <sling:include path="<%= renditionPicker.getPath() %>" resourceType="<%= renditionPicker.getResourceType()%>" />
    <sling:include path="<%= asset.getPath() %>" resourceType="dam/gui/components/admin/assetview/zoomviewer" />
    <% }
     %>
</div>
<%}%>

<%!

    boolean isEditablePrintAsset(Node assetNode){
        try {
            Node metadataNode = assetNode.getNode("jcr:content/metadata");
            return metadataNode.hasProperty(StringConstants.TEMPLATE_TYPE);
        } catch (Exception ex) {
            log("Exception occurred while getting indesign template asset type"
                    + ex.getMessage());
        }
        return false;
    }

    boolean contains(String[] mimeTypes, String mimeType) {
        for (String item:mimeTypes) {
            if (item.equalsIgnoreCase(mimeType)) {
                return true;
            }
        }
        return false;
    }
    boolean isLocked(ResourceResolver resourceResolver, Node node) {
        boolean isLocked = false;

        try {
            String lockedBy="";
            final LockManager lockManager = resourceResolver.adaptTo(
                    Session.class).getWorkspace().getLockManager();
            if(node != null) {
                if (node.isLocked()) {
                    isLocked=true;
                    lockedBy = lockManager.getLock(
                            node.getPath()).getLockOwner();

                }else if (node.hasNode(JcrConstants.JCR_CONTENT)){
                    Node contentNode = node.getNode(JcrConstants.JCR_CONTENT);
                    if(contentNode.isLocked()){
                        isLocked = true;
                        lockedBy = lockManager.getLock(contentNode.getPath()).getLockOwner();
                    }
                }
                if(isLocked){
                    User self = resourceResolver.adaptTo(User.class);
                    String selfId = self.getID();
                    isLocked = !selfId.equals(lockedBy);
                }
                return isLocked;
            }
        } catch (Exception ex) {
            log("Exception occurred while checking whether the node is locked: "
                    + ex.getMessage());
        }
        return true;
    }
%>