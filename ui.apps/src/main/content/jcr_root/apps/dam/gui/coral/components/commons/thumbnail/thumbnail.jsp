<%--
  ADOBE CONFIDENTIAL

  Copyright 2016 Adobe Systems Incorporated
  All Rights Reserved.

  NOTICE:  All information contained herein is, and remains
  the property of Adobe Systems Incorporated and its suppliers,
  if any.  The intellectual and technical concepts contained
  herein are proprietary to Adobe Systems Incorporated and its
  suppliers and may be covered by U.S. and Foreign Patents,
  patents in process, and are protected by trade secret or copyright law.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe Systems Incorporated.
--%><%
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page session="false"
          import="java.util.Calendar,
                  org.apache.jackrabbit.util.Text,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.sling.resource.collection.ResourceCollection,
                  org.apache.sling.api.resource.ResourceResolver,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ValueMap,
                  org.apache.commons.lang.StringUtils,
                  com.day.cq.commons.jcr.JcrConstants,
                  com.day.cq.dam.commons.util.UIHelper,
                  com.day.cq.dam.commons.util.DamConfigurationConstants,
                  com.day.cq.dam.commons.util.DamUtil,
                  com.day.cq.dam.api.Rendition,
                  com.day.cq.dam.commons.util.S7SetHelper,
                  com.day.cq.dam.api.Asset,
                  com.adobe.granite.security.user.UserManagementService,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.ComponentHelper.Options,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Tag,
				  com.adobe.granite.security.user.util.AuthorizableUtil,
				  com.adobe.cq.dam.processor.nui.util.DirectBinaryUtil,
                  javax.jcr.Node" %><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0" %><%
%><%@taglib prefix="ui" uri="http://www.adobe.com/taglibs/granite/ui/1.0"%><%
%><%

    Config cfg = cmp.getConfig();

    Asset targetAsset = null;
    Resource targetAssetResource = null;

    String strAssetInput = cmp.getExpressionHelper().getString(cfg.get("asset", String.class));
	String variant = cfg.get("variant", String.class);
	String context = "";
	String thumbnailUrl = cfg.get("thumbnailUrl", String.class);
	String title = "";

    Tag tag = cmp.consumeTag();
    AttrBuilder attrs = tag.getAttrs();
    attrs.addClass("cq-dam-assetthumbnail");
    attrs.addClass(cfg.get("class", String.class));

    AttrBuilder thumbnailAttr = new AttrBuilder(request, xssAPI);
    thumbnailAttr.addClass("foundation-layout-thumbnail");

    if (cfg.get("quiet", false)) {
         thumbnailAttr.addClass("foundation-layout-thumbnail-quiet");
    }

    if (strAssetInput != null) {

    boolean defaultPreview = cfg.get("defaultPreview", false); //applicable for Collections
    targetAssetResource = resourceResolver.getResource(strAssetInput);
    attrs.add("data-cq-dam-assetthumbnail-path", targetAssetResource.getPath());
    boolean isFolder = targetAssetResource != null && (targetAssetResource.isResourceType("sling:Folder") ||
            targetAssetResource.isResourceType("sling:OrderedFolder") ||
            targetAssetResource.isResourceType("nt:folder"));

	boolean isAsset = false;
    if (targetAssetResource != null && !isFolder) {
        targetAsset = targetAssetResource.adaptTo(Asset.class);
        if (targetAsset != null) {
        	isAsset = true;
    	}
    }

	boolean isCollection = false;
    if (!isAsset && !isFolder) {
       ResourceCollection collection = targetAssetResource.adaptTo(ResourceCollection.class);
    	if (collection != null) {
        	isCollection = true;
        } else if (defaultPreview) {
            isCollection = true;
        }
    }

	if (!isAsset && !isFolder && !isCollection) {
        return;
	}

	Node contentNode = targetAssetResource.adaptTo(Node.class);

    title = UIHelper.getTitle(targetAssetResource);
	boolean isFragment = isContentFragment(targetAssetResource);
	boolean isS7Set = S7SetHelper.isS7Set(targetAssetResource);
    if (isFragment) {
        context = i18n.get("ASSET");
        variant = "asset";
        thumbnailUrl = getFragmentThumbnailUrl(targetAssetResource);
    } else  if (contentNode.isNodeType("dam:Asset") && !isS7Set) {
        Rendition rendition = UIHelper.getBestfitRendition(targetAssetResource.adaptTo(Asset.class), 319);
        if (rendition != null) {
            thumbnailUrl = DirectBinaryUtil.getRenditionCloudURI(rendition) == null ? null : DirectBinaryUtil.getRenditionCloudURI(rendition).toString();
            if(thumbnailUrl == null || thumbnailUrl.isEmpty()){
                thumbnailUrl = request.getContextPath() + xssAPI.getValidHref(Text.escapePath(rendition.getPath()));
            }
        }
        context = i18n.get("ASSET");
        variant = "asset";
    } else if (isFolder)  {
        context = i18n.get("FOLDER");
        variant = "inverted";
        boolean manualThumnailExists = false;
        String manualThumbnailPath = targetAssetResource.getPath() + "/jcr:content/manualThumbnail.jpg";
        Resource manualThumbnail = resourceResolver.getResource(manualThumbnailPath);
        if (manualThumbnail != null) {
            manualThumnailExists = true;
        }

        if (manualThumnailExists) {
            thumbnailUrl = request.getContextPath() + xssAPI.getValidHref(Text.escapePath(manualThumbnail.getPath()));
        } else {
            thumbnailUrl = request.getContextPath() + xssAPI.getValidHref(Text.escapePath(targetAssetResource.getPath())) + ".folderthumbnail.jpg?width=280&height=240";
        }
        attrs.add("data-manual-thumbnail", manualThumnailExists);
		attrs.add("data-thumbnail-url", thumbnailUrl);
		
		String metadataPath = targetAssetResource.getPath() + "/jcr:content/metadata";
		Resource metadata = resourceResolver.getResource(metadataPath);
		if (metadata != null && StringUtils.equals(metadata.getValueMap().get("bnpp-media", ""), "true")) {
            context = i18n.get("MEDIA");
        }
		
    } else if (isS7Set) {
        thumbnailUrl = request.getContextPath() + xssAPI.getValidHref(targetAssetResource.getPath()) + ".folderthumbnail.jpg?width=280&height=240";
        context = i18n.get("COLLECTION");
        variant = "inverted";
    } else if (isCollection) {
        if (defaultPreview) {
			thumbnailUrl = request.getContextPath() + xssAPI.getValidHref("/content/dam/collections") + ".folderthumbnail.jpg?width=280&height=240";
        } else {
        	thumbnailUrl = request.getContextPath() + xssAPI.getValidHref(targetAssetResource.getPath()) + ".folderthumbnail.jpg?width=280&height=240";
        }
        // Path at which manual thumbnail(if present) exists
        String manualThumbnailPath = targetAssetResource.getPath() + "/manualThumbnail.jpg";
        Resource manualThumbnail = resourceResolver.getResource(manualThumbnailPath);
        if (manualThumbnail != null) {
            thumbnailUrl = manualThumbnailPath;
            attrs.add("data-manual-thumbnail", true);
			attrs.add("data-thumbnail-url", thumbnailUrl);
        }

        context = i18n.get("COLLECTION");
        variant = "inverted";

        // to determine collection home
        String userCollsHome = DamUtil.getUserCollectionsPath(resourceResolver);
        attrs.addClass("cq-dam-collection-metadata");
        attrs.add("data-coll-home", userCollsHome);
    }

	String pathEncoded = xssAPI.encodeForHTMLAttr(targetAssetResource.getPath());
    }

%><div <%= attrs.build() %>>
    <div <%= thumbnailAttr.build() %>>
        <div class="foundation-layout-thumbnail-image">
            <coral-card variant="<%= xssAPI.encodeForHTMLAttr(variant) %>">
                <coral-card-asset>
                    <img src="<%= xssAPI.getValidHref(thumbnailUrl) %>" alt="<%=xssAPI.encodeForHTMLAttr(UIHelper.getAltText(targetAssetResource))%>"/>
                </coral-card-asset>
                <coral-card-content>
                    <coral-card-context><%= context %></coral-card-context>
                    <coral-card-title><%= xssAPI.encodeForHTML(title) %></coral-card-title>
                </coral-card-content>
            </coral-card>
        </div>
    </div>
    <div class="foundation-field-editable foundation-layout-util-vmargin">
        <div class="foundation-field-edit foundation-layout-inline"><%
            Resource actions = resource.getChild("actions");
            if (actions != null) {
                Resource upload = actions.getChild("upload");
                if (upload != null) {
                    cmp.include(upload, new Options().rootField(false));
                    %><ui:includeClientLib categories="dam.gui.admin.thumbnailupload" /><%
                }
             }%>
        </div>
    </div>
</div><%!

   private boolean isContentFragment(Resource resource) {
        Resource contentResource = resource.getChild(JcrConstants.JCR_CONTENT);
        boolean isFragment = false;
        if (contentResource != null) {
            ValueMap contentProps = contentResource.adaptTo(ValueMap.class);
            isFragment = contentProps.get("contentFragment", false);
        }
        return isFragment;
    }

   private String getFragmentThumbnailUrl(Resource resource) {
        String thumbnailUrl;
        Resource thumbnailRsc = resource.getChild("jcr:content/thumbnail.png");
        if (thumbnailRsc != null) {
            // use the existing thumbnail
            Calendar createdCal = thumbnailRsc.getValueMap()
                    .get(JcrConstants.JCR_CREATED, Calendar.class);
            Resource contentResource = thumbnailRsc.getChild(JcrConstants.JCR_CONTENT);
            Calendar lastModifiedCal = createdCal;
            if (contentResource != null) {
                lastModifiedCal = contentResource.getValueMap()
                        .get(JcrConstants.JCR_LASTMODIFIED, createdCal);
            }
            long lastModified =
                    (lastModifiedCal != null ? lastModifiedCal.getTimeInMillis() : -1);
            thumbnailUrl = Text.escapePath(thumbnailRsc.getPath()) + "?_ck=" + lastModified;
        } else {
            // default thumbnail
            thumbnailUrl = Text.escapePath("/libs/dam/cfm/admin/content/static/thumbnail_fragment.png");
        }
        return thumbnailUrl;
    }

%>
