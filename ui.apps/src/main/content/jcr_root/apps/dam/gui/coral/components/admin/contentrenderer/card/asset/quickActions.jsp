<%--
  ADOBE CONFIDENTIAL

  Copyright 2015 Adobe Systems Incorporated
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
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@page import="org.apache.sling.api.resource.Resource,
                  org.apache.jackrabbit.util.Text,
					org.apache.sling.jcr.base.util.AccessControlUtil,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
				  javax.jcr.Node,
				  com.day.cq.dam.api.Asset"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/card/common/processAttributes.jsp"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/assetBase.jsp"%><%--###
ASSET Quick Action
=========

###--%><%

    boolean isEditablePrintAsset =  request.getAttribute(IS_EDITABLE_PRINT_ASSET) != null ? (boolean) request.getAttribute(IS_EDITABLE_PRINT_ASSET) : false;
    boolean isAdmin = request.getAttribute(IS_DOWNLOAD_ALLOWED_FOR_ADMINS) != null ? (boolean) request.getAttribute(IS_DOWNLOAD_ALLOWED_FOR_ADMINS) : false;
    boolean isAssetExpired = request.getAttribute(IS_ASSETEXPIRED) != null ? (boolean) request.getAttribute(IS_ASSETEXPIRED) : false;
    boolean isSubAssetExpired = request.getAttribute(IS_SUBASSET_EXPIRED) != null ? (boolean) request.getAttribute(IS_SUBASSET_EXPIRED) : false;
    boolean showDesktopLinks = request.getAttribute(SHOW_DESKTOP_LINKS) != null ? (boolean) request.getAttribute(SHOW_DESKTOP_LINKS) : false;
    boolean canAnnotate = request.getAttribute(CAN_ANNOTATE) != null ? (boolean) request.getAttribute(CAN_ANNOTATE) : false;
    boolean canEdit = request.getAttribute(CAN_EDIT) != null ? (boolean) request.getAttribute(CAN_EDIT) : false;
    boolean isDMSet = request.getAttribute(IS_DM_SET) != null ? (boolean) request.getAttribute(IS_DM_SET) : false;
    String dmAssetType = request.getAttribute(DM_ASSET_TYPE) != null ? (String) request.getAttribute(DM_ASSET_TYPE) : "";
    String type = request.getAttribute(TYPE) != null ? (String) request.getAttribute(TYPE) : "";
    boolean isSubAsset = type.equals("subasset");
    String resourcePath = request.getAttribute(RESOURCE_PATH) != null ? (String) request.getAttribute(RESOURCE_PATH) : "";
    long bytes = request.getAttribute(BYTES) != null ? (long) request.getAttribute(BYTES) : 0;
    String dmDisplayMimeType = request.getAttribute(S7_DAM_TYPE) != null ? (String) request.getAttribute(S7_DAM_TYPE) : "";
    boolean isCheckedOut = request.getAttribute(IS_CHECKED_OUT) != null ? (boolean) request.getAttribute(IS_CHECKED_OUT) : false;
    boolean isCheckedOutByCurrentUser = request.getAttribute(CHECKED_OUT_BY_CURRENT_USER) != null ? (boolean) request.getAttribute(CHECKED_OUT_BY_CURRENT_USER) : false;
    boolean isParentAssetCheckedOut = request.getAttribute(IS_PARENT_ASSET_CHECKED_OUT) != null ? (boolean) request.getAttribute(IS_PARENT_ASSET_CHECKED_OUT) : false;
    boolean isParentAssetCheckedOutByCurrentUser = request.getAttribute(IS_PARENT_ASSET_CHECKED_OUT_BY_CURRENT_USER) != null ? (boolean) request.getAttribute(IS_PARENT_ASSET_CHECKED_OUT_BY_CURRENT_USER) : false;
    boolean canUseRestricedActions = isSubAsset ? (!isParentAssetCheckedOut || isParentAssetCheckedOutByCurrentUser) : (!isCheckedOut || isCheckedOutByCurrentUser);
    String escapedResourcePath = (Text.escape(resourcePath)).replaceAll("%2f","/");
    boolean isDownloadable = request.getAttribute(IS_DOWNLOADABLE) != null ? (Boolean.parseBoolean(request.getAttribute(IS_DOWNLOADABLE).toString())) : true;
    boolean isLiveCopy = request.getAttribute(IS_LIVE_COPY) != null ? (boolean) request.getAttribute(IS_LIVE_COPY) : false;
    boolean hasSimSearchFeatures = request.getAttribute(HAS_FEATURES) != null ? (Boolean.parseBoolean(request.getAttribute(HAS_FEATURES).toString())) : true;
    boolean canFindSimilar = request.getAttribute(CAN_FINDSIMILAR) != null ? (Boolean.parseBoolean(request.getAttribute(CAN_FINDSIMILAR).toString())) : false;


    UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
    boolean isAdminUser = false;

    if(userManager != null){
        User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
        Group group = (Group)userManager.getAuthorizable("administrators");
        if(currentUser != null){
            if(group != null){
                isAdminUser = group.isMember(currentUser) || "admin".equals(resourceResolver.getUserID());
            }

            if( (!isAdminUser) && (userManager.getAuthorizable("mediahub-administrators") != null) ){
                isAdminUser = ((Group)userManager.getAuthorizable("mediahub-administrators")).isMember(currentUser);
            }

            if( (!isAdminUser) && (userManager.getAuthorizable("mediahub-basic-entity-manager") != null) ){
                isAdminUser = ((Group)userManager.getAuthorizable("mediahub-basic-entity-manager")).isMember(currentUser);
            }
        }
    }

    Resource cardResource = resourceResolver.getResource(resourcePath);

    String bnpDownloadAuth = "no";
    if(cardResource != null && cardResource.getChild("jcr:content") != null && cardResource.getChild("jcr:content").getChild("metadata") != null){
        bnpDownloadAuth = cardResource.getChild("jcr:content").getChild("metadata").getValueMap().get("bnpp-download-auth","no");
    }

%>
<coral-quickactions scrollonfocus="off" target="_prev" alignmy="left top" alignat="left top">
    <coral-quickactions-item icon="check" class="foundation-collection-item-activator"><%= xssAPI.encodeForHTML(i18n.get("Select")) %></coral-quickactions-item>
    <% if (showQuickActions) { %>
    <% if ((isAdminUser || (!isAssetExpired && !isSubAssetExpired )) && isDownloadable) {
        if( bytes != 0 ) { %>
    <% if(isAdminUser || StringUtils.contains(resourcePath, "/content/dam/projects")){%>
    <coral-quickactions-item icon="download" class="cq-damadmin-admin-actions-download-activator foundation-collection-action" link="<%=resourcePath%>" data-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/downloadasset.html") %>" data-itempath="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>" data-haslicense-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/haslicense.html") %>" data-license-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/licensecheck.external.html") %>"><%= xssAPI.encodeForHTML(i18n.get("Download")) %></coral-quickactions-item>
    <%  %>
    <% } else {
        if(StringUtils.contains(resourcePath, "/content/dam/medialibrary") && StringUtils.equals(bnpDownloadAuth, "yes")){%>
    <coral-quickactions-item icon="download" class="cq-damadmin-admin-actions-download-activator foundation-collection-action" link="<%=resourcePath%>" data-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/bnpdownloadasset.html") %>" data-itempath="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>" data-haslicense-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/haslicense.html") %>" data-license-href="<%= xssAPI.getValidHref("/mnt/overlay/dam/gui/content/assets/licensecheck.external.html") %>"><%= xssAPI.encodeForHTML(i18n.get("Download")) %></coral-quickactions-item>

    <% } } %>
    <% }
    }%>
    <% if (showDesktopLinks && isAdminUser) { %>
    <coral-quickactions-item icon="printPreview" class="dam-asset-desktop-action" data-path="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>" data-href-query="?action=open"><%= xssAPI.encodeForHTML(i18n.get("Open on desktop")) %></coral-quickactions-item><%
    }
%>

    <coral-quickactions-item icon="infoCircle" class="foundation-anchor" data-foundation-anchor-href="<%= xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/dam/gui/content/assets/metadataeditor.external.html" + "?item=" + escapedResourcePath) + "&_charset_=utf8" %>" data-pageheading = "AEM Assets | Asset Metadata Editor" data-contextpath = "/assetdetails.html"><%= xssAPI.encodeForHTML(i18n.get("Properties")) %></coral-quickactions-item>
    <%   if (canEdit && (isAdmin || (!isAssetExpired && !isSubAssetExpired && canUseRestricedActions))) {
        String assetEditorPath = "/mnt/overlay/dam/gui/content/assets/assetedit.html";
        if (isDMSet) {
            if (dmDisplayMimeType.equalsIgnoreCase("carouselset")) { //special editor for carousel set
                assetEditorPath = "/mnt/overlay/dam/gui/content/s7dam/sets/carousel/editor.html" ;
            }
            else {
                assetEditorPath = "/mnt/overlay/dam/gui/content/s7dam/sets/editor.html";
            }
        }
        else if (dmAssetType.equals("video")) {
            assetEditorPath = "/mnt/overlay/dam/gui/content/s7dam/shoppablevideo/editor.html";
        } else if(isEditablePrintAsset) {
            assetEditorPath = "/mnt/overlay/dam/gui/content/idsprint/templates/edittemplate.html";
        }

        if (isAdminUser) {
    %>
    <coral-quickactions-item icon="edit" class="foundation-anchor" data-foundation-anchor-href="<%= xssAPI.getValidHref(request.getContextPath() + assetEditorPath + escapedResourcePath)%>" data-pageheading="<%= i18n.get("AEM Assets | Asset Editor")%>"><%= xssAPI.encodeForHTML(i18n.get("Edit")) %></coral-quickactions-item>
    <% } }
    %>
    <% if (canFindSimilar && isAdminUser) { %>
    <coral-quickactions-item icon="imageSearch" class="foundation-button image-search-quickaction-button" data-vis-search-features="<%=hasSimSearchFeatures%>"
                             data-pageheading="<%= i18n.get("AEM Assets | Asset Editor")%>"><%= xssAPI.encodeForHTML(i18n.get("Find similar")) %></coral-quickactions-item>
    <% } %>

    <% if (canAnnotate && canUseRestricedActions && !isLiveCopy && isAdminUser) { %>
    <coral-quickactions-item icon="note" class="foundation-anchor" data-foundation-anchor-href="<%= xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/dam/gui/content/assets/annotate.html" + escapedResourcePath)%>"><%= xssAPI.encodeForHTML(i18n.get("Annotate")) %></coral-quickactions-item>
    <% } %>
    <% if ((isAdmin || (!isSubAsset && !isAssetExpired)) && !isLiveCopy && isAdminUser) { %>
    <coral-quickactions-item icon="copy" class="foundation-collection-action" data-foundation-collection-action='{"action": "cq.wcm.copy"}'><%= xssAPI.encodeForHTML(i18n.get("Copy")) %></coral-quickactions-item>
    <% } %>
    <% } %>
    <% if (showDesktopLinks && isAdminUser) { %>
    <%    if (!isCheckedOut && canEdit) { %>
    <coral-quickactions-item icon="scribble" class="dam-asset-desktop-action cq-damadmin-admin-actions-checkout-activator" data-path="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>" data-href-query="?action=open"><%= xssAPI.encodeForHTML(i18n.get("Edit desktop")) %></coral-quickactions-item>
    <% } %>
    <coral-quickactions-item icon="open" class="dam-asset-desktop-action cq-damadmin-admin-actions-reveal-activator" data-path="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>"><%= xssAPI.encodeForHTML(i18n.get("Reveal")) %></coral-quickactions-item>
    <% } %>
</coral-quickactions>
