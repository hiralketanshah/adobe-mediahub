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
--%>
<%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%><%
%><%@ page import="org.apache.jackrabbit.util.Text" %>
<%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/init/assetBase.jsp"%>
<cq:include script="init.jsp"/><%

final String CONST_S7DM_REMOTE_SET_ASSET_TYPE = "remote";

boolean isOmniSearchRequest = request.getAttribute(IS_OMNISEARCH_REQUEST) != null ? (boolean) request.getAttribute(IS_OMNISEARCH_REQUEST) : false;

// s7damType is defined in /libs/dam/gui/coral/components/admin/contentrenderer/base/init/assetBase.jsp
boolean isRemoteDMSetAsset = s7damType.equals(CONST_S7DM_REMOTE_SET_ASSET_TYPE);

String assetActionRels = StringUtils.join(
  UIHelper.getAssetActionRels(
    UIHelper.ActionRelsResourceProperties.create(isAssetExpired, isSubAssetExpired, isContentFragment, 
      isArchive, isSnippetTemplate, isDownloadable, isStockAsset, isStockAssetLicensed, isStockAccessible, canFindSimilar),
    UIHelper.ActionRelsUserProperties.create(hasJcrRead, hasJcrWrite, hasAddChild, canEdit, canAnnotate, isDAMAdmin),
    UIHelper.ActionRelsRequestProperties.create(isOmniSearchRequest,isLiveCopy)), 
  " ");
request.setAttribute("actionRels", actionRels.concat(" " + assetActionRels));
JSONObject viewSettings = (JSONObject) request.getAttribute(VIEW_SETTINGS);

if (allowNavigation) {
attrs.addClass("foundation-collection-navigator");
}
%>
<cq:include script="link.jsp"/>
<%
    if (request.getAttribute("com.adobe.assets.card.nav")!=null){
        navigationHref =  (String) request.getAttribute("com.adobe.assets.card.nav");
        navigationHref = Text.escapePath(navigationHref);
    }
attrs.add("data-foundation-collection-navigator-href", xssAPI.getValidHref(navigationHref));
attrs.add("data-item-type", type);

request.setAttribute("com.adobe.assets.meta.attributes", metaAttrs);
%>
<cq:include script = "meta.jsp"/>
<coral-card <%= attrs.build() %>>
    <coral-card-asset>
        <cq:include script = "assetViewer.jsp"/>
    </coral-card-asset><%
    if(viewSettings.getBoolean(VIEW_SETTINGS_PN_INSIGHT)) {
       %><cq:include script = "insight.jsp"/>
    <% } %>
	<coral-card-content>
        <coral-card-context>
          <%= xssAPI.encodeForHTML(displayMimeType) %>
          <% if (isLiveCopy) { %><%= xssAPI.encodeForHTML(i18n.get("Live Copy")) %><% } %>
        </coral-card-context>
        <% String parent = "";
        if(isOmniSearchRequest && resource.getPath().startsWith("/content/dam/medialibrary")){
        parent = "From Medialibrary";
        
        }
        else if(isOmniSearchRequest && resource.getPath().startsWith("/content/dam/projects")){
        parent = "From Project";
        
        }
        
        %>
        <coral-card-subtitle><%= i18n.get(parent)%></coral-card-subtitle>
        <coral-card-title class="foundation-collection-item-title"><%= xssAPI.encodeForHTML(resourceTitle) %></coral-card-title>
        <% if (!resource.getName().equalsIgnoreCase(resourceTitle)) { %>
        <coral-card-subtitle class="foundation-collection-item-subtitle"><%= xssAPI.encodeForHTML(resource.getName()) %></coral-card-subtitle>
        <% } %>
        <cq:include script = "propertyList.jsp"/>
        <link rel="properties" href="<%=xssAPI.getValidHref(navigationHref)%>">
    </coral-card-content>
    <cq:include script = "../common/card-banner.jsp"/>
	<cq:include script = "applicableRelationships.jsp"/>
</coral-card>

<% /* s7 remote sets are cached in tmp, and none of the quick actions or selection states are applicable to them */ %>
<% if (!isRemoteDMSetAsset) { %>
    <cq:include script = "quickActions.jsp"/>
<% } %>
