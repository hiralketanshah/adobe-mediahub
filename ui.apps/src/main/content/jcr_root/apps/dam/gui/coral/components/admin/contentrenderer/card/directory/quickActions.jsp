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
--%><%@page import="javax.jcr.RepositoryException"%>
<%
%><%@include file="/libs/granite/ui/global.jsp"%><%         
%><%@page import="org.apache.sling.api.resource.Resource,
				  javax.jcr.Node,
				  javax.jcr.RepositoryException,
				  org.apache.jackrabbit.util.Text,
					com.adobe.granite.ui.components.rendercondition.RenderCondition,
					com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition,
					org.apache.sling.jcr.base.util.AccessControlUtil,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
				  com.day.cq.dam.api.Asset"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/card/common/processAttributes.jsp"%><%                       
%><sling:include resourceType= "dam/gui/components/s7dam/smartcroprenditions/rendercondition"/><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/directoryBase.jsp"%><%--###

Directory Quick Action
=========
###--%><%
boolean showDesktopLinks = request.getAttribute(SHOW_DESKTOP_LINKS) != null ? (boolean) request.getAttribute(SHOW_DESKTOP_LINKS) : false;
String resourcePath = request.getAttribute(RESOURCE_PATH) != null ? (String) request.getAttribute(RESOURCE_PATH) : "";
String escapedResourcePath = (Text.escape(resourcePath)).replaceAll("%2f","/");
boolean hasReplicate = (boolean) request.getAttribute(HAS_REPLICATE_ACCESS);

boolean showSmartCropEditLink = true;
final String CROP_EDITOR_PATH = "/mnt/overlay/dam/gui/content/s7dam/smartcrop/smartcropedit.html";

Resource currentFolderResource = slingRequest.getResourceResolver().getResource(resourcePath);
SimpleRenderCondition s7smarCropRenderCondition = (SimpleRenderCondition) request.getAttribute(RenderCondition.class.getName());

UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
  boolean isAdmin = false;

  if(userManager != null){
      User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
      Group group = (Group)userManager.getAuthorizable("administrators");
      if(currentUser != null){
          if(group != null){
            isAdmin = group.isMember(currentUser) || "admin".equals(resourceResolver.getUserID());
          } else if( (!isAdmin) && (userManager.getAuthorizable("mediahub-administrators") != null) ){
            isAdmin = ((Group)userManager.getAuthorizable("mediahub-administrators")).isMember(currentUser);
          } else if( (!isAdmin) && (userManager.getAuthorizable("mediahub-super-administrators") != null) ){
            isAdmin = ((Group)userManager.getAuthorizable("mediahub-super-administrators")).isMember(currentUser);
          }
      }
  }

%>

<% if (showQuickActions) { %>
<coral-quickactions scrollonfocus="off" target="_prev" alignmy="left top" alignat="left top">
    <coral-quickactions-item icon="check" class="foundation-collection-item-activator"><%= xssAPI.encodeForHTML(i18n.get("Select")) %></coral-quickactions-item>
    <% if (showDesktopLinks) { %>
    <coral-quickactions-item icon="open" class="dam-asset-desktop-action" data-path="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>"><%= xssAPI.encodeForHTML(i18n.get("Reveal on desktop")) %></coral-quickactions-item>
    <%
       }
    %>
    <coral-quickactions-item icon="download" class="cq-damadmin-admin-actions-download-activator foundation-collection-action" data-href="<%= xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/dam/gui/content/assets/downloadasset.html") %>" data-itempath="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>" data-haslicense-href="/mnt/overlay/dam/gui/content/assets/haslicense.html" data-license-href="<%= xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/dam/gui/content/assets/licensecheck.external.html") %>"><%= xssAPI.encodeForHTML(i18n.get("Download")) %></coral-quickactions-item>
    <coral-quickactions-item icon="collectionAdd" class="foundation-anchor" data-foundation-anchor-href="<%= xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/dam/gui/content/collections/addtocollectionwizard.html/content/dam/collections?item=" + escapedResourcePath) %>"><%= xssAPI.encodeForHTML(i18n.get("To Collection")) %></coral-quickactions-item> 
<%
    	if(isAdmin){
    %>
    <coral-quickactions-item icon="copy" class="foundation-collection-action" data-foundation-collection-action='{"action": "cq.wcm.copy"}'><%= xssAPI.encodeForHTML(i18n.get("Copy")) %></coral-quickactions-item>
<%
    }
    %>
    <% if(hasReplicate && isAdmin) { %>
        <coral-quickactions-item icon="globe" class="cq-damadmin-admin-actions-publish-activator" data-src = "/mnt/overlay/dam/gui/content/commons/publish.html" data-foundation-mode-value="default" data-foundation-collection-item="<%= xssAPI.encodeForHTMLAttr(resourcePath) %>"><%= xssAPI.encodeForHTML(i18n.get("Publish")) %></coral-quickactions-item>
    <% } %>
    <!-- edit smart crops quick action -->
    <% if(s7smarCropRenderCondition.check() && isAdmin) { %>
    <coral-quickactions-item 
		icon="cropLightning"
		class="foundation-anchor"
		data-foundation-anchor-href="<%= xssAPI.getValidHref(request.getContextPath() + CROP_EDITOR_PATH + currentFolderResource.getPath())%>"
		data-pageheading="<%= i18n.get("AEM Assets | Smart Crop Editor")%>"><%= xssAPI.encodeForHTML(i18n.get("Smart Crop")) %>
	</coral-quickactions-item>
    <% } %>
</coral-quickactions>
<% } %>
