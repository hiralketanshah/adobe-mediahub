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
<%
%><%@page import="java.util.Collections,
                  java.util.Arrays,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ResourceResolver,
                  org.apache.sling.jcr.base.util.AccessControlUtil,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
                  javax.jcr.Session"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/init/directoryBase.jsp"%>
<%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/row/common/common.jsp"%><%

String directoryActionRels = StringUtils.join(UIHelper.getDirectoryActionRels(hasJcrRead, hasModifyAccessControl, hasJcrWrite, hasReplicate, isMACShared, isCCShared, isRootMACShared, isMPShared, isRootMPShared, isLiveCopy, hasAddChild, hasRemoveNode, hasModifyProperties), " ");

request.setAttribute("actionRels", actionRels.concat(" " + directoryActionRels));

attrs.addClass("foundation-collection-navigator");
if (request.getAttribute("com.adobe.directory.card.nav") != null){
    navigationHref =  (String) request.getAttribute("com.adobe.directory.card.nav");
    attrs.add("data-foundation-collection-navigator-href", xssAPI.getValidHref(navigationHref));
}

attrs.add("is", "coral-table-row");
attrs.add("data-item-title", resourceTitle);
attrs.add("data-item-type", type);

String path = resource.getPath();

request.setAttribute("com.adobe.assets.meta.attributes", metaAttrs);
PublicationStatus publicationStatus = getPublicationStatus(request, i18n);

Boolean internalUser = false;
if(StringUtils.contains(path, "/content/dam/projects") ){
  if( StringUtils.isNotBlank(resourceResolver.getUserID()) && null != resourceResolver.getResource( resourceResolver.adaptTo(UserManager.class).getAuthorizable(resourceResolver.getUserID()).getPath())){
    Resource user = resourceResolver.getResource(resourceResolver.adaptTo(UserManager.class).getAuthorizable(resourceResolver.getUserID()).getPath());
    if(null != user.getChild("profile")){
      Resource profile = user.getChild("profile");
      internalUser = StringUtils.equals("internal", profile.getValueMap().get("type","external"));
    }
  }
}

if(StringUtils.contains(path, "/content/dam/projects") && resource.getChild("jcr:content") != null && resource.getChild("jcr:content").getChild("metadata") != null) {
  ValueMap metadata = resource.getChild("jcr:content").getChild("metadata").getValueMap();
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
  if( metadata.containsKey("internalfolder") && !(isAdmin || internalUser) ){
    if(metadata.containsKey("internaluserpicker")){
      if( (metadata.get("internaluserpicker") instanceof String) && !StringUtils.equals( metadata.get("internaluserpicker", ""), resourceResolver.getUserID() ) ){
        attrs.add("hidden", "true");
      } else if( !Arrays.asList(metadata.get("internaluserpicker", String[].class)).contains(resourceResolver.getUserID())){
        attrs.add("hidden", "true");
      }
    } else {
      attrs.add("hidden", "true");
    }
  }
}

%><tr <%= attrs %>>
    <td is="coral-table-cell" coral-table-rowselect>
        <coral-icon class="foundation-collection-item-thumbnail" icon="folder"></coral-icon>
    </td>
    <td is="coral-table-cell" role="rowheader" value="<%= xssAPI.encodeForHTMLAttr(resource.getName()) %>">
        <%= xssAPI.encodeForHTML(resource.getName()) %>
    </td>
    <td class="foundation-collection-item-title" is="coral-table-cell" value="<%= xssAPI.encodeForHTMLAttr(resourceAbsTitle) %>"><%= xssAPI.encodeForHTML(resourceAbsTitle) %></td>
    <td is="coral-table-cell" value="<%= displayLanguage %>"><%= displayLanguage %></td>
    <td is="coral-table-cell" value="0"></td> <!--Adding a placeholder column for expiryStatus -->
    <td is="coral-table-cell" value="0"></td> <!--Adding a placeholder column for encodingStatus -->
    <td is="coral-table-cell" value="type">
          <%
              if (resource != null && resource.getChild("jcr:content") != null && resource.getChild("jcr:content").getChild("metadata") != null && resource.getChild("jcr:content").getChild("metadata").getValueMap().get("bnpp-media") != null && "true".equalsIgnoreCase(resource.getChild("jcr:content").getChild("metadata").getValueMap().get("bnpp-media", String.class))) {
          %>
           		<%= i18n.get("MEDIA") %>
          <% }  else { %>
         		<%= i18n.get("FOLDER") %>
          <% } %>

        <% if (isLiveCopy) { %><div class="foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(i18n.get("Live Copy")) %></div><% } %>
    </td>
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for dimensions -->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for size -->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for rating -->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for usagescore-->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for impression score-->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for click score-->
    <td is="coral-table-cell" value="<%= xssAPI.encodeForHTMLAttr(Long.toString(createdLong)) %>"><%
        if (createdStr != null) {
            %><foundation-time type="datetime" value="<%= xssAPI.encodeForHTMLAttr(createdStr) %>"></foundation-time><%
        }
    %>
    </td>
    <td is="coral-table-cell" value="<%= xssAPI.encodeForHTMLAttr(Long.toString(directoryLastModification)) %>"><%
        if (lastModified != null) {
            %><foundation-time type="datetime" value="<%= xssAPI.encodeForHTMLAttr(lastModified) %>"></foundation-time><%

            // Modified-after-publish indicator
                if (publishDateInMillis > 0 && publishDateInMillis < directoryLastModification) {
                String modifiedAfterPublishStatus = i18n.get("Modified since last publication");
                %><coral-icon icon="alert" style = "margin-left: 5px;" size="XS" title="<%= xssAPI.encodeForHTMLAttr(modifiedAfterPublishStatus) %>"></coral-icon><%
            }

            %><div class="foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(lastModifiedBy) %></div><%
        }
    %>
    </td>
    <td is="coral-table-cell" value="<%= (!isDeactivated && publishedDate != null) ? xssAPI.encodeForHTMLAttr(Long.toString(publishDateInMillis)) : "0" %>"><%
        // Published date and status
        String icon = null;
        String briefStatus = "";
        String title = "";
        if (publicationStatus.getAction() != null) {
            icon = publicationStatus.getIcon();
            briefStatus = publicationStatus.getBriefStatus();
            title = publicationStatus.getDetailedStatus();
        } else {
            if (publishedDate != null) {
                icon = isDeactivated ? "globeStrike" : "globe";
            }
        }
        if (icon != null) {
    %><coral-icon icon="<%= icon %>" style = "margin-left: 5px;" title = "<%= title %>" size="XS"%></coral-icon><%
        if (briefStatus != null && briefStatus.length() > 0) {%>
        <label> <%= i18n.getVar(briefStatus) %> </label>
        <%} else {%>
                <foundation-time type="datetime" value="<%= xssAPI.encodeForHTMLAttr(publishedDate) %>"></foundation-time><%
            }
        } else {
        %><span><%= xssAPI.encodeForHTML(i18n.get("Not published")) %></span><%
            }

            // Published by
            if (publishedBy != null) {
        %><div class="foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(publishedBy) %></div><%
            }

        %><cq:include script = "applicableRelationships.jsp"/>
    </td>
    <%
    
    if (showDMPublishCol) { %>
        <td is="coral-table-cell"></td>  <!--Adding a placeholder column for dm publish status-->
    <% } %>
    <td is="coral-table-cell"></td>  <!--Adding a placeholder column for workflow status-->
    <td is="coral-table-cell"></td> <!--Adding a placeholder column for checkout status-->
    <td is="coral-table-cell" value="0"></td>   <!--Adding a placeholder column for comments-->
    
    <% if (isProcessingProfileEntitled && !profileTitleList[0].trim().isEmpty()) { %>
        <td is="coral-table-cell" value="0"><%= xssAPI.encodeForHTML(profileTitleList[0].trim()) %></td>
    <% } else{ %>
        <td is="coral-table-cell" value="0"></td>
    <% } %>

    <% if (isProcessingProfileEntitled && !profileTitleList[1].trim().isEmpty()) { %>
        <td is="coral-table-cell" value="0"><%= xssAPI.encodeForHTML(profileTitleList[1].trim()) %></td>
    <% } else{ %>
        <td is="coral-table-cell" value="0"></td>
    <% } %>

    <% if (isProcessingProfileEntitled && !profileTitleList[2].trim().isEmpty()) { %>
        <td is="coral-table-cell" value="0"><%= xssAPI.encodeForHTML(profileTitleList[2].trim()) %></td>
    <% } else{ %>
        <td is="coral-table-cell" value="0"></td>
    <% } %>

    <cq:include script = "reorder.jsp"/>
    <cq:include script = "meta.jsp"/>
</tr><%!

%>
