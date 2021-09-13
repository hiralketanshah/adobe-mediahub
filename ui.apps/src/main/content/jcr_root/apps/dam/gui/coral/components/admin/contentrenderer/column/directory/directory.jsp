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
%><%
%><%@page import="org.apache.sling.api.resource.Resource,
          java.util.Arrays,
          org.apache.jackrabbit.api.security.user.UserManager,
				  java.util.Iterator,
				  org.apache.sling.jcr.base.util.AccessControlUtil,
          org.apache.jackrabbit.api.security.user.User,
          org.apache.jackrabbit.api.security.user.UserManager,
          org.apache.jackrabbit.api.security.user.Group,
          javax.jcr.Session"%><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/init/directoryBase.jsp"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/column/common/common.jsp"%><%

String directoryActionRels = StringUtils.join(UIHelper.getDirectoryActionRels(hasJcrRead, hasModifyAccessControl, hasJcrWrite, hasReplicate, isMACShared, isCCShared, isRootMACShared, isMPShared, isRootMPShared, isLiveCopy, hasAddChild, hasRemoveNode, hasModifyProperties), " ");

String name = resource.getName(); 
request.setAttribute("actionRels", actionRels.concat(" " + directoryActionRels));

attrs.add("itemscope", "itemscope");
attrs.add("data-item-title", resourceTitle);
attrs.add("data-item-type", type);


if (hasChildren(resource)) {
    attrs.add("variant", "drilldown");
} else {
  String path = resource.getPath();
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

}

request.setAttribute("com.adobe.assets.meta.attributes", metaAttrs);

%><coral-columnview-item <%= attrs %>>
<cq:include script = "meta.jsp"/>
    <coral-columnview-item-thumbnail>
        <coral-icon icon="folder"></coral-icon>
        </coral-columnview-item-thumbnail>
    <coral-columnview-item-content>
        <div class="foundation-collection-item-title" itemprop="title" title="<%= xssAPI.encodeForHTMLAttr(resourceTitle) %>">
            <%= xssAPI.encodeForHTML(resourceTitle) %>
        </div><%
        if (name != null && !name.equals(resourceTitle)) {
            %><div class="foundation-layout-util-subtletext">
                <%= xssAPI.encodeForHTML(name) %>
            </div><%
        }%>
    </coral-columnview-item-content>
<cq:include script = "applicableRelationships.jsp"/>
<cq:include script = "link.jsp"/>
</coral-columnview-item><%!
//Add private methods here
%>
