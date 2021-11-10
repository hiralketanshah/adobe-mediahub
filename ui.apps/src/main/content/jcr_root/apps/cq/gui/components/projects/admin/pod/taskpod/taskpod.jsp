<%--
  ADOBE CONFIDENTIAL

  Copyright 2012 Adobe Systems Incorporated
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
          java.util.Arrays,
          org.apache.jackrabbit.api.security.user.UserManager,
				  java.util.Iterator,
				  org.apache.sling.jcr.base.util.AccessControlUtil,
          org.apache.jackrabbit.api.security.user.User,
          org.apache.jackrabbit.api.security.user.UserManager,
          org.apache.jackrabbit.api.security.user.Group,
          javax.jcr.Session" session="false" %><%
%><ui:includeClientLib categories="cq.projects.admin.actions.delete" /><%
  request.setAttribute("projectLinkResource", resource);

  UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
    boolean isAdmin = false;

    if(userManager != null){
        User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
        Group group = (Group)userManager.getAuthorizable("administrators");
        if(currentUser != null){
            if(group != null){
              isAdmin = group.isMember(currentUser);
            }
        }
    }
	if (isAdmin) {
%><sling:include path="/mnt/overlay/cq/core/content/projects/dashboard/default/tasklink/jcr:content"/><%
                 }
  request.setAttribute("projectLinkResource", null);
%>