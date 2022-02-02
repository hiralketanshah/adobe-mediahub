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
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page session="false"
          import="com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.rendercondition.RenderCondition,
                  org.apache.sling.api.resource.Resource,
                  com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition,
                  org.apache.jackrabbit.api.security.user.User,
				  org.apache.sling.jcr.base.util.AccessControlUtil,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
				  javax.jcr.Session,
                  com.adobe.granite.security.user.UserPropertiesService" %><%
Config cfg = cmp.getConfig();
String path = cmp.getExpressionHelper().getString(cfg.get("resourcePath", (String)null));
Boolean render = false;
if (path != null) {
    Resource userResource = resourceResolver.getResource(path);
    if (userResource != null && userResource.adaptTo(User.class) != null) {

		UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
        boolean isAdmin = false;
        if(userManager != null){
            User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
            Group group = (Group)userManager.getAuthorizable("administrators");
            if(currentUser != null){
                if(group != null){
                    isAdmin = group.isMember(currentUser) || "admin".equals(resourceResolver.getUserID());
                }
                if( (!isAdmin) && (userManager.getAuthorizable("mediahub-administrators") != null) ){
                    isAdmin = ((Group)userManager.getAuthorizable("mediahub-administrators")).isMember(currentUser);
                }
                if( (!isAdmin) && (userManager.getAuthorizable("mediahub-super-administrators") != null) ){
                    isAdmin = ((Group)userManager.getAuthorizable("mediahub-super-administrators")).isMember(currentUser);
                }
            }
        }
            Resource privateProfileResource = userResource.getChild(UserPropertiesService.PUBLIC_PROFILE);
            if (privateProfileResource != null
                    && privateProfileResource.getResourceType().equals("cq/security/components/profile") && isAdmin) {
                render = true;
            }
        }
    }

request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(render));
%>