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
--%><%@page session="false" %><%
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@page import="
        javax.jcr.Session,
        javax.jcr.RepositoryException,
        com.adobe.granite.security.user.UserProperties,
        com.adobe.granite.security.user.UserPropertiesManager,
        com.adobe.granite.security.user.UserPropertiesService,
        com.adobe.granite.security.user.util.AuthorizableUtil,
        org.apache.jackrabbit.api.security.user.UserManager,
        org.apache.jackrabbit.api.security.user.Authorizable,    
        org.apache.sling.api.resource.ResourceResolver,
        org.apache.sling.commons.json.io.JSONWriter"
        %>
<%

    String userId = slingRequest.getParameter("userid");
    if (userId==null || userId.trim().length()==0) {
        return;
    }

    response.setContentType("application/json");
    response.setCharacterEncoding("utf-8");

    ResourceResolver resolver = null;
    resolver = slingRequest.getResourceResolver();
    Session session = resolver.adaptTo(Session.class);

    UserManager um = resolver.adaptTo(UserManager.class);
    UserPropertiesService upService = sling.getService(UserPropertiesService.class);
    final UserPropertiesManager upm = upService.createUserPropertiesManager(session, resolver);

    Authorizable authorizable = um.getAuthorizable(userId);

    JSONWriter w = new JSONWriter(response.getWriter());
    String avatarPath = null;
    if (authorizable != null ) {
        UserProperties up = AuthorizableUtil.getProfile(upm, authorizable.getID());
        w.object();
        w.key("userId").value(userId);
        w.key("name").value(getFullName(authorizable,  up));
        w.key("type").value(up.getProperty("type"));
        if (up != null) {
            w.key("email").value(up.getProperty(UserProperties.EMAIL));
            avatarPath = up.getResourcePath(UserProperties.PHOTOS, "/primary/image", "");
            if (avatarPath != null && !avatarPath.equals("")) {
                w.key("avatar").value(avatarPath);
            }
        }


        if (avatarPath == null || avatarPath.equals("")) {
            // set default avatar for users and groups if there is no profile
            String avatar;
            if (authorizable.isGroup()) {
                avatar = "/libs/granite/security/clientlib/themes/default/resources/sample-group-thumbnail.36.png";
            } else {
                avatar = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.100.png";
            }
            avatarPath = avatar;
            w.key("avatar").value(avatarPath);
        }

        w.endObject();
    }


%><%!
    /** Get a full name string for a user
     */
    private String getFullName(Authorizable user, UserProperties up) {
        try {
            // if we have no user profile, we return the ID
            if (up == null) return user.getID();

            String givenName = up.getProperty(UserProperties.GIVEN_NAME);
            String familyName = up.getProperty(UserProperties.FAMILY_NAME);
            String name = "";
            if (givenName != null) name += givenName;
            if (givenName != null && familyName != null) name += " ";
            if (familyName != null) name += familyName;
            if (name.length() == 0) name = user.getID();
            return name;
        } catch (RepositoryException e) {
            return "";
        }
    }
%>
