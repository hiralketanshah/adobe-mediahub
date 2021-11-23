<%--
  ADOBE CONFIDENTIAL
  ___________________

  Copyright 2015 Adobe
  All Rights Reserved.

  NOTICE: All information contained herein is, and remains
  the property of Adobe and its suppliers, if any. The intellectual
  and technical concepts contained herein are proprietary to Adobe
  and its suppliers and are protected by all applicable intellectual
  property laws, including trade secret and copyright laws.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe.
--%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
           import="java.text.Collator,
                  java.util.ArrayList,
                  java.util.Collections,
                  java.util.Comparator,
                  java.util.Iterator,
                  java.util.List,
                  javax.jcr.RepositoryException,
                  javax.jcr.Session,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.jackrabbit.api.security.user.Group,
                  org.apache.jackrabbit.api.security.user.Query,
                  org.apache.jackrabbit.api.security.user.QueryBuilder,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.sling.api.resource.ResourceResolver,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.UserPropertiesService,
                  com.adobe.granite.security.user.UserManagementService,
                  com.adobe.granite.security.user.util.AuthorizableUtil,
                  com.adobe.granite.ui.components.AttrBuilder" %><%
%><%
    String search = request.getParameter("query");
    if (search == null) {
        search = "";
    }
    String searchProcessed = search.replace("%20", " ").trim();

    boolean impersonatesOnly = "true".equals(request.getParameter("impersonatesOnly"));
    boolean groupsOnly = "true".equals(request.getParameter("groupsOnly"));
    boolean serviceUsersOnly = "true".equals(request.getParameter("serviceUsersOnly"));
    boolean hideServiceUsers = "true".equals(request.getParameter("hideServiceUsers"));
    boolean useHomeAsValue = "true".equals(request.getParameter("useHomeAsValue"));
    boolean showAllAuthorizables = "true".equals(request.getParameter("showAllAuthorizables"));

    int offset = 0;
    try {
        offset = Integer.parseInt(request.getParameter("start"));
    } catch (Exception ignore) {
    }

    int limit = 25;
    try {
        limit = Integer.parseInt(request.getParameter("end")) - offset;
    } catch (Exception ignore) {
    }

    final ResourceResolver resolver = resourceResolver;
    final Session session = resolver.adaptTo(Session.class);
    UserManager um = resolver.adaptTo(UserManager.class);
    UserPropertiesService ups = sling.getService(UserPropertiesService.class);
    final UserManagementService userManagementService = sling.getService(UserManagementService.class);
    final UserPropertiesManager upm = ups.createUserPropertiesManager(session, resolver);
    User currentUser = resolver.adaptTo(User.class);

    Query query = createQuery(session, userManagementService, searchProcessed, currentUser, impersonatesOnly, groupsOnly, serviceUsersOnly, hideServiceUsers, showAllAuthorizables, offset, limit);
    Iterator<Authorizable> authorizablesIt = um.findAuthorizables(query);

    List<Authorizable> authorizables = new ArrayList<Authorizable>();
    while (authorizablesIt.hasNext()) {
        authorizables.add(authorizablesIt.next());
    }

    final String nameDisplayOrder = i18n.get("{0} {1}","name display order: {0} is the given (first) name, {1} the family (last) name","givenName middleName","familyName");
    final Collator langCollator = Collator.getInstance(request.getLocale());

    Collections.sort(authorizables, new Comparator<Authorizable>() {
        public int compare(Authorizable u1, Authorizable u2) {
            try {
                UserProperties up1 = upm.getUserProperties(u1, "profile");
                String name1 = AuthorizableUtil.getFormattedName(resolver, u1, nameDisplayOrder);

                UserProperties up2 = upm.getUserProperties(u2, "profile");
                String name2 = AuthorizableUtil.getFormattedName(resolver, u2, nameDisplayOrder);

                return langCollator.compare(name1, name2);
            } catch (RepositoryException e) {
                return 0;
            }
        }
    });

    String defaultUserImagePath = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.36.png";
    String defaultGroupImagePath = "/libs/granite/security/clientlib/themes/default/resources/sample-group-thumbnail.36.png";

    for (Authorizable authorizable : authorizables) {
        String defaultImage = authorizable.isGroup() ? defaultGroupImagePath : defaultUserImagePath;
        UserProperties up = upm.getUserProperties(authorizable, "profile");

        String name = AuthorizableUtil.getFormattedName(resolver, authorizable.getID(), nameDisplayOrder);
        String image = (up == null || authorizable.isGroup()) ? defaultImage : up.getResourcePath(UserProperties.PHOTOS + "/primary/image", null, defaultUserImagePath);

        String nameHtml = xssAPI.encodeForHTML(name);

        int i = name.toLowerCase().indexOf(searchProcessed.toLowerCase());
        if (i >= 0) {
            nameHtml = xssAPI.encodeForHTML(name.substring(0, i)) +
                    "<b>" + xssAPI.encodeForHTML(name.substring(i, i + searchProcessed.length())) + "</b>" +
                    xssAPI.encodeForHTML(name.substring(i + searchProcessed.length()));
        }

        AttrBuilder liAttrs = new AttrBuilder(request, xssAPI);
        liAttrs.addClass("js-userpicker-item coral-SelectList-item coral-SelectList-item--option foundation-layout-flexmedia foundation-layout-flexmedia-middle");
        liAttrs.add("data-value", useHomeAsValue ? authorizable.getPath() : authorizable.getID());
        liAttrs.add("data-display", name);

%><li <%= liAttrs %>><%
%><img class="foundation-layout-flexmedia-img" width="32" height="32" src="<%= xssAPI.getValidHref(request.getContextPath() + image) %>" alt=""><%
%><div class="foundation-layout-flexmedia-bd"><%
%><div class="foundation-layout-flexmedia-bd-singleline"><%= nameHtml %></div><%
    if (!name.equals(authorizable.getID())) {
%><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(authorizable.getID()) %></div><%
    }
%></div><%
%></li><%
    }
%><%!
    private Query createQuery(final Session session, final UserManagementService userManagementService, final String queryString, final User user, final boolean impersonatesOnly,
                              final boolean groupsOnly, final boolean serviceUsersOnly, final boolean hideServiceUsers, final boolean showAllAuthorizables,
                              final int offset, final int limit) {
        return new Query() {
            public <T> void build(QueryBuilder<T> builder) {
                T condition = null;

                if (queryString != null && queryString.length() > 0) {
                    condition = builder.or(builder.contains(".", queryString), builder.contains(".", queryString + "*"));
                }

                if (impersonatesOnly) {
                    try {
                        // Condition is: Search for allowed impersonates OR in case of admins,
                        // give all users EXCEPT oneself and EXCEPT "anonymous"
                        T impCondition = (user.isAdmin() ?
                                builder.and(builder.not(builder.nameMatches(user.getID())), builder.not(builder.nameMatches(userManagementService.getAnonymousId())))
                                : builder.impersonates(user.getPrincipal().getName()));
                        condition = (condition == null) ? impCondition : builder.and(impCondition, condition);
                    } catch(RepositoryException e) {
                    }
                }

                if (serviceUsersOnly) {
                    // Condition is: Show the user if it's a service user
                    T hsuCondition = null;
                    try {
                        hsuCondition = builder.eq("@jcr:primaryType", session.getValueFactory().createValue("rep:SystemUser"));
                    } catch (RepositoryException e) {
                    }
                    condition = (condition == null) ? hsuCondition : builder.and(hsuCondition, condition);
                }

                if (hideServiceUsers) {
                    // Condition is: Hide the user if it's a service user
                    T hsuCondition = null;
                    try {
                        hsuCondition = builder.not(builder.eq("@jcr:primaryType", session.getValueFactory().createValue("rep:SystemUser")));
                    } catch (RepositoryException e) {
                    }
                    condition = (condition == null) ? hsuCondition : builder.and(hsuCondition, condition);
                }

                if (condition != null) {
                    builder.setCondition(condition);
                }

                if (!showAllAuthorizables) {
                    builder.setSelector(groupsOnly ? Group.class : User.class);
                }
                builder.setLimit(offset, limit);
            }
        };
    }
%>