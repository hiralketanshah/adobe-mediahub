<%--
* ADOBE CONFIDENTIAL
* ___________________
*
* Copyright 2020 Adobe
* All Rights Reserved.
*
* NOTICE: All information contained herein is, and remains
* the property of Adobe and its suppliers, if any. The intellectual
* and technical concepts contained herein are proprietary to Adobe
* and its suppliers and are protected by all applicable intellectual
* property laws, including trade secret and copyright laws.
* Dissemination of this information or reproduction of this material
* is strictly forbidden unless prior written permission is obtained
* from Adobe.
--%>
<%@page session="false" %>
<%@include file="/libs/granite/ui/global.jsp"%>
<%@page import="javax.jcr.Session,
        org.apache.jackrabbit.api.JackrabbitSession,
        javax.jcr.RepositoryException,
        com.adobe.granite.security.user.util.AuthorizableUtil,
        com.adobe.granite.security.user.UserProperties,
        com.adobe.granite.security.user.UserPropertiesManager,
        com.adobe.granite.security.user.UserPropertiesService,
        org.apache.jackrabbit.api.security.user.UserManager,
        org.apache.jackrabbit.api.security.user.Authorizable,
        org.apache.jackrabbit.api.security.user.Query,
        org.apache.jackrabbit.api.security.user.QueryBuilder,
        org.apache.jackrabbit.api.security.user.User,
        com.adobe.granite.ui.components.Config,
        com.adobe.granite.ui.components.AttrBuilder,
        org.apache.sling.api.resource.ResourceResolver,
        org.apache.jackrabbit.api.security.principal.PrincipalManager,
        org.apache.sling.jcr.base.util.AccessControlUtil,
        java.security.Principal,
        java.util.Iterator,
        java.util.List,
        java.util.ArrayList,
        org.apache.sling.tenant.Tenant"%><%

  Config cfg = new Config(resource);
    String search = slingRequest.getParameter("query");
    if (search == null) {
      return;
    }
    String searchProcessedOriginal = search.replace("%20"," ").trim();
    String searchProcessed = searchProcessedOriginal.toLowerCase();
    String offsetString = slingRequest.getParameter("start");
    String limitString = slingRequest.getParameter("end");
    String searchById = slingRequest.getParameter("searchById");
    String userGroups[] = null;
    Tenant tenant = resourceResolver.adaptTo(Tenant.class);
    if (tenant != null) {
        userGroups = new String[]{(String)tenant.getProperty("dam:allUsersGroupId")};
    }
    String userGroup = null;
    if (userGroups != null && userGroups.length > 0) {
        userGroup = userGroups[0];
    } else {
        userGroup = cfg.get("userGroup", String.class);
    }

    if (userGroup == null) {
        return;
    }

    long offset = 0;
    long limit = 25;
    try {
        offset = Long.parseLong(offsetString);
        limit = Long.parseLong(limitString);
    } catch (Exception e) {
        //eat it..
    }

    Session userSession = null;
    ResourceResolver userResolver = null;
    UserManager um = null;
    UserPropertiesService upService = sling.getService(UserPropertiesService.class);
    List<Authorizable> authorizables = new ArrayList<Authorizable>();
    final String nameDisplayOrder = i18n.get("{0} {1}","name display order: {0} is the given (first) name, {1} the family (last) name","givenName middleName","familyName");
    boolean emailFound = false;

    try {
        userResolver = resource.getResourceResolver();
        userSession = userResolver.adaptTo(Session.class);
        um = AccessControlUtil.getUserManager(userSession);
        PrincipalManager pm = ((JackrabbitSession) userSession).getPrincipalManager();
        final UserPropertiesManager upm = upService.createUserPropertiesManager(userSession, resourceResolver);
        if ("true".equals(searchById)) {
           authorizables.add(um.getAuthorizable(searchProcessed));
        } else {
            Principal groupPrincipal = pm.getPrincipal(userGroup);
            Authorizable authorizable = um.getAuthorizable(groupPrincipal);

            if (authorizable.isGroup()) {
                String sortOn = cfg.get("sortOn", UserPropertiesService.PROFILE_PATH + "/@"+ UserProperties.GIVEN_NAME);
                QueryBuilder.Direction sortOrder = null;
                if (cfg.get("sortOrder", String.class) != null) {
                    sortOrder = QueryBuilder.Direction.valueOf(cfg.get("sortOrder", String.class));
                }
                boolean showDeclaraedGroupMembersOnly = cfg.get("showDeclaraedGroupMembersOnly", false);
                //create query
                Query q = createQuery(searchProcessedOriginal,
                            authorizable.getID(), offset, limit, sortOn, sortOrder, showDeclaraedGroupMembersOnly, userSession);

                Iterator<? extends Authorizable> result = um.findAuthorizables(q);
                // iterate over the result and append to authorizables
                while (result.hasNext()) {
                    authorizables.add(result.next());
                }
            }

            if (!authorizables.isEmpty()) {
                //removing duplicate users
                String previousID = authorizables.get(0).getID();
                for (int i=1;i<authorizables.size();) {
                    String currentID = authorizables.get(i).getID();
                    if (previousID.compareTo(currentID) == 0 ) {
                        authorizables.remove(i);
                    } else {
                        previousID = currentID;
                        i++;
                    }
                }
            }
        }
%><coral-buttonlist><%
        for (Authorizable authorizable: authorizables) {
            if(!authorizable.isGroup()){
            UserProperties up = upm.getUserProperties(authorizable, "profile");
            String principalName = authorizable.getPrincipal().getName();
            String name = AuthorizableUtil.getFormattedName(userResolver, authorizable, nameDisplayOrder);
            String nameHtml = xssAPI.encodeForHTML(name);
            String email = getEmail(up);
            String displayData = name;
            displayData += !email.trim().isEmpty() ? ( " <" + email + ">" ) : "";

            int i = name.toLowerCase().indexOf(searchProcessed.toLowerCase());
            if (i >= 0 && i < name.length()) {
                nameHtml = xssAPI.encodeForHTML(name.substring(0, i)) +
                        "<b>" + xssAPI.encodeForHTML(name.substring(i, i + searchProcessed.length())) + "</b>" +
                        xssAPI.encodeForHTML(name.substring(i + searchProcessed.length()));
            }

            String image = (up == null) ? "" : up.getResourcePath(UserProperties.PHOTOS, "/primary/image", "");
            if (image == null || image.equals("")) {
                if (authorizable.isGroup()) {
                    image = "/libs/granite/security/clientlib/themes/default/resources/sample-group-thumbnail.36.png";
                } else {
                    image = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.36.png";
                }
            }
            image  = request.getContextPath() + image;

            AttrBuilder attrs = new AttrBuilder(request, xssAPI);
            attrs.add("type", "button");
            attrs.add("role", "option");
            attrs.add("is", "coral-buttonlist-item");
            attrs.add("value", displayData);
            attrs.add("foundation-picker-buttonlist-text", name);
            attrs.addClass("userpicker-add-user");

            attrs.addOther("authorizableid", authorizable.getID());
            attrs.addOther("principal-name", principalName);
            attrs.addOther("name", name);
            attrs.addOther("email", email);
            attrs.addOther("image", image);

            %><button <%= attrs %>>
                <div class="foundation-layout-flexmedia foundation-layout-flexmedia-middle">
                    <img class="foundation-layout-flexmedia-img" width="32" height="32" src="<%=  xssAPI.getValidHref(image) %>" alt="">
                    <div class="foundation-layout-flexmedia-bd">
                        <div class="foundation-layout-flexmedia-bd-singleline"><%= nameHtml %></div><%
                        if(!email.trim().isEmpty()){
                          if (email.equalsIgnoreCase(searchProcessed)) {
                              emailFound = true;
                          }
                          %><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(email) %></div><%
                        } else if (!name.equals(authorizable.getID())) {
                            %><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= xssAPI.encodeForHTML(authorizable.getID()) %></div><%
                        }
                    %></div><%
                %></div><%
            %></button><%
            }
		    }
    } catch (RepositoryException e) {
        log("exception while using user session", e);
    }
%></coral-buttonlist><%!

    private String getEmail(UserProperties up){
        try {
            return up != null? up.getProperty(UserProperties.EMAIL, "", String.class) : "";
        } catch (RepositoryException e) {
           return "";
        }
    }

    /**
     * Create a query to search for users
     */
     private Query createQuery(final String queryStringOriginal, final String group,
             final long offset, final long limit, final String sortOn,
             final QueryBuilder.Direction sortOrder,
             final boolean showDeclaraedGroupMembersOnly,
             final Session session) {

        Query q = new Query() {
            public <T> void build(QueryBuilder<T> queryBuilder) {
                T condition = null;

                if (queryStringOriginal != null && !queryStringOriginal.isEmpty()) {
                    condition = queryBuilder.or(queryBuilder.contains(".", queryStringOriginal), queryBuilder.contains(".", queryStringOriginal + "*"));
                }
                // Condition is: Hide the user if it's a service user
                 T hsuCondition = null;
                 try {
                     hsuCondition = queryBuilder.not(queryBuilder.eq("@jcr:primaryType", session.getValueFactory().createValue("rep:SystemUser")));
                     condition = (condition == null) ? hsuCondition : queryBuilder.and(hsuCondition, condition);
                 } catch (RepositoryException e) {
                     log("Exception while creating a condition to hide service user ", e);
                 }
                if (condition != null) {
                    queryBuilder.setCondition(condition);
                }
                queryBuilder.setScope(group, showDeclaraedGroupMembersOnly);
                queryBuilder.setSelector(Authorizable.class);
                queryBuilder.setSortOrder(sortOn, sortOrder);
                queryBuilder.setLimit(offset, limit);
            }
        };
        return q;
    }
%>
