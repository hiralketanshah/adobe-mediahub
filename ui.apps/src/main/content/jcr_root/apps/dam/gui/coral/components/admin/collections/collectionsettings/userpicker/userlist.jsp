<%@page session="false" %>
<%@include file="/libs/granite/ui/global.jsp"%>
<%@page import="javax.jcr.Session,
        org.apache.jackrabbit.api.JackrabbitSession,
        javax.jcr.RepositoryException,
        com.adobe.granite.security.user.UserProperties,
        com.adobe.granite.security.user.UserPropertiesManager,
        com.adobe.granite.security.user.UserPropertiesService,
        org.apache.jackrabbit.api.security.user.UserManager,
        org.apache.jackrabbit.api.security.user.Authorizable,
        org.apache.jackrabbit.api.security.user.Query,
        org.apache.jackrabbit.api.security.user.QueryBuilder,
        org.apache.jackrabbit.api.security.user.User,org.apache.jackrabbit.api.security.user.Group,
        com.adobe.granite.ui.components.Config,
        org.apache.sling.api.resource.ResourceResolver,
        org.apache.jackrabbit.api.security.principal.PrincipalManager,
        org.apache.sling.jcr.base.util.AccessControlUtil,
        java.security.Principal,
        java.util.Iterator,
        java.util.List,
        java.util.ArrayList,
        org.slf4j.Logger,
        org.slf4j.LoggerFactory,
        org.apache.sling.tenant.Tenant"%><%

	Config cfg = new Config(resource);
    final Logger LOG = LoggerFactory.getLogger(getClass());
    String search = slingRequest.getParameter("query");
    if (search == null) {
    	return;
    }
    String searchProcessedOriginal = search.trim();
    String searchProcessed = searchProcessedOriginal.toLowerCase();
    String offsetString = slingRequest.getParameter("start");
    String limitString = slingRequest.getParameter("end");
    String impersonatesOnlyString = slingRequest.getParameter("impersonatesOnly");
    String searchById = slingRequest.getParameter("searchById");
    final boolean impersonatesOnly = "true".equals(impersonatesOnlyString) || "1".equals(impersonatesOnlyString);
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
        if (userGroup == null) {
            userGroups = cfg.get("userGroups", String[].class);
            if (userGroups != null && userGroups.length > 0) {
                userGroup = userGroups[0];
            }
        }
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
	String user = null;
    ResourceResolver userResolver = null;
    UserManager um = null;
    UserPropertiesService upService = sling.getService(UserPropertiesService.class);
    List<Authorizable> authorizables = new ArrayList<Authorizable>();
    final String nameDisplayOrder = i18n.get("{0} {1}","name display order: {0} is the given (first) name, {1} the family (last) name","givenName middleName","familyName");
    try {
        userResolver = resource.getResourceResolver();
        userSession = userResolver.adaptTo(Session.class);
        user = userSession.getUserID();
        um = AccessControlUtil.getUserManager(userSession);
        PrincipalManager pm = ((JackrabbitSession) userSession).getPrincipalManager();
        final UserPropertiesManager upm = upService.createUserPropertiesManager(userSession, resourceResolver);
		boolean isExt = isExternalUser(um.getAuthorizable(userSession.getUserID()), upm);
        boolean isAdmin = isAdminUser(um.getAuthorizable(userSession.getUserID()), upm);

        if ("true".equals(searchById)) {
           authorizables.add(um.getAuthorizable(searchProcessed));
        } else {

            Principal groupPrincipal = null;
            Authorizable authorizable = null;
            if(userGroup != null) {
                groupPrincipal = pm.getPrincipal(userGroup);
                authorizable = um.getAuthorizable(groupPrincipal);
            }

            if (!isExt && (authorizable == null || authorizable.isGroup())) {
                boolean showDeclaraedGroupMembersOnly = cfg.get("showDeclaraedGroupMembersOnly", false);
                String scope = authorizable != null ? authorizable.getID() : null;
                // create query
                Query q = createQuery(searchProcessedOriginal,
                        scope, offset, limit, showDeclaraedGroupMembersOnly, isExt, isAdmin);
                Iterator<? extends Authorizable> result = um.findAuthorizables(q);
                // iterate over the result and append to authorizables
                while (result.hasNext()) {
                    Authorizable next = result.next();
                    if (next.isGroup() ||  !((User)next).isSystemUser()){
                        authorizables.add(next);
                    }
                }
            }

            if (isExt && (authorizable == null || authorizable.isGroup())) {

                boolean showDeclaraedGroupMembersOnly = cfg.get("showDeclaraedGroupMembersOnly", false);

               List<String> groups = getGroups(um.getAuthorizable(userSession.getUserID()));
                for(String group : groups){
					String scope = authorizable != null ? authorizable.getID() : null;
                    // create query
                    Query q = createQuery(searchProcessedOriginal,
                            group, -1, -1, showDeclaraedGroupMembersOnly, isExt, isAdmin);
                    Iterator<? extends Authorizable> result = um.findAuthorizables(q);
                    // iterate over the result and append to authorizables
                    while (result.hasNext()) {
                        Authorizable next = result.next();
                        if ((next.isGroup() ||  !((User)next).isSystemUser()) && !authorizables.contains(next)){
                            authorizables.add(next);
                        }
                    }

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


    for(Authorizable authorizable: authorizables) {

        UserProperties up = upm.getUserProperties(authorizable, "profile");

        String name = getFullName(authorizable,  up, nameDisplayOrder);
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

		  String principalName = authorizable.getPrincipal().getName();
		  %><li class="coral-SelectList-item coral-SelectList-item--option foundation-layout-flexmedia" data-principal-name="<%= xssAPI.encodeForHTMLAttr(principalName) %>" data-value="<%= xssAPI.encodeForHTMLAttr(authorizable.getID()) %>" data-display="<%= xssAPI.encodeForHTMLAttr(displayData) %>" data-name="<%= nameHtml %>" data-email="<%= xssAPI.encodeForHTMLAttr(email) %>">	
          <% if (image != null && image.length() > 0) { %>
                 <img class="foundation-layout-flexmedia-img" width="32" height="32" src="<%=  xssAPI.getValidHref(image) %>" alt="<%= xssAPI.encodeForHTMLAttr(name) %>"><%
             }
          %><div class="foundation-layout-flexmedia-bd"><%
              %><div class="foundation-layout-flexmedia-bd-singleline"><%= nameHtml %></div><%
              if(!email.trim().isEmpty()){
                %><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= xssAPI.encodeForHTMLAttr(email) %></div><%
              }
              else if (!name.equals(authorizable.getID())) {
             %><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= xssAPI.encodeForHTMLAttr(authorizable.getID()) %></div><%
              }
          %></div><%
      %></li><%
    }
    } catch (RepositoryException e) {
        log("exception while using user session", e);
    }

%><%!
    /** Get a full name string for a user
     */
    private String getFullName(Authorizable authorizable, UserProperties up, String nameDisplayOrder) {
        try {
            // if we have no user profile, we return the ID
            if (up == null) return authorizable.getID();

            String name = up.getDisplayName(nameDisplayOrder);
            if (name.length() == 0) name = authorizable.getID();
            return name;
        } catch (RepositoryException e) {
           return "";
        }
    }

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
                              final long offset, final long limit,
                              final boolean showDeclaraedGroupMembersOnly, final boolean isExt, final boolean isAdmin) {
        Query q = new Query() {
            public <T> void build(QueryBuilder<T> queryBuilder) {
                T condition = null;

                if (queryStringOriginal != null && !queryStringOriginal.isEmpty()) {
                    condition = queryBuilder.or(queryBuilder.contains(".", queryStringOriginal), queryBuilder.contains(".", queryStringOriginal + "*"));
                }
                queryBuilder.setCondition(condition);
                if(group != null) {
                    queryBuilder.setScope(group, showDeclaraedGroupMembersOnly);
                }
                if(offset!=-1 && limit !=-1){
					queryBuilder.setLimit(offset, limit);
                }
                queryBuilder.setSelector(User.class);
                if(isAdmin){
                	queryBuilder.setSelector(Authorizable.class);
                }
            }
        };
        return q;
    }

	private Boolean isExternalUser(Authorizable user, UserPropertiesManager upm)throws RepositoryException{
		UserProperties up = upm.getUserProperties(user, "profile");
		boolean returnValue = false;
		if(null != up){
			returnValue = up.getProperty("type", "", String.class).equalsIgnoreCase("external") ? true : false;
		}
		return returnValue;
	}

	private Boolean isAdminUser(Authorizable user, UserPropertiesManager upm)throws RepositoryException{
        boolean isAdminUser = false;
		Iterator<Group> groups = user.memberOf();
        while(groups.hasNext()) {
			Group group = groups.next();
			if(null!=group.getID() && group.getID().matches("administrators|mediahub-administrators|mediahub-super-administrators|mediahub-basic-entity-manager")) {
				isAdminUser = true;
				break;
			}
		}
        return isAdminUser;
	}

	private List<String> getGroups(Authorizable user)throws RepositoryException{
        Iterator<Group> groups = user.memberOf();
        List<String> listOfGroups = new ArrayList<>();

		while(groups.hasNext()) {
			Group group = groups.next();
			if(null!=group.getID() && group.getID().contains("external-contributor")) {
				String groupName = group.getID();
                String[] arrayOfGroupName = groupName.split("-external-contributor",2);
                listOfGroups.add(arrayOfGroupName[0]);

			}
		}
        return listOfGroups;

	}

%>

