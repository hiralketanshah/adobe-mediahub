<%
%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
          import="java.text.Collator,
                  java.util.Collections,
                  java.util.Comparator,
                  java.util.Iterator,
                  java.util.List,
                  java.util.regex.Matcher,
                  java.util.regex.Pattern,
                  javax.jcr.Session,
                  javax.jcr.RepositoryException,
                  org.apache.commons.collections4.IteratorUtils,
                  org.apache.commons.collections4.Transformer,
                  org.apache.commons.collections4.iterators.TransformIterator,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.sling.api.resource.ResourceResolver,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.UserPropertiesService,
                  com.adobe.granite.security.user.util.AuthorizableUtil,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
				  org.apache.sling.jcr.base.util.AccessControlUtil,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
				  javax.jcr.Session,
                  com.adobe.granite.ui.components.ExpressionHelper" %><%--###
Suggestion
==========

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/authorizable/autocomplete/suggestion

   The implementation of autocomplete suggestion that is specific for granite:servercomponent:`/libs/granite/ui/components/coral/foundation/authorizable/autocomplete`

   It has the following content structure:

   .. gnd:gnd::

      [granite:AuthorizableAutocompleteSuggestion]

      /**
       * The query entered by the user, which will be used to highlight the suggestion.
       */
      - query (StringEL)

      /**
       * The type of value to be submitted:
       *
       * id
       *    Use authorizable ID
       * path
       *    Use authorizable home path
       * principalname
       *    Use principal name
       */
      - valueType (StringEL) = 'id' < 'id', 'path', 'principalname'
###--%><%

final String FOUNDATION_PICKER_BUTTONLIST_PREFIX = "foundation-picker-buttonlist-custom-";

UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
    boolean isEntityManager = false;
    if(userManager != null){
        User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
        Group group = (Group)userManager.getAuthorizable("mediahub-basic-entity-manager");
        if(null!=group){
			isEntityManager = group.isMember(currentUser);
        }

    }

ExpressionHelper ex = cmp.getExpressionHelper();
Config cfg = cmp.getConfig();
final ResourceResolver resolver = resourceResolver;
UserPropertiesService ups = sling.getService(UserPropertiesService.class);
UserPropertiesManager upm = ups.createUserPropertiesManager(resolver.adaptTo(Session.class), resolver);

String query = ex.getString(cfg.get("query", String.class));
String valueType = ex.getString(cfg.get("valueType", String.class));

Pattern queryPattern = Pattern.compile(Pattern.quote(query), Pattern.CASE_INSENSITIVE);
final String nameDisplayOrder = i18n.get("{0} {1}", "name display order: {0} is the given (first) name, {1} the family (last) name", "givenName middleName", "familyName");
final Collator langCollator = Collator.getInstance(request.getLocale());
String defaultUserImageUrl = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.36.png";
String defaultGroupImageUrl = "/libs/granite/security/clientlib/themes/default/resources/sample-group-thumbnail.36.png";

List<Authorizable> authorizables = IteratorUtils.toList(new TransformIterator<>(cmp.getItemDataSource().iterator(), new Transformer<Resource, Authorizable>() {
    public Authorizable transform(Resource o) {
        return o.adaptTo(Authorizable.class);
    }
}));

Collections.sort(authorizables, new Comparator<Authorizable>() {
    @Override
    public int compare(Authorizable u1, Authorizable u2) {
        String name1 = AuthorizableUtil.getFormattedName(resolver, u1, nameDisplayOrder);
        String name2 = AuthorizableUtil.getFormattedName(resolver, u2, nameDisplayOrder);

        return langCollator.compare(name1, name2);
    }
});

%><coral-buttonlist><%
    for (Authorizable auth : authorizables) {
        if (auth instanceof User && ((User) auth).isDisabled()) {
            continue;
        }
    if(isEntityManager && (!auth.getPath().startsWith("/home/groups/mediahub") || auth.getPrincipal().getName().equalsIgnoreCase("mediahub-administrators") || auth.getPrincipal().getName().equalsIgnoreCase("mediahub-super-administrators") || auth.getPrincipal().getName().equalsIgnoreCase("mediahub-projects-users") || auth.getPrincipal().getName().equalsIgnoreCase("mediahub-basic-entity-manager") || auth.getPrincipal().getName().equalsIgnoreCase("mediahub-project-administrator"))) {
			continue;
		}

        UserProperties up = upm.getUserProperties(auth, "profile");

        String principalName = auth.getPrincipal().getName();
        String authorizableId = auth.getID();

        String value = "path".equals(valueType) ? auth.getPath() : "principalname".equals(valueType) ? principalName : authorizableId;
        String name = AuthorizableUtil.getFormattedName(resolver, auth, nameDisplayOrder);
        String defaultImage = auth.isGroup() ? defaultGroupImageUrl : defaultUserImageUrl;
        String imageUrl = (up == null || auth.isGroup()) ? defaultImage : up.getResourcePath(UserProperties.PHOTOS + "/primary/image", null, defaultImage);
        String markDisplayText = "principalname".equals(valueType) ? principalName : authorizableId;

        AttrBuilder attrs = new AttrBuilder(request, xssAPI);
        attrs.add("type", "button");
    attrs.add("test","test");
        attrs.add("is", "coral-buttonlist-item");
        attrs.add("value", value);
        attrs.add("foundation-picker-buttonlist-text", name);
        attrs.add("role", "option");

        String escapedImagePath = xssAPI.getValidHref(request.getContextPath() + imageUrl);

        attrs.addOther(FOUNDATION_PICKER_BUTTONLIST_PREFIX + "authorizableid", authorizableId);
        attrs.addOther(FOUNDATION_PICKER_BUTTONLIST_PREFIX + "principalname", principalName);
        attrs.addOther(FOUNDATION_PICKER_BUTTONLIST_PREFIX + "image", escapedImagePath);
        attrs.addOther(FOUNDATION_PICKER_BUTTONLIST_PREFIX + "group", Boolean.toString(auth.isGroup()));

        %><button <%= attrs %>>
            <div class="foundation-layout-flexmedia abc foundation-layout-flexmedia-middle">
                <img class="foundation-layout-flexmedia-img" width="32" height="32" src="<%= escapedImagePath %>" alt="">
                <div class="foundation-layout-flexmedia-bd">
                    <div class="foundation-layout-flexmedia-bd-singleline"><%= mark(name, queryPattern, xssAPI) %></div><%
                    if (!name.equals(markDisplayText)) {
                        %><div class="foundation-layout-flexmedia-bd-singleline foundation-layout-util-subtletext"><%= mark(markDisplayText, queryPattern, xssAPI) %></div><%
                    }
                %></div>
            </div>
        </button><%
    }
%></coral-buttonlist><%!

private static String mark(String text, Pattern regex, XSSAPI xssAPI) {
    StringBuilder sb = new StringBuilder();

    Matcher m = regex.matcher(text);

    if (m.lookingAt()) {
        sb.append(xssAPI.encodeForHTML(text.substring(0, m.start())));

        if (m.group().length() > 0) {
            sb.append("<mark>" + xssAPI.encodeForHTML(m.group()) + "</mark>");
        }

        sb.append(xssAPI.encodeForHTML(text.substring(m.end())));
    } else {
        sb.append(xssAPI.encodeForHTML(text));
    }

    return sb.toString();
}
%>