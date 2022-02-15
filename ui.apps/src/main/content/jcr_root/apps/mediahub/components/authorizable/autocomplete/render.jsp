<%
%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
          import="java.util.ArrayList,
                  java.util.List,
                  java.security.Principal,
                  javax.jcr.RepositoryException,
                  javax.jcr.Session,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.principal.PrincipalManager,
                  org.apache.jackrabbit.api.JackrabbitSession,
                  org.apache.jackrabbit.util.Text,
                  org.apache.sling.api.resource.ResourceResolver,
                  com.adobe.granite.security.user.util.AuthorizableUtil,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ExpressionHelper,
                  com.adobe.granite.ui.components.Field,
                  com.adobe.granite.ui.components.Tag" %><%--###
Autocomplete
============

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/authorizable/autocomplete
   :supertype: /libs/granite/ui/components/coral/foundation/form/field

   An autocomplete field that is designed to allow the user to pick the authorizable from suggested list.

   It extends :granite:servercomponent:`Field </libs/granite/ui/components/coral/foundation/form/field>` component.

   It has the following content structure:

   .. gnd:gnd::

      [granite:AuthorizableAutocomplete] > granite:FormField

      /**
       * The name that identifies the field when submitting the form.
       */
      - name (String)

      /**
       * A hint to the user of what can be entered in the field.
       */
      - emptyText (String) i18n

      /**
       * Indicates if the field is in disabled state.
       */
      - disabled (Boolean)

      /**
       * Indicates if the field is mandatory to be filled.
       */
      - required (Boolean)

      /**
       * The name of the validator to be applied. E.g. ``foundation.jcr.name``.
       * See :doc:`validation </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/js/validation/index>` in Granite UI.
       */
      - validation (String) multiple

      /**
       * Indicates if the user is able to select multiple selections.
       */
      - multiple (Boolean)

      /**
       * Indicates if the user must only select from the list of given options.
       * If it is not forced, the user can enter arbitrary value.
       */
      - forceSelection (Boolean)

      /**
       * ``true`` to generate the `SlingPostServlet @Delete <http://sling.apache.org/documentation/bundles/manipulating-content-the-slingpostservlet-servlets-post.html#delete>`_ hidden input based on the field name.
       */
      - deleteHint (Boolean) = true

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
      - valueType (String) = 'id' < 'id', 'path', 'principalname'

      /**
       * The selector of authorizable:
       *
       * all
       *    Show all authorizables
       * user
       *    Show only users
       * group
       *    Show only groups
       */
      - selector (String) = 'all' < 'all', 'user', 'group'

      /**
       * Filter for service user:
       *
       * off
       *    Turn off the filter to show all users
       * includeonly
       *    Include only the service users
       * exclude
       *    Exclude the service users
       */
      - serviceUserFilter (String) = 'exclude' < 'off', 'includeonly', 'exclude'

      /**
       * Filter for impersonable user:
       *
       * off
       *    Turn off the filter to show all users
       * includeonly
       *    Include only the users that can be impersonated by the current user
       * exclude
       *    Exclude the users that can be impersonated by the current user
       */
      - impersonableUserFilter (String) = 'off' < 'off', 'includeonly', 'exclude'
###--%><%

final Config cfg = cmp.getConfig();
final ValueMap vm = (ValueMap) request.getAttribute(Field.class.getName());

final String[] values = vm.get("value", new String[0]);

final String name = cfg.get("name", String.class);
final boolean multiple = cfg.get("multiple", false);
final boolean disabled = cfg.get("disabled", false);

final String valueType = cfg.get("valueType", String.class);
final String selector = cfg.get("selector", String.class);
final String serviceUserFilter = cfg.get("serviceUserFilter", "exclude");
final String impersonableUserFilter = cfg.get("impersonableUserFilter", String.class);

String suggestionSrc = "/apps/mediahub/components/authorizable/autocomplete/suggestion{.offset,limit}.html?";

final List<String> params = new ArrayList<String>();
params.add("_charset_=utf-8");

if (valueType != null) {
    params.add("valueType=" + Text.escape(valueType));
}
if (selector != null) {
    params.add("selector=" + Text.escape(selector));
}
if (serviceUserFilter != null) {
    params.add("serviceUserFilter=" + Text.escape(serviceUserFilter));
}
if (impersonableUserFilter != null) {
    params.add("impersonableUserFilter=" + Text.escape(impersonableUserFilter));
}
params.add("query={query}");
suggestionSrc += StringUtils.join(params, '&');

final String nameDisplayOrder = i18n.get("{0} {1}", "name display order: {0} is the given (first) name, {1} the family (last) name", "givenName middleName", "familyName");

final Tag tag = cmp.consumeTag();
final AttrBuilder attrs = tag.getAttrs();
cmp.populateCommonAttrs(attrs);

attrs.add("name", name);
attrs.add("placeholder", i18n.getVar(cfg.get("emptyText", String.class)));
attrs.addDisabled(disabled);
attrs.addBoolean("multiple", multiple);
attrs.addBoolean("required", cfg.get("required", false));
attrs.addBoolean("forceselection", cfg.get("forceSelection", false));

String fieldLabel = cfg.get("fieldLabel", String.class);
String fieldDesc = cfg.get("fieldDescription", String.class);
String labelledBy = null;

if (fieldLabel != null && fieldDesc != null) {
    labelledBy = vm.get("labelId", String.class) + " " + vm.get("descriptionId", String.class);
} else if (fieldLabel != null) {
    labelledBy = vm.get("labelId", String.class);
} else if (fieldDesc != null) {
    labelledBy = vm.get("descriptionId", String.class);
}

if (StringUtils.isNotBlank(labelledBy)) {
    attrs.add("labelledby", labelledBy);
}

if (multiple) {
    attrs.add("valuedisplaymode", "block");
}

attrs.add("data-foundation-validation", StringUtils.join(cfg.get("validation", new String[0]), " "));

final AttrBuilder suggestionAttrs = new AttrBuilder(request, xssAPI);
suggestionAttrs.add("foundation-autocomplete-suggestion", "");
suggestionAttrs.addClass("foundation-picker-buttonlist");
suggestionAttrs.add("data-foundation-picker-buttonlist-src", request.getContextPath() + suggestionSrc);

final AttrBuilder valueAttrs = new AttrBuilder(request, xssAPI);
valueAttrs.add("foundation-autocomplete-value", "");
valueAttrs.add("name", name);
valueAttrs.addClass("foundation-autocomplete-taglist");

%><foundation-autocomplete test<%= attrs %>>
    <coral-overlay <%= suggestionAttrs %>></coral-overlay>
    <coral-taglist <%= valueAttrs %>><%
        for (String value : values) {
            String authName = getFormattedName(value, valueType, resourceResolver, nameDisplayOrder);
            %><coral-tag value="<%= xssAPI.encodeForHTMLAttr(value) %>"><%= xssAPI.encodeForHTML(authName) %></coral-tag><%
        }
    %></coral-taglist><%

    if (!StringUtils.isBlank(name) && cfg.get("deleteHint", true)) {
        AttrBuilder deleteAttrs = new AttrBuilder(request, xssAPI);
        deleteAttrs.addClass("foundation-field-related");
        deleteAttrs.add("type", "hidden");
        deleteAttrs.add("name", name + "@Delete");
        deleteAttrs.addDisabled(disabled);

        %><input <%= deleteAttrs %>><%
    }
%></foundation-autocomplete><%!

private static String getFormattedName(String value, String valueType, ResourceResolver resolver, String nameDisplayOrder) {
    if ("id".equals(valueType)) {
        return AuthorizableUtil.getFormattedName(resolver, value, nameDisplayOrder);
    }

    Authorizable auth = null;

    if ("path".equals(valueType)) {
        Resource authResource = resolver.getResource(value);
        if (authResource == null) {
            return value;
        }
        auth = authResource.adaptTo(Authorizable.class);
    } else {
        UserManager um = resolver.adaptTo(UserManager.class);
        Session session = resolver.adaptTo(Session.class);
        if (um != null && session != null && session instanceof JackrabbitSession) {
            JackrabbitSession jSession = (JackrabbitSession)session;
            try {
                PrincipalManager pm = jSession.getPrincipalManager();
                auth = um.getAuthorizable(pm.getPrincipal(value));
            } catch (Exception e) {
                return value;
            }
        }
    }

    if (auth == null) {
        return value;
    }
    return AuthorizableUtil.getFormattedName(resolver, auth, nameDisplayOrder);
}
%>