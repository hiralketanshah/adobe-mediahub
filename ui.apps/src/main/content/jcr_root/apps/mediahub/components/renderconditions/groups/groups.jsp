<%@page session="false"
          import="com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.rendercondition.RenderCondition,
                  com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  com.adobe.aem.formsndocuments.util.FMUtils" %><%

Config cfg = cmp.getConfig();
UserManager um = resourceResolver.adaptTo(UserManager.class);
boolean isAllowed = false;

String[] groups = cfg.get("groups", String[].class);

for (String group : groups) {
    if( FMUtils.isUserPartOfGroup(request.getUserPrincipal(), um, group) ) {
        isAllowed = true;
        break;
    }
}

request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(isAllowed));
%>