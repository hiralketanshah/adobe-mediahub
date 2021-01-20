<%--
  ADOBE CONFIDENTIAL
  ___________________
  Copyright 2020 Adobe
  All Rights Reserved.
  NOTICE: All information contained herein is, and remains
  the property of Adobe and its suppliers, if any. The intellectual
  and technical concepts contained herein are proprietary to Adobe
  and its suppliers and are protected by all applicable intellectual
  property laws, including trade secret and copyright laws.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe.
--%>
<%@ include file="/libs/granite/ui/global.jsp" %>
<%
%>
<%@ page session="false"
         import="java.net.URLEncoder,
                 java.util.Iterator,
                 com.adobe.granite.ui.components.AttrBuilder,
                 com.adobe.cq.inbox.api.preferences.ViewConfigurationProvider,
                 com.adobe.cq.inbox.api.preferences.domain.view.ViewConfiguration,
                 com.adobe.granite.ui.components.Config,
                 org.apache.commons.lang3.StringUtils,
                 org.apache.sling.api.resource.ResourceResolver,
                 org.apache.jackrabbit.api.security.user.UserManager,
                 org.apache.jackrabbit.api.JackrabbitSession,
                 javax.jcr.Session,
                 org.apache.jackrabbit.api.security.user.Authorizable,
                 org.apache.jackrabbit.api.security.user.Group,
                 org.slf4j.Logger,
                 java.net.URL,
                 java.util.Set,
                 java.util.HashSet,
                 javax.jcr.RepositoryException,
                 org.apache.sling.settings.SlingSettingsService,
                 com.adobe.granite.ui.components.ExpressionHelper" %>
<%@ page import="java.util.Arrays" %>
<%

    Config cfg = cmp.getConfig();
    ExpressionHelper ex = cmp.getExpressionHelper();

    String consoleId = StringUtils.trimToNull(ex.getString(cfg.get("consoleId", String.class)));
    String navigationId = consoleId;
    boolean isNavigationDisabled = false;
    boolean isWorkflowAdmin = false;

    final String SOLUTIONS = "solutions";
    final String HELP = "help";
    final String ACTION_RESOURCE_PATH = "/mnt/overlay/granite/ui/content/shell/header/actions";
    final String CUSTOMHELP_RESOURCE_PATH = "/mnt/overlay/cq/inbox/content/inbox/header/customhelp";

    isWorkflowAdmin = isWorkflowAdministrator(resourceResolver, log);

    ViewConfigurationProvider viewConfigurationProvider = sling.getService(ViewConfigurationProvider.class);


    String runmode = " ";
    Set<String> runmodes = sling.getService(SlingSettingsService.class).getRunModes();
    if(runmodes.contains("dev")){
      runmode = " dev";
    } else if(runmodes.contains("stage")){
      runmode = " stage";
    }

    ViewConfiguration viewConfiguration = viewConfigurationProvider.getViewConfiguration(resourceResolver);
    URL customHelpUrl = viewConfiguration.getCustomHelpUrl();
    if (viewConfiguration != null) {
        isNavigationDisabled = viewConfiguration.isEndUserConfigurationEnabled();
    }

    String navigationUrl = "/mnt/overlay/granite/ui/content/shell/globalnav.html";
    if (navigationId != null) {
        navigationUrl += "?consoleId=" + URLEncoder.encode(navigationId, "utf-8");
    }

    AttrBuilder headerHome = new AttrBuilder(request, xssAPI);
    headerHome.addClass("inbox-shell-header");
    headerHome.add("data-is-navigation-disabled", isNavigationDisabled);
    headerHome.add("data-is-workflow-admin", isWorkflowAdmin);
    if (customHelpUrl != null) {
        headerHome.add("data-custom-help-url", customHelpUrl.toString());
    }
    if (!isNavigationDisabled) {
        headerHome.addClass("globalnav-toggle");
        headerHome.addHref("data-globalnav-toggle-href", navigationUrl);
    }

%>
<coral-shell-header-home <%= headerHome %>>

    <a href="#" style="text-decoration: none;">
		<img src="/content/dam/medialibrary/group_functions/company_engagement/group_communications/user_experience/vst/dms/mediahub/logo-bnp.svg" height="34" width="165" alt="BNP Paribas">
	 </a>

    <coral-shell-homeanchor-label class="bnp-text-style"> | MediaHub MVP<%= runmode %></coral-shell-homeanchor-label>
</coral-shell-header-home>

<coral-shell-header-actions>
    <coral-shell-menubar><%
        final Resource headerActions = resourceResolver.getResource(ACTION_RESOURCE_PATH);
        if (headerActions != null) {
            for (Iterator<Resource> it = headerActions.listChildren(); it.hasNext(); ) {
                Resource actionResource = it.next();
                if (customHelpUrl != null && actionResource.getName().equals(HELP)) {
                    Resource customHelpResource = resourceResolver.getResource(CUSTOMHELP_RESOURCE_PATH);
                    %><sling:include resource="<%= customHelpResource %>"/><%
                }
                else if (isNavigationDisabled) {
                    if (!actionResource.getName().equals(SOLUTIONS) && !actionResource.getName().equals(HELP)) {
                        %><sling:include resource="<%= actionResource %>"/><%
                    }
                } else {
                    %><sling:include resource="<%= actionResource %>"/><%
                }
            }
        }
    %></coral-shell-menubar>
</coral-shell-header-actions>

<coral-shell-header-content>
    <coral-shell-menubar>
      <coral-shell-menubar-item class="bnpmedialibrary">
        <a href="/assets.html/content/dam/medialibrary">
          <coral-shell-homeanchor-label class="bnp-text-style bnpmedialibrary"> Medialibrary </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item class="bnpprojects">
        <a href="/projects.html/content/projects">
          <coral-shell-homeanchor-label class="bnp-text-style bnpprojects"> Projects </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item>
        <a href="#">
          <coral-shell-homeanchor-label class="bnp-text-style"> Photostock </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item>
        <a href="/mnt/overlay/dam/gui/content/stock/search.html">
          <coral-shell-homeanchor-label class="bnp-text-style"> Adobe Stock </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item class="bnpcollections">
        <a href="/mnt/overlay/dam/gui/content/collections.html/content/dam/collections">
          <coral-shell-homeanchor-label class="bnp-text-style bnpcollections"> Collections </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
    </coral-shell-menubar>
</coral-shell-header-content>

<%!
    private boolean isWorkflowAdministrator(ResourceResolver resourceResolver, Logger log) {

        Set<String> adminUsers = new HashSet();
        adminUsers.addAll(Arrays.asList("admin", "administrators", "workflow-administrators"));

        if (adminUsers.contains(resourceResolver.getUserID())) {
            return true;
        }

        Session session = resourceResolver.adaptTo(Session.class);
        try {
            UserManager userManager = ((JackrabbitSession) session).getUserManager();
            Authorizable thisUser = userManager.getAuthorizable(session.getUserID());
            for (String adminUserId : adminUsers) {
                try {
                    Authorizable gr = userManager.getAuthorizable(adminUserId);
                    if (gr == null) {
                        continue;
                    }
                    if (gr.isGroup() && ((Group) gr).isMember(thisUser)) {
                        return true;
                    }
                } catch (RepositoryException e) {
                    continue;
                }
            }
        } catch (Exception e) {
            log.warn("Unable to get group membership of the user" + e);
            return false;
        }
        return false;
    }
%>