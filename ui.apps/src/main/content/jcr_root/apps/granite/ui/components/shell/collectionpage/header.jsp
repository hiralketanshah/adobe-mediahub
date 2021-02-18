<%--
  ADOBE CONFIDENTIAL
  ___________________
  Copyright 2019 Adobe
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
           import="java.net.URLEncoder,
			     java.util.Iterator,
			     com.adobe.granite.ui.components.AttrBuilder,
			     com.adobe.granite.ui.components.Config,
				   org.apache.commons.lang3.StringUtils,
				   java.util.Set,
				   org.apache.sling.settings.SlingSettingsService,
				   com.adobe.granite.ui.components.ExpressionHelper" %><%

    Config cfg = cmp.getConfig();
    ExpressionHelper ex = cmp.getExpressionHelper();

    String consoleId = StringUtils.trimToNull(ex.getString(cfg.get("consoleId", String.class)));
    String navigationId = consoleId;

    String navigationUrl = "/mnt/overlay/granite/ui/content/shell/globalnav.html";
    if (navigationId != null) {
        navigationUrl += "?consoleId=" + URLEncoder.encode(navigationId, "utf-8");
    }

    AttrBuilder headerHome = new AttrBuilder(request, xssAPI);
    headerHome.addClass("globalnav-toggle");
    headerHome.addHref("data-globalnav-toggle-href", navigationUrl);

    String runmode = " ";
    Set<String> runmodes = sling.getService(SlingSettingsService.class).getRunModes();
    if( runmodes.contains("dev")){
      runmode = " Dev";
    } else if(runmodes.contains("stage")){
      runmode = " Stage";
    }
%><coral-shell-header-home <%= headerHome %>> 
	<a href="#" style="text-decoration: none;">
		<img src="/content/dam/technique/mediahub/logo-bnp.svg" height="34" width="165" alt="BNP Paribas">
	</a>
	<coral-shell-homeanchor-label class="bnp-text-style"> | Mediahub MVP<%= runmode %></coral-shell-homeanchor-label>
</coral-shell-header-home>

<coral-shell-header-actions>
    <coral-shell-menubar><%
        final Resource headerActions = resourceResolver.getResource("/mnt/overlay/granite/ui/content/shell/header/actions");
        if (headerActions != null) {
            for (Iterator<Resource> it = headerActions.listChildren(); it.hasNext();) {
                %><sling:include resource="<%= it.next() %>" /><%
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