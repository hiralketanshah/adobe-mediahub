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

%><coral-shell-header-home <%= headerHome %>> 
	<a href="#">
		<img src="https://cdn-group.bnpparibas.com/bundles/app/img/logo-bnp.svg" width="165" height="34" alt="BNP Paribas">
	</a>
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