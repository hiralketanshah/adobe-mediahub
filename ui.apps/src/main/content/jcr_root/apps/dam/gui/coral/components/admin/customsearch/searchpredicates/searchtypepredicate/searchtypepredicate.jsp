<%--

  ADOBE CONFIDENTIAL
  __________________

   Copyright 2017 Adobe Systems Incorporated
   All Rights Reserved.

  NOTICE:  All information contained herein is, and remains
  the property of Adobe Systems Incorporated and its suppliers,
  if any.  The intellectual and technical concepts contained
  herein are proprietary to Adobe Systems Incorporated and its
  suppliers and are protected by trade secret or copyright law.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe Systems Incorporated.

--%><%
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false" contentType="text/html" pageEncoding="utf-8"
         import="com.adobe.granite.ui.components.Config" %><%
%><%
    Config cfg = new Config(resource);
    String fieldLabel = cfg.get("fieldLabel", String.class);

%><ui:includeClientLib categories="dam.admin.searchpanel.searchtypepredicate" />
<coral-select class="cq-dam-searchContextDropdown" label="<%= xssAPI.encodeForHTML(i18n.get(fieldLabel)) %>">
    <coral-select-item value="files" selected><%= xssAPI.encodeForHTML(i18n.get("Assets")) %></coral-select-item>
    <coral-select-item value="folders"><%= xssAPI.encodeForHTML(i18n.get("Media")) %></coral-select-item>
</coral-select>

