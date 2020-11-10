<%--
  ADOBE CONFIDENTIAL

  Copyright 2015 Adobe Systems Incorporated
  All Rights Reserved.

  NOTICE:  All information contained herein is, and remains
  the property of Adobe Systems Incorporated and its suppliers,
  if any.  The intellectual and technical concepts contained
  herein are proprietary to Adobe Systems Incorporated and its
  suppliers and may be covered by U.S. and Foreign Patents,
  patents in process, and are protected by trade secret or copyright law.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe Systems Incorporated.
--%><%
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page session="false"
          import="
            com.adobe.granite.ui.components.Config,
            org.apache.sling.api.resource.Resource,
            com.day.cq.commons.inherit.HierarchyNodeInheritanceValueMap" %><%

    Config cfg = cmp.getConfig();

    Resource contentNode = resourceResolver.getResource(slingRequest.getRequestPathInfo().getSuffix()).getChild("jcr:content");

    // do not honor inheritance (change to getInherited if inheritance is needed)
    String path = new HierarchyNodeInheritanceValueMap(contentNode).get("folderMetadataSchema", String.class);

    if (path == null) {
        return;
    }

    // Get the resource using resourceResolver so that the search path is applied.
	Resource targetResource = resourceResolver.getResource(path + "/items/tabs/items/tab2" );

    if (targetResource == null) {
        return;
    }
%>
<%
	cmp.include(targetResource, cfg.get("resourceType", String.class), cmp.consumeTag());
%>