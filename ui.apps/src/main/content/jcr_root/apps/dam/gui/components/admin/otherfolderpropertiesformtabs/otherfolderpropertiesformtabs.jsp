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
	Resource targetResource4 = resourceResolver.getResource(path + "/items/tabs/items/tab4" );
	Resource targetResource5 = resourceResolver.getResource(path + "/items/tabs/items/tab5" );
	Resource targetResource6 = resourceResolver.getResource(path + "/items/tabs/items/tab6" );
	Resource targetResource7 = resourceResolver.getResource(path + "/items/tabs/items/tab7" );



    if (targetResource4 == null) {
        return;
    }
%>
<%
    if (targetResource4 != null) {
    cmp.include(targetResource4, cfg.get("resourceType", String.class), cmp.consumeTag());
	}
	if (targetResource5 != null) {
    cmp.include(targetResource5, cfg.get("resourceType", String.class), cmp.consumeTag());
	}
	if (targetResource6 != null) {
    cmp.include(targetResource6, cfg.get("resourceType", String.class), cmp.consumeTag());
	}
	if (targetResource7 != null) {
    cmp.include(targetResource7, cfg.get("resourceType", String.class), cmp.consumeTag());
	}
%>