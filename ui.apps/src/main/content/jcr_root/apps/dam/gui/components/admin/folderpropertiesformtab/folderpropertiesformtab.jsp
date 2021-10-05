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
			org.apache.commons.lang3.StringUtils,
            com.day.cq.commons.inherit.HierarchyNodeInheritanceValueMap" %><%

    Config cfg = cmp.getConfig();

    String suffix = slingRequest.getRequestPathInfo().getSuffix();

    if(StringUtils.contains(suffix, ",/")){
        suffix = suffix.split(",/")[0];
    }

    Resource contentNode = resourceResolver.getResource(suffix).getChild("jcr:content");

    // do not honor inheritance (change to getInherited if inheritance is needed)
    String path = new HierarchyNodeInheritanceValueMap(contentNode).get("folderMetadataSchema", String.class);

    if (path == null) {
       path = "/apps/dam/gui/content/assets/v2/foldersharewizard/jcr:content/content/items/form/items/wizard/items/settingStep/items/fixedColumns/items/fixedColumn2/items";
    } else{
       path = StringUtils.replace(path,"/conf/global/settings/dam/adminui-extension/foldermetadataschema" ,"/apps/dam/temp");
    }

    // Get the resource using resourceResolver so that the search path is applied.
	  Resource targetResource = resourceResolver.getResource(path + "/tabs" );

    if (targetResource == null) {
        out.println("Folder Metadata Not Available");
        return;
    }
%>
<%
    cmp.include(targetResource, cfg.get("resourceType", String.class), cmp.consumeTag());
%>