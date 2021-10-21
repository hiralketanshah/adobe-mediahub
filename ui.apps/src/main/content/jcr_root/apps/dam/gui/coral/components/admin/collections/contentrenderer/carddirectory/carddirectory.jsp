<%--
  ADOBE CONFIDENTIAL

  Copyright 2016 Adobe Systems Incorporated
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
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@page session="false"
          import="com.adobe.granite.ui.components.AttrBuilder,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ResourceWrapper,
                  com.adobe.granite.ui.components.Tag"%>
<%

    String navigationHref = request.getContextPath() + "/assets.html" + resource.getPath();
    Tag tag = cmp.consumeTag();
    AttrBuilder itemAttrs = tag.getAttrs();
    itemAttrs.add("data-foundation-collection-navigator-href", xssAPI.encodeForHTMLAttr(navigationHref));

    Resource item = new ResourceWrapper((Resource) resource) {
        public String getResourceType() {
            return "/apps/dam/gui/coral/components/admin/contentrenderer/card/directory";
        }
    };

    cmp.include(item, new Tag(itemAttrs));
%>
