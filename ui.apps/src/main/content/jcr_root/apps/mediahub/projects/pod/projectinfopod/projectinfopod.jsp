<%--
  ADOBE CONFIDENTIAL

  Copyright 2013 Adobe Systems Incorporated
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
%><%@page session="false"
            import ="java.net.URLEncoder" %><%
%><%@include file="/libs/granite/ui/global.jsp"%><%
    String suffix = slingRequest.getRequestPathInfo().getSuffix();
	ValueMap cardMap = resource.adaptTo(ValueMap.class);
    int cardWeight = cardMap.get("cardWeight", 0);
    String pagePropertiespath = cardMap.get("pagePropertiesPath", "/libs/cq/core/content/projects/properties.html");

//String url = "/mnt/overlay/cq/core/content/projects/properties.html" + "?item=" + encodeURIComponent(suffix);

    String url = pagePropertiespath + "?item=" + encodeURIComponent(suffix);
    String projectInfoLink = xssAPI.getValidHref(url);

    request.setAttribute("projectLinkResource", resource);
    request.setAttribute("projectInfoLink", projectInfoLink);
%>
<sling:include path="/mnt/overlay/cq/core/content/projects/dashboard/default/info/jcr:content"/><%
    request.setAttribute("projectLinkResource", null);
%>

<%!
    private String encodeURIComponent(String s) {
        try {
            return URLEncoder.encode(s, "UTF-8");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
%>