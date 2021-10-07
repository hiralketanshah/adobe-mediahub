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
%><%@page import="org.apache.sling.api.resource.Resource"%><%
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/base.jsp"%><%
%><%@include file="/libs/dam/gui/coral/components/admin/contentrenderer/base/insightBase.jsp"%><%      
%>
<% if(request.getAttribute(SHOW_INSIGHT) != null && "true".equals(request.getAttribute(SHOW_INSIGHT))) {
%> 
<coral-card-overlay class="dam-Card-insightsOverlay">
    <coral-card-insight>
        <coral-card-propertylist>
            <coral-card-property icon="delegate" role="img" aria-label='<%= xssAPI.encodeForHTMLAttr(i18n.get("Impressions")) %> : <%= assetImpressionScore %>' title ='<%= xssAPI.encodeForHTMLAttr(i18n.get("Impressions")) %>'>
                <span class="score"><%= assetImpressionScore %></span>
            </coral-card-property>
        </coral-card-propertylist>
    </coral-card-insight>
</coral-card-overlay>    
<%
  }
%>
