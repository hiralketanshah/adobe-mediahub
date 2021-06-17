<%@page session="false" %><%--
  Copyright 1997-2009 Day Management AG
  Barfuesserplatz 6, 4001 Basel, Switzerland
  All Rights Reserved.

  This software is the confidential and proprietary information of
  Day Management AG, ("Confidential Information"). You shall not
  disclose such Confidential Information and shall use it only in
  accordance with the terms of the license agreement you entered into
  with Day.

  ==============================================================================

  Default redirect component.

  Sends a redirect to the location specified in "redirectTarget" if the WCM is
  disabled. Otherwise calls the super script.

  ==============================================================================

--%>
<%@page session="false" %>
<%@ page import="com.day.cq.wcm.foundation.ELEvaluator" %>
<%
%>
<%@include file="/libs/foundation/global.jsp" %>
<%

    // read the redirect target from the 'page properties'
    String location = properties.get("redirectTarget", "");
    // resolve variables in location
    location = ELEvaluator.evaluate(location, slingRequest, pageContext);

    // check for absolute path
    final int protocolIndex = location.indexOf(":/");
    final int queryIndex = location.indexOf('?');
    String redirectPath;

    if (protocolIndex > -1 && (queryIndex == -1 || queryIndex > protocolIndex)) {
        redirectPath = location;
    } else {
        redirectPath = request.getContextPath() + location + ".html";
    }

    response.sendRedirect(redirectPath);

%>