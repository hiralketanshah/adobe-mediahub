	<%--
  ADOBE CONFIDENTIAL
  ___________________

  Copyright 2015 Adobe
  All Rights Reserved.

  NOTICE: All information contained herein is, and remains
  the property of Adobe and its suppliers, if any. The intellectual
  and technical concepts contained herein are proprietary to Adobe
  and its suppliers and are protected by all applicable intellectual
  property laws, including trade secret and copyright laws.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe.
--%><%
%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
          import="java.util.Arrays,
                  java.util.Collection,
                  java.util.Iterator,
                  java.util.Map,
                  java.util.stream.Collectors,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.sling.commons.json.JSONObject,
                  org.apache.sling.commons.json.io.JSONStringer,
                  org.apache.sling.resourcemerger.api.ResourceMergerService,
                  com.adobe.granite.i18n.LocaleUtil,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.UserPropertiesService,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ExpressionResolver,
                  com.adobe.granite.ui.components.FilteringResourceWrapper" %><%--###
Page
====

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/page

   The generic page component to create any HTML page.

   It reads the current request locale and set ``lang`` attribute of ``html`` element. Also the dictionary for i18n is configured.

   It also supports :doc:`resource filtering using FilteringResourceWrapper </jcr_root/libs/granite/ui/docs/server/resourcehiding>` and acts as its container.

   It has the following content structure:

   .. gnd:gnd::

      [granite:Page]

      /**
       * A general purpose ID to uniquely identify the console.
       *
       * The recommended value is hierarchical separated by "-".
       * e.g. "cq-commerce-report"
       */
      - consoleId (String)

      /**
       * To render the title of the page, resource at ``head/title`` is first inspected. If it doesn't exist, this property is used accordingly; otherwise do nothing—i.e. the title at ``head/title`` is included naturally.</p>
       */
      - jcr:title (String)

      /**
       * To redirect the page, this resource can be specified. It will be included, where the redirect can be performed.
       */
      + redirector
      /**
       * Indicates the head of the page. The child resources are iterated and included as is.
       */
      + head
      /**
       * Indicates the body of the page, which typically a :granite:servercomponent:`Body </libs/granite/ui/components/coral/foundation/page/body>`.
       */
      + body

Components:

.. toctree::
   :glob:

   */index
###--%><%

Config cfg = cmp.getConfig();

if (!cfg.get("noMerge", false)) {
    ResourceMergerService resourceMerger = sling.getService(ResourceMergerService.class);
    if (resourceMerger != null) {
        Resource uiResource = resourceMerger.getMergedResource(resource);
        if (uiResource != null) {
            resource = uiResource;
            cfg = new Config(resource);
        }
    }
}

resource = new FilteringResourceWrapper(resource, sling.getService(ExpressionResolver.class), slingRequest);

Resource redirector = resource.getChild("redirector");
if (redirector != null) {
    %><sling:include resource="<%= redirector %>" /><%

    if (response.isCommitted()) {
        return;
    }
}

// GRANITE-8258: Force the header to bypass the compatibility mode on intranet sites
response.setHeader("X-UA-Compatible", "IE=edge");

%><!DOCTYPE html>
<html class="foundation-layout-util-maximized-alt skipCoral2Validation" lang="<%= xssAPI.encodeForHTMLAttr(LocaleUtil.toRFC4646(request.getLocale()).toLowerCase()) %>" data-i18n-dictionary-src="<%= request.getContextPath() %>/libs/cq/i18n/dict.{+locale}.json">
<head>
    <meta charset="utf-8">
    <link rel="shortcut icon" href="/apps/granite/core/content/login/favicon.ico">
    <%
        String title = cfg.get("jcr:title", String.class);
        Resource titleRes = resource.getChild("head/title");
        if (titleRes == null) {
            if (title != null) {
                %><title><%= outVar(xssAPI, i18n, title) %></title><%
            }
        }

        String consoleId = cfg.get("consoleId", String.class);
        String[] pageHierarchy = getPageHierarchy(consoleId);

        if (pageHierarchy == null) {
            pageHierarchy = getPageHierarchyByTitle(!StringUtils.isBlank(title) ? title : resource.getPath());
        }

        String assetId = StringUtils.isNotEmpty(request.getParameter("item"))
            ? request.getParameter("item")
            : slingRequest.getRequestPathInfo().getSuffix();

        JSONObject trackingPage = new JSONObject();
        trackingPage.put("hierarchy", pageHierarchy[0]);
        trackingPage.put("name", pageHierarchy[1]);
        if (StringUtils.isNotEmpty(assetId)) {
            trackingPage.put("assetId", assetId);
        }

        %><meta name="foundation.tracking.page" content="<%= xssAPI.encodeForHTMLAttr(trackingPage.toString()) %>"><%

        Resource globalHead = resourceResolver.getResource("/mnt/overlay/granite/ui/content/globalhead");
        if (globalHead != null) {
            for (Iterator<Resource> it = globalHead.listChildren(); it.hasNext();) {
                %><sling:include resource="<%= it.next() %>" /><%
            }
        }

        Authorizable auth = resourceResolver.adaptTo(Authorizable.class);
        UserPropertiesManager upm = resourceResolver.adaptTo(UserPropertiesManager.class);
        UserProperties userPreferences = upm.getUserProperties(auth, UserPropertiesService.PREFERENCES_PATH);
        String userPreferencesPath = auth.getPath() + "/" + UserPropertiesService.PREFERENCES_PATH;

        AttrBuilder userPrefAttrs = new AttrBuilder(request, xssAPI);
        userPrefAttrs.add("name", "user.preferences");
        userPrefAttrs.add("content", getPreferencesJSON(userPreferences));
        userPrefAttrs.addHref("data-foundation-preference-action", userPreferencesPath);

        %><meta <%= userPrefAttrs %>><%

        Resource head = resource.getChild("head");
        if (head != null) {
            for (Iterator<Resource> it = head.listChildren(); it.hasNext();) {
                %><sling:include resource="<%= it.next() %>" /><%
            }
        }
    %>
</head>
<%

// Flush head so that the browser can start downloading the clientlibs
response.flushBuffer();

Resource body = resource.getChild("body");
if (body != null) {
    %><sling:include resource="<%= body %>" /><%
}

Resource globalFooter = resourceResolver.getResource("/mnt/overlay/granite/ui/content/globalfooter");
if (globalFooter != null) {
    for (Iterator<Resource> it = globalFooter.listChildren(); it.hasNext();) {
        %><sling:include resource="<%= it.next() %>" /><%
    }
}

%>
</html><%!

private String getPreferencesJSON(UserProperties props) throws Exception {
    JSONStringer json = new JSONStringer();
    json.object();

    if (props != null) {
        for (String name : props.getPropertyNames()) {
            Object v = props.getProperty(name, null, Object.class); // No conversion, use Object.class
            if (v != null) {
                json.key(name).value(v);
            }
        }
    }

    json.endObject();
    return json.toString();
}

/**
 * Return the hierarchy and name of the current page based on consoleId.
 *
 * If the first part of the consoleId is "cq" or "granite", the part is ignored.
 *
 * Example
 *
 * ----------------------------------------------
 * consoleId    | result
 * ----------------------------------------------
 * cq-sites     | hierarchy = sites, name = sites
 * cq-sites-abc | hierarchy = sites, name = abc
 * ----------------------------------------------
 */
private String[] getPageHierarchy(String consoleId) {
    if (StringUtils.isBlank(consoleId)) {
        return null;
    }

    String[] parts = consoleId.split("-");

    int startIndex = 0;

    String namespace = parts[0];
    if (namespace.equals("cq") || namespace.equals("granite")) {
        startIndex = 1;
    }

    if (startIndex >= parts.length) {
        return null;
    }

    String name = parts[parts.length - 1];

    String hierarchy;
    if (startIndex == parts.length - 1) {
        hierarchy = name;
    } else {
        hierarchy = StringUtils.join(parts, ':', startIndex, parts.length - 1);
    }

    return new String[] { hierarchy, name };
}

/**
 * Returns the hierarchy and name of the current page based on the title.
 */
private String[] getPageHierarchyByTitle(String title) {
    String[] parts = title.split("\\|");

    String namespace = parts[0];
    if (namespace.startsWith("AEM")) {
        parts[0] = namespace.replaceFirst("AEM", "");
    }

    String name = parts[parts.length - 1].trim().toLowerCase();

    String hierarchy;
    if (parts.length == 1) {
        hierarchy = name;
    } else {
    	hierarchy = Arrays.stream(parts)
            .limit(parts.length - 1)
            .map(String::trim)
            .map(String::toLowerCase)
            .collect(Collectors.joining(":"));
    }

    return new String[] { hierarchy, name };
}
%>
