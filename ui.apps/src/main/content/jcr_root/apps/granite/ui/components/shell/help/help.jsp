<%--
  ADOBE CONFIDENTIAL
  ___________________

  Copyright 2016 Adobe
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
          import="java.util.Iterator,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Tag" %><%--###
Help
====

.. granite:servercomponent:: /libs/granite/ui/components/shell/help

   Component to render the help menu in the shell.

   It implements :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/toggleable` vocabulary.

   It has the following content structure:

   .. gnd:gnd::

      [granite:ShellHelp] > granite:commonAttrs

   Example::

      + myhelp
        - sling:resourceType = "granite/ui/components/shell/help"
        + items
          + home
            - sling:resourceType = "granite/ui/components/shell/help/item"
            - href_i18n = "https://www.adobe.com/go/aem6_5_docs"
            - icon = "globe"
            - text = "Help Home"
          + community
            - sling:reisourceType = "granite/ui/components/shell/help/item"
            - href_i18n = "https://www.adobe.com/go/aem6_5_community_en"
            - icon = "users"
            - text = "Community"
###--%><%

Tag tag = cmp.consumeTag();

AttrBuilder attrs = tag.getAttrs();

cmp.populateCommonAttrs(attrs);

attrs.addClass("foundation-toggleable");

attrs.add("placement", "right");
attrs.add("from", "top");
attrs.add("top", "");

%><coral-shell-menu <%= attrs %>><%

    AttrBuilder helpAttrs = new AttrBuilder(request, xssAPI);
    helpAttrs.addClass("granite-shell-help");

    String helpEndpoint = i18n.get("https://sp1004ced0.guided.ss-omtrdc.net/?tmpl=help{&q}", "API Endpoint URL to search for a term.");
    String helpSite = i18n.get("https://experiencecloud.adobe.com/resources/help/en_US/home/{?q}", "Help Site URL to show the all results of the search.");

    helpAttrs.addOther("granite-shell-help-endpoint", helpEndpoint);
    helpAttrs.addOther("granite-shell-help-site", helpSite);

    %><coral-shell-help <%= helpAttrs %>><%
        for (Iterator<Resource> items = cmp.getItemDataSource().iterator(); items.hasNext();) {
        	Resource item = items.next();
        	if (cmp.getRenderCondition(item, true).check()) {
            	%><sling:include resource="<%= item %>" /><%
            }
        }
    %></coral-shell-help>
</coral-shell-menu>