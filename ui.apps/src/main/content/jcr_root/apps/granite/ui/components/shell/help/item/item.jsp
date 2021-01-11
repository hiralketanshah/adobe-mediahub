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
          import="com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Tag" %><%--###
Item
====

.. granite:servercomponent:: /libs/granite/ui/components/shell/help/item

   Component to render a shell help menu item.

   It has the following content structure:

   .. gnd:gnd::

      [granite:ShellHelpItem] > granite:commonAttrs

      /**
       * The icon name. e.g. "globe".
       */
      - icon (String)

      /**
       * The href attribute.
       */
      - href (StringEL)

      /**
       * The href attribute. This is usually used to produce different value based on locale.
       */
      - href_i18n (StringEL) i18n

      /**
       * The body text of the element.
       */
      - text (String) i18n

###--%><%

final Config cfg = cmp.getConfig();

String href_i18n = cfg.get("href_i18n", String.class);

Tag tag = cmp.consumeTag();

AttrBuilder attrs = tag.getAttrs();
cmp.populateCommonAttrs(attrs);

String href;
if (href_i18n != null) {
    href = i18n.getVar(cmp.getExpressionHelper().getString(href_i18n));
} else {
    href = cmp.getExpressionHelper().getString(cfg.get("href", String.class));
}
attrs.addHref("href", href);

attrs.add("is", "coral-shell-help-item");
attrs.add("icon", cfg.get("icon", String.class));
attrs.add("x-cq-linkchecker", "skip");
attrs.add("target", "_blank");
%><a <%= attrs.build() %>><%= outVar(xssAPI, i18n, cfg.get("text", "")) %></a>