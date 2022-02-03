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
          import="org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.jackrabbit.util.Text,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.UserPropertiesService,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.ComponentHelper.Options,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Tag,
                  com.adobe.granite.ui.components.ValueMapResourceWrapper" %><%--###
PreferencesForm
===============

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/authorizable/preferencesform

   A form component that set the ``action`` to path of preferences resource (``UserPropertiesService.PREFERENCES_PATH``) of the current user.
   The value of the ``method`` attribute of this form is always ``post``.

   It has the following content structure:

   .. gnd:gnd::

      [granite:AuthorizablePreferencesForm] > granite:commonAttrs, granite:renderCondition, granite:container

      /**
       * The ``enctype`` attribute.
       */
      - enctype (String)

      /**
       * The ``target`` attribute.
       */
      - target (String)

      /**
       * Indicates if input elements can by default have their values automatically completed by the browser.
       *
       * See also `MDN documentation regarding autocomplete attribute <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form>`_.
       */
      - autocomplete (String)

      /**
       * The ``novalidate`` attribute to indicate that the form is not to be validated when submitted.
       */
      - novalidate (Boolean)

      /**
       * The layout style of the form.
       *
       * (no value)
       *    No style is applied.
       * vertical
       *    The form fields (and their labels) are laid out in vertical manner.
       * aligned
       *    The each form field is laid out side-by-side with its label.
       */
      - style (String) < 'vertical', 'aligned'

      /**
       * Put vertical margin to the root element.
       */
      - margin (Boolean)

      /**
       * Make the element maximized to fill the available space.
       */
      - maximized (Boolean)
###--%><%

    Authorizable auth = resourceResolver.adaptTo(Authorizable.class);
    UserPropertiesManager upm = resourceResolver.adaptTo(UserPropertiesManager.class);
    UserProperties preferences = upm.getUserProperties(auth, "");

    String contentPath = preferences.getResource(".").getPath() + "/" + UserPropertiesService.PREFERENCES_PATH;

    Config cfg = cmp.getConfig();

    Tag tag = cmp.consumeTag();

    AttrBuilder attrs = tag.getAttrs();
    cmp.populateCommonAttrs(attrs);

    Resource r = new ValueMapResourceWrapper(resource, "granite/ui/components/coral/foundation/form") {
        public Resource getChild(String relPath) {
            if ("granite:data".equals(relPath)) {
                // As we already do `cmp.populateCommonAttrs(attrs)`, we have to make the button ignore `granite:data` to avoid double processing.
                return null;
            }
            return super.getChild(relPath);
        }
    };

    ValueMap vm = r.getValueMap();
    vm.put("dataPath", contentPath);
    vm.put("action", Text.escapePath(contentPath));
    vm.put("method", "post");
    vm.put("enctype", cfg.get("enctype", String.class));
    vm.put("target", cfg.get("target", String.class));
    vm.put("autocomplete", cfg.get("autocomplete", String.class));
    vm.put("novalidate", cfg.get("novalidate", Boolean.class));
    vm.put("style", cfg.get("style", String.class));
    vm.put("margin", cfg.get("margin", Boolean.class));
    vm.put("maximized", cfg.get("maximized", Boolean.class));

    cmp.include(r, new Options().tag(tag));
%>