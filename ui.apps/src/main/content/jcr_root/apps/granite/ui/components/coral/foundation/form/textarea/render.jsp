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
          import="org.apache.commons.lang3.StringUtils,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Field,
                  com.adobe.granite.ui.components.Tag" %><%--###
Textarea
========

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/form/textarea
   :supertype: /libs/granite/ui/components/coral/foundation/form/field

   A textarea component.

   It extends :granite:servercomponent:`Field </libs/granite/ui/components/coral/foundation/form/field>` component.

   It has the following content structure:

   .. gnd:gnd::

      [granite:FormTextarea] > granite:FormField

      /**
       * The name that identifies the field when submitting the form.
       */
      - name (String)

      /**
       * The value of the field.
       */
      - value (StringEL)

      /**
       * A hint to the user of what can be entered in the field.
       */
      - emptyText (String) i18n

      /**
       * Indicates if the field is in disabled state.
       */
      - disabled (Boolean)

      /**
       * Indicates if the field is mandatory to be filled.
       */
      - required (Boolean)

      /**
       * Indicates if the value can be automatically completed by the browser.
       *
       * See also `MDN documentation regarding autocomplete attribute <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input>`_.
       */
      - autocomplete (String) = 'off'

      /**
       * The ``autofocus`` attribute to lets you specify that the field should have input focus when the page loads,
       * unless the user overrides it, for example by typing in a different control.
       * Only one form element in a document can have the ``autofocus`` attribute.
       */
      - autofocus (Boolean)

      /**
       * The name of the validator to be applied. E.g. ``foundation.jcr.name``.
       * See :doc:`validation </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/js/validation/index>` in Granite UI.
       */
      - validation (String) multiple

      /**
       * The maximum number of characters (in Unicode code points) that the user can enter.
       */
      - maxlength (Long)

      /**
       * The visible width of the text control, in average character widths.
       */
      - cols (Integer)

      /**
       * The number of visible text lines.
       */
      - rows (Integer) = 5

      /**
       * The resizing behaviour.
       *
       * Note that since it is implemented using CSS ``resize`` property, it may not work for some browsers.
       */
      - resize = 'none' < 'none', 'both', 'horizontal', 'vertical'
###--%><%

    Config cfg = cmp.getConfig();
    ValueMap vm = (ValueMap) request.getAttribute(Field.class.getName());
    Field field = new Field(cfg);

    boolean isMixed = field.isMixed(cmp.getValue());

    Tag tag = cmp.consumeTag();
    AttrBuilder attrs = tag.getAttrs();
    cmp.populateCommonAttrs(attrs);

    attrs.add("name", cfg.get("name", String.class));
    attrs.add("placeholder", i18n.getVar(cfg.get("emptyText", String.class)));
    attrs.addDisabled(cfg.get("disabled", false));
    attrs.add("maxlength", cfg.get("maxlength", Integer.class));
    attrs.add("cols", cfg.get("cols", Integer.class));
    attrs.add("rows", cfg.get("rows", 5));
    attrs.addClass(getResizable(cfg.get("resize", "none")));
    attrs.add("autocomplete", cfg.get("autocomplete", String.class));
    attrs.addBoolean("autofocus", cfg.get("autofocus", false));

    String fieldLabel = cfg.get("fieldLabel", String.class);
    String fieldDesc = cfg.get("fieldDescription", String.class);
    String labelledBy = null;

    if (fieldLabel != null && fieldDesc != null) {
        labelledBy = vm.get("labelId", String.class) + " " + vm.get("descriptionId", String.class);
    } else if (fieldLabel != null) {
        labelledBy = vm.get("labelId", String.class);
    } else if (fieldDesc != null) {
        labelledBy = vm.get("descriptionId", String.class);
    }

    if (StringUtils.isNotBlank(labelledBy)) {
        attrs.add("labelledby", labelledBy);
    }

    if (cfg.get("required", false)) {
        attrs.add("aria-required", true);
    }

    String validation = StringUtils.join(cfg.get("validation", new String[0]), " ");
    attrs.add("data-foundation-validation", validation);
    attrs.add("data-validation", validation); // Compatibility

    String value;

    if (isMixed) {
        attrs.addClass("foundation-field-mixed");
        attrs.add("placeholder", i18n.get("<Mixed Entries>"));
        value = "";
    } else {
        value = vm.get("value", "");
    }

    // @coral
    attrs.add("is", "coral-textarea");

%><textarea <%= attrs.build() %>><%= xssAPI.encodeForHTML(value) %></textarea><%!

    private String getResizable(String resize) {
        if ("none".equalsIgnoreCase(resize)) {
            return "foundation-layout-util-resizable-none";
        }

        if ("both".equalsIgnoreCase(resize)) {
            return "foundation-layout-util-resizable";
        }

        if ("horizontal".equalsIgnoreCase(resize)) {
            return "foundation-layout-util-resizable-x";
        }

        if ("vertical".equalsIgnoreCase(resize)) {
            return "foundation-layout-util-resizable-y";
        }

        return null;
    }
%>