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
           import="java.util.ArrayList,
                  java.util.List,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.util.Text,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Field,
                  com.adobe.granite.ui.components.Tag" %><%--###
UserPicker
==========

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/form/userpicker
   :supertype: /libs/granite/ui/components/coral/foundation/form/field
   :deprecated:

   .. warning:: Deprecated; please use :granite:servercomponent:`/libs/granite/ui/components/coral/foundation/authorizable/autocomplete`.

   An autocomplete component that is designed to allow the user to pick the user from suggested list.

   It extends :granite:servercomponent:`Field </libs/granite/ui/components/coral/foundation/form/field>` component.

   It has the following content structure:

   .. gnd:gnd::

      [granite:FormUserPicker] > granite:FormField

      /**
       * The name that identifies the field when submitting the form.
       */
      - name (String)

      /**
       * The value of the field.
       */
      - value (String)

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
       * ``true`` to allow selection of multiple authorizables; ``false`` otherwise.
       */
      - multiple (Boolean)

      /**
       * The name of the validator to be applied. E.g. ``foundation.jcr.name``.
       * See :doc:`validation </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/js/validation/index>` in Granite UI.
       */
      - validation (String) multiple

      /**
       * The `URI Template <http://tools.ietf.org/html/rfc6570>`_ that is giving the response of HTML of the suggestion.
       * The format of the HTML is a stream of ``li`` elements. For example:
       *
       * .. code-block:: html
       *
       *    <li class="coral-SelectList-item coral-SelectList-item--option" data-value="itemvalue1">Item Display 1</li>
       *    <li class="coral-SelectList-item coral-SelectList-item--option" data-value="itemvalue2">Item Display 2</li>
       *
       * The URI Template supports ``start``, ``end``, ``query`` variables.
       *
       * start
       *    The start index of the paging of selection items to be returned.
       *
       * end
       *    The end index of the paging of selection items to be returned.
       *
       * query
       *    The text that the user queries for.
       *
       * Example::
       *
       *    /path.html?start={start}&end={end}&query={query}&my-other-param=param1
       *
       * If this property is not set, the default value is used that is giving the response of the users in Granite.
       */
      - src (String)

      /**
       * ``true`` to display the users suggestion that are allowed to be impersonated by current user; ``false`` otherwise.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - impersonatesOnly (Boolean)

      /**
       * ``true`` to display only groups suggestions; ``false`` to display only users suggestions.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - groupsOnly (Boolean)

      /**
       * ``true`` to display both users and groups; ``false`` otherwise.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - showAllAuthorizables (Boolean)

      /**
       * ``true`` to show service users only from the suggestion; ``false`` otherwise.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - serviceUsersOnly (Boolean)

      /**
       * ``true`` to hide the service users from the suggestion; ``false`` otherwise.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - hideServiceUsers (Boolean)

      /**
       * ``true`` to use the authorizable home as value for the automcomplete items; ``false`` to use the authorizable IDs as value for the
       * autocomplete items.
       * This property is only applicable when the ``src`` property is not specified.
       */
      - useHomeAsValue (Boolean)
###--%><%

    Config cfg = cmp.getConfig();

    ValueMap vm = (ValueMap) request.getAttribute(Field.class.getName());
    String value = vm.get("value", String.class);

    boolean disabled = cfg.get("disabled", false);

    String src = cmp.getExpressionHelper().getString(cfg.get("src", String.class));

    if (src == null) {
        src =  Text.escapePath(resource.getPath()) + ".userlist.html?_charset_=utf-8{&query,start,end}";

        List<String> requestParameters = new ArrayList<String>();
        if (cfg.get("impersonatesOnly", false)){
            requestParameters.add("impersonatesOnly=true");
        }
        if (cfg.get("groupsOnly", false)){
            requestParameters.add("groupsOnly=true");
        }
        if (cfg.get("serviceUsersOnly", false)){
            requestParameters.add("serviceUsersOnly=true");
        }
        if (cfg.get("hideServiceUsers", false)){
            requestParameters.add("hideServiceUsers=true");
        }
        if (cfg.get("useHomeAsValue", false)){
            requestParameters.add("useHomeAsValue=true");
        }
        if (cfg.get("showAllAuthorizables", false)){
            requestParameters.add("showAllAuthorizables=true");
        }

        if (!requestParameters.isEmpty()) {
            src += "&" + StringUtils.join(requestParameters, '&');
        }
    }

    if (src.startsWith("/")) {
        src = request.getContextPath() + src + "?_charset_=utf-8{&query}";
    }

    Tag tag = cmp.consumeTag();

    AttrBuilder attrs = tag.getAttrs();
    cmp.populateCommonAttrs(attrs);

    attrs.add("data-forceselection", "true");

    attrs.addClass("granite-autocomplete coral-Autocomplete");
    // WARNING do not initialize the autocomplete using data-init, rather .granite-autocomplete will initialize it
    // attrs.add("data-init", "autocomplete");

    if (cfg.get("required", false)) {
        attrs.add("aria-required", true);
    }
    if (cfg.get("multiple", false)) {
        attrs.add("data-multiple", "true");
    }

    String validation = StringUtils.join(cfg.get("validation", new String[0]), " ");
    attrs.add("data-foundation-validation", validation);
    attrs.add("data-validation", validation); // Compatibility

    AttrBuilder inputAttrs = new AttrBuilder(request, xssAPI);
    inputAttrs.addClass("coral-InputGroup-input js-coral-Autocomplete-textfield");
    inputAttrs.add("type", "text");
    inputAttrs.add("name", cfg.get("name", String.class));
    inputAttrs.add("value", value);
    inputAttrs.add("placeholder", i18n.getVar(cfg.get("emptyText", String.class)));
    inputAttrs.addDisabled(disabled);
    inputAttrs.add("is", "coral-textfield");

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
        inputAttrs.add("aria-labelledby", labelledBy);
    }

    AttrBuilder buttonAttrs = new AttrBuilder(request, xssAPI);
    buttonAttrs.addClass("js-coral-Autocomplete-toggleButton");
    buttonAttrs.add("type", "button");
    buttonAttrs.add("title", i18n.get("Toggle"));
    buttonAttrs.addDisabled(disabled);
    buttonAttrs.add("is", "coral-button");
    buttonAttrs.add("icon", "chevronDown");
    buttonAttrs.add("iconSize", "XS");

    AttrBuilder selectListAttrs = new AttrBuilder(request, xssAPI);
    selectListAttrs.addClass("coral-SelectList js-coral-Autocomplete-selectList");
    selectListAttrs.add("data-type", "dynamic");
    selectListAttrs.add("data-granite-autocomplete-src", src);
    selectListAttrs.add("data-datapagesize", 25);

%><div <%= attrs.build() %>>
    <div class="coral-InputGroup coral-InputGroup--block js-coral-Autocomplete-inputGroup">
        <input <%= inputAttrs.build() %>>
        <span class="coral-InputGroup-button">
            <button <%= buttonAttrs %>></button>
        </span>
    </div>
    <ul <%= selectListAttrs.build() %>></ul>
</div>