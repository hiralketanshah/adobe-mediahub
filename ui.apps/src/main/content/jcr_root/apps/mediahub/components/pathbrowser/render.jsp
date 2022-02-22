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
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page import="org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.util.Text,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Field,
                  com.adobe.granite.ui.components.Tag" %><%

    // @deprecated Use PathField </libs/granite/ui/components/coral/foundation/form/pathfield> instead

    Config cfg = cmp.getConfig();
    ValueMap vm = (ValueMap) request.getAttribute(Field.class.getName());

    Field field = new Field(cfg);
    boolean mixed = field.isMixed(cmp.getValue());

    String predicate = cfg.get("predicate", "hierarchyNotFile"); // 'folder', 'hierarchy', 'hierarchyNotFile' or 'nosystem'
    String rootPath = cmp.getExpressionHelper().getString(cfg.get("rootPath", "/"));
    String defaultPickerSrc = "/libs/wcm/core/content/common/pathbrowser/column.html" + Text.escapePath(rootPath) + "?predicate=" + Text.escape(predicate);

    String crumbRoot = cfg.get("crumbRoot", String.class);
    if (crumbRoot == null) {
        Resource rootResource = resourceResolver.getResource(rootPath);
        if (rootResource != null) {
            crumbRoot = rootResource.getValueMap().get("jcr:title", rootResource.getName());
        }

        if (StringUtils.isEmpty(crumbRoot)) {
            crumbRoot = i18n.get("Home");
        }
    }

    Tag tag = cmp.consumeTag();
    AttrBuilder attrs = tag.getAttrs();
    cmp.populateCommonAttrs(attrs);

    attrs.addClass("coral-PathBrowser");
    attrs.add("data-init", "pathbrowser");
    attrs.add("data-root-path", rootPath);
    attrs.add("data-option-loader", cfg.get("optionLoader", "granite.ui.pathBrowser.pages." + predicate));
    attrs.add("data-option-loader-root", cfg.get("optionLoaderRoot", String.class));
    attrs.add("data-option-value-reader", cfg.get("optionValueReader", String.class));
    attrs.add("data-option-title-reader", cfg.get("optionTitleReader", String.class));
    attrs.add("data-option-renderer", cfg.get("optionRenderer", String.class));
	attrs.add("data-autocomplete-callback", cfg.get("autocompleteCallback", String.class));
    attrs.addHref("data-picker-src", cfg.get("pickerSrc", defaultPickerSrc));
    attrs.add("data-picker-title", i18n.getVar(cfg.get("pickerTitle", String.class)));
    attrs.add("data-picker-value-key", cfg.get("pickerValueKey", String.class));
    attrs.add("data-picker-id-key", cfg.get("pickerIdKey", String.class));
    attrs.add("data-crumb-root", cfg.get("crumbRoot", crumbRoot));
    attrs.add("data-picker-multiselect", cfg.get("pickerMultiselect", false));
    attrs.add("data-root-path-valid-selection", cfg.get("rootPathValidSelection", true));

    if (cfg.get("disabled", false)) {
        attrs.add("data-disabled", true);
    }

    if (mixed) {
        attrs.addClass("foundation-field-mixed");
    }

    AttrBuilder inputAttrs = new AttrBuilder(request, xssAPI);
    inputAttrs.addClass("coral-InputGroup-input");
    inputAttrs.addClass("js-coral-pathbrowser-input");
    inputAttrs.add("type", "text");
    inputAttrs.add("name", cfg.get("name", String.class));
    inputAttrs.add("autocomplete", "off");
    inputAttrs.add("is", "coral-textfield");

	//MED-135 do not allow user to type force select
	inputAttrs.add("readonly", true);

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

    if (mixed) {
        inputAttrs.add("placeholder", i18n.get("<Mixed Entries>"));
    } else {
        inputAttrs.add("value", vm.get("value", String.class));
        inputAttrs.add("placeholder", i18n.getVar(cfg.get("emptyText", String.class)));
    }



    if (cfg.get("required", false)) {
        inputAttrs.add("aria-required", true);
    }

    String validation = StringUtils.join(cfg.get("validation", new String[0]), " ");
    inputAttrs.add("data-foundation-validation", validation);
    inputAttrs.add("data-validation", validation); // Compatibility

    AttrBuilder buttonAttrs = new AttrBuilder(request, xssAPI);
    buttonAttrs.addClass("js-coral-pathbrowser-button");
    buttonAttrs.add("type", "button");
    buttonAttrs.add("title", i18n.get("Browse"));
    buttonAttrs.add("is", "coral-button");
    buttonAttrs.add("icon", cfg.get("icon", "folderSearch"));
    buttonAttrs.add("iconsize", "S");

    if(cfg.get("hideBrowseBtn", false)){
        buttonAttrs.add("hidden", "");
    }

%><span <%= attrs.build() %>>
    <span class="coral-InputGroup coral-InputGroup--block">
        <input <%= inputAttrs.build() %>>
        <span class="coral-InputGroup-button">
            <button <%= buttonAttrs %>></button>
        </span>
    </span>
</span>