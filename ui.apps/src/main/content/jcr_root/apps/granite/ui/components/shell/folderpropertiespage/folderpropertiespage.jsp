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
          import="org.slf4j.LoggerFactory,
                  org.slf4j.Logger,
                  java.util.Arrays,
                  java.util.Collection,
                  java.util.Iterator,
                  java.util.stream.Collectors,
                  java.util.Map,
                  java.net.URLDecoder,
                  java.io.UnsupportedEncodingException,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.sling.api.SlingHttpServletRequest,
                  org.apache.sling.api.request.RequestDispatcherOptions,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.commons.json.JSONObject,
                  org.apache.sling.commons.json.io.JSONStringer,
                  org.apache.sling.resourcemerger.api.ResourceMergerService,
                  org.apache.sling.scripting.jsp.util.JspSlingHttpServletResponseWrapper,
                  com.adobe.granite.i18n.LocaleUtil,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.UserPropertiesService,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ExpressionHelper,
                  com.adobe.granite.ui.components.ExpressionResolver,
                  com.adobe.granite.ui.components.FilteringResourceWrapper,
                  com.adobe.granite.ui.components.Tag,
                  com.adobe.granite.ui.components.rendercondition.RenderCondition,
                  com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition,
                  com.day.cq.dam.api.Asset,
                  com.day.cq.dam.commons.util.DamUtil,
                  com.mediahub.core.constants.BnpConstants" %><%--###
PropertiesPage
==============

.. granite:servercomponent:: /libs/granite/ui/components/shell/propertiespage

   Properties page is a page showing the properties of a resource.

   It also supports :doc:`resource filtering using FilteringResourceWrapper </jcr_root/libs/granite/ui/docs/server/resourcehiding>` and acts as its container.

   It has the following content structure:

   .. gnd:gnd::

      [granite:ShellPropertiesPage]

      /**
       * A general purpose ID to uniquely identify the console.
       *
       * The recommended value is hierarchical separated by "-".
       * e.g. "cq-commerce-report"
       */
      - consoleId (StringEL)

      /**
       * The base title of the page.
       *
       * e.g. "AEM Sites"
       */
      - jcr:title (String)

      /**
       * The URL to navigate to when closing this page, e.g., when the user presses "Close".
       */
      - backHref (StringEL)

      /**
       * The id of the form component.
       *
       * The intention of propertiespage is to standardize the property screen, including the behaviour of form success handler whether the page is reloaded or redirected or otherwise.
       * Thus, it is RECOMMENDED that the form is a :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/js/form/index` without any success handler configured.
       * propertiespage will manage the form success handler behaviour internally.
       */
      - formId (String)

      /**
       * When ``formId`` is specified, ``true`` to indicate if a simple "Save" button is rendered, ``false`` to render a more complex form actions (currently "Save & Close" and "Save").
       *
       * Note that this flag is only temporary for compatibility, since the intention is to standardize the UX as the behaviour when the value is ``false``.
       */
      - simpleSave (Boolean) = true

      /**
       * To redirect the page, this resource can be specified.
       * It will be included, where the redirect can be performed.
       */
      + redirector

      /**
       * A folder to specify the content of ``<head>`` of the page.
       * Its child resources are iterated and included as is.
       */
      + head

      /**
       * The component to render the title.
       *
       * This is optional, and if not specified, the value of ``jcr:title`` is used.
       * If the title is just a simple string, :doc:`../title/index` can be used.
       *
       * The only requirement of the component is to generate a simple text without any wrapping markup.
       * E.g. To have a title of "My Page", just make the component do something like ``out.print("My Page")``.
       */
      + title

      /**
       * The header area just above the properties view.
       * Any component can be used here.
       */
      + header

      /**
       * The :doc:`render condition </jcr_root/libs/granite/ui/docs/server/rendercondition>` component to check if the form is editable by the current session.
       *
       * By default the form is assumed to be editable, even though the form submission may fail and handled accordingly.
       */
      + editcondition

      /**
       * The component to render the properties view (the content area).
       */
      + content

      /**
       * The footer area just below the properties view.
       * Any component can be used here.
       */
      + footer

      /**
       * The folder for the actions applicable in the context of the properties page.
       *
       * The action can be any action component such as :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/button/index`,
       * :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/anchorbutton/index`,
       * :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/pulldown/index`,
       *
       * The ``actionBar`` variant of the components above SHOULD be used, unless ``primary`` variant is used.
       */
      + actions

      /**
       * A folder to specify the panels of the rail.
       *
       * Its child resources are considered as the panels, where each MUST be a :doc:`../../coral/foundation/panel/railpanel/index` (or its derivative).
       *
       * By default the rail is closed. To make it always open, ``alwaysActive`` boolean property can be specified.
       * One of the railpanel then must be configured to be active.
       */
      + rails


   Example::

      + mypage
        - sling:resourceType = "granite/ui/components/shell/propertiespage"
        - jcr:title = "My Properties"
        - formId = "myFormId"
        - backHref = '${empty header.Referer ? granite:concat("/mycollection.html", granite:encodeURIPath(granite:relativeParent(param.item, 1))) : header.Referer}'
        + content
          - sling:resourceType = "granite/ui/components/coral/foundation/form"
          - granite:id = "myFormId"
        + rails
          - alwaysActive = true
          + panel1
            - sling:resourceType = "granite/ui/components/coral/foundation/panel/railpanel"
            - jcr:title = "Panel 1"
            - active = true
          + panel2
            - sling:resourceType = "granite/ui/components/coral/foundation/panel/railpanel"
            - jcr:title = "Panel 2"
###--%><%

final Logger logger = LoggerFactory.getLogger("libs.granite.ui.components.shell.propertiespage");
logger.debug("Render starts");

Config cfg = cmp.getConfig();
ExpressionHelper ex = cmp.getExpressionHelper();

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

AttrBuilder htmlAttrs = new AttrBuilder(request, xssAPI);
htmlAttrs.addClass("skipCoral2Validation foundation-layout-util-maximized-alt");
htmlAttrs.add("lang", LocaleUtil.toRFC4646(request.getLocale()).toLowerCase());
htmlAttrs.add("data-i18n-dictionary-src", request.getContextPath() + "/libs/cq/i18n/dict.{+locale}.json");
String consoleId = StringUtils.trimToNull(ex.getString(cfg.get("consoleId", String.class)));

// Rail status
Resource rails = resource.getChild("rails");
String railSaveKey = "rail-cq-propertiespage";
String savedRailTarget = getSavedRail(slingRequest, railSaveKey);
boolean savedRailTargetFound = false;
boolean hasActiveRail = rails != null && cmp.getExpressionHelper().getBoolean(rails.getValueMap().get("active", "false"));


%><!DOCTYPE html>
<html <%= htmlAttrs %>>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="shortcut icon" href="<%= request.getContextPath() %>/libs/granite/core/content/login/favicon.ico"><%
    Resource headTitleRes = resource.getChild("head/title");
    String origTitle = cfg.get("jcr:title", String.class);
    String title = i18n.getVar(origTitle);
    if (headTitleRes == null) {
        if (title != null) {
            %><title><%= xssAPI.encodeForHTML(title) %></title><%
        }
    }

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

    %><meta <%= userPrefAttrs %>>
    <%-- <meta name="user.preferences.winmode"> is deprecated, use foundation-preference instead --%>
    <meta name="user.preferences.winmode" content="<%= xssAPI.encodeForHTMLAttr(getPreference(userPreferences, "winMode", "multi")) %>"><%

    String[] pageHierarchy = getPageHierarchy(consoleId);

    if (pageHierarchy == null) {
        pageHierarchy = getPageHierarchyByTitle(!StringUtils.isBlank(origTitle) ? origTitle : resource.getPath());
    }

    String assetId = StringUtils.isNotEmpty(request.getParameter("item"))
        ? request.getParameter("item")
        : slingRequest.getRequestPathInfo().getSuffix();

    JSONObject trackingPage = new JSONObject();
    trackingPage.put("type", "properties");
    trackingPage.put("hierarchy", pageHierarchy[0]);
    trackingPage.put("name", pageHierarchy[1]);
    if (StringUtils.isNotEmpty(assetId)) {
        trackingPage.put("assetId", assetId);
    }


    %><meta name="foundation.tracking.page" content="<%= xssAPI.encodeForHTMLAttr(trackingPage.toString()) %>"><%

    if (cfg.get("coral2", false)) {
        %><ui:includeClientLib categories="coralui2,granite.ui.coral.foundation,granite.ui.coral.foundation.addon.coral2" /><%
    } else {
        %><ui:includeClientLib categories="coralui3,granite.ui.coral.foundation" /><%
    }

    Resource head = resource.getChild("head");
    if (head != null) {
        for (Iterator<Resource> it = head.listChildren(); it.hasNext();) {
            %><sling:include resource="<%= it.next() %>" /><%
        }
    }
    logger.debug("<head> rendered");
%></head>
<%
// Flush head so that the browser can start downloading the clientlibs
response.flushBuffer();

String backHref = StringUtils.trimToNull(ex.getString(cfg.get("backHref", String.class)));
if (backHref == null) {
    backHref = "/";
}

String formId = cfg.get("formId", String.class);
boolean railAlwaysActive = false;
if (rails != null) {
    Config railsConfig = new Config(rails);
    railAlwaysActive = railsConfig.get("alwaysActive", false);
}
try {
%><body class="coral--light foundation-layout-util-maximized-alt">
<%
    Resource skipNavigationLinksRes = resource.getChild("skipnavigationlinks");
    if (skipNavigationLinksRes != null) {
        %><sling:include resource="<%= skipNavigationLinksRes %>" /><%
    }
%>
<div class="foundation-layout-panel">
    <div class="foundation-layout-panel-header">
        <coral-actionbar class="betty-ActionBar betty-ActionBar--large">
            <coral-actionbar-primary><%
                Resource actions = resource.getChild("actions");
                if (actions != null) {
                    for (Iterator<Resource> it = actions.listChildren(); it.hasNext();) {
                        Resource item = it.next();

                        if (!cmp.getRenderCondition(item, true).check()) {
                            continue;
                        }
                        %><coral-actionbar-item><%
                            AttrBuilder selectionItemAttrs = new AttrBuilder(request, xssAPI);
                            selectionItemAttrs.addClass("betty-ActionBar-item");
                            cmp.include(item, new Tag(selectionItemAttrs));
                        %></coral-actionbar-item><%
                    }
                }
            %></coral-actionbar-primary>
            <coral-actionbar-secondary><%

                AttrBuilder backAttrs = new AttrBuilder(request, xssAPI);
                backAttrs.add("id", "shell-propertiespage-closeactivator");
                backAttrs.addClass("betty-ActionBar-item");
                backAttrs.addHref("href", backHref);
                backAttrs.add("x-cq-linkchecker", "skip");
                backAttrs.add("is", "coral-anchorbutton");
                backAttrs.add("variant", "quiet");
                backAttrs.addClass("foundation-fixedanchor");

                // condition used to determine if the editing ui will be shown
                RenderCondition editCondition = getEditCondition(pageContext, slingRequest, resource);

                boolean showEdit = !StringUtils.isEmpty(formId) && editCondition.check();

				showEdit = true;

				String cancelText;
                if (showEdit) {
                    cancelText = i18n.get("Cancel");

                    backAttrs.addClass("foundation-backanchor");
                    backAttrs.add("data-foundation-backanchor-form", formId);
                } else {
                    cancelText = i18n.get("Close");
                }

                %><coral-actionbar-item>
                    <a <%= backAttrs %>><%= xssAPI.encodeForHTML(cancelText) %></a>
                </coral-actionbar-item><%

                if (showEdit) {
                    %><coral-actionbar-item><%
                        if (cfg.get("simpleSave", true)) {
                            AttrBuilder saveAttrs = new AttrBuilder(request, xssAPI);
                            saveAttrs.add("id", "shell-propertiespage-saveactivator");
                            saveAttrs.addClass("betty-ActionBar-item");
                            saveAttrs.add("type", "submit");
                            saveAttrs.add("form", formId);
                            saveAttrs.add("data-foundation-command", "ctrl+s");
                            saveAttrs.add("is", "coral-button");
                            saveAttrs.add("variant", "primary");

                            %><button <%= saveAttrs %>><%= xssAPI.encodeForHTML(i18n.get("Save")) %></button><%
                        } else {
                            String saveBtnVariant = "primary";
                            boolean isChildAssetActive = false;
                            String[] assets = assetId.split(",/");
                            for(String asset : assets){
                                if(!StringUtils.startsWith(asset, "/")){
                                  asset = "/" + asset;
                                }
                                if(null != resourceResolver.getResource(asset)){
                                  Iterator<Asset> subAssets = DamUtil.getAssets(resourceResolver.getResource(asset));
                                  while(subAssets.hasNext()){
                                      Asset subAsset = subAssets.next();
                                      if(subAsset.adaptTo(Resource.class).getChild("jcr:content") != null && subAsset.adaptTo(Resource.class).getChild("jcr:content").getChild("metadata") != null){
                                        ValueMap metadata =  subAsset.adaptTo(Resource.class).getChild("jcr:content").getChild("metadata").getValueMap();
                                        if(metadata.containsKey(BnpConstants.BNPP_INTERNAL_FILE_URL) || metadata.containsKey(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL)){
                                            isChildAssetActive = true;
                                            break;
                                        }
                                      }
                                  }
                                }

                                if(isChildAssetActive){
                                  break;
                                }
                            }


                            AttrBuilder doneAttrs = new AttrBuilder(request, xssAPI);
                            doneAttrs.add("id", "propertiespage-bulkedit-save");
                            doneAttrs.add("isChildAssetActive", isChildAssetActive);
                            doneAttrs.add("type", "submit");
                            doneAttrs.add("form", formId);
                            doneAttrs.add("is", "coral-button");
                            doneAttrs.add("variant", saveBtnVariant);
                            doneAttrs.addClass("granite-form-saveactivator");
                            doneAttrs.addHref("data-granite-form-saveactivator-href", backHref);
                            doneAttrs.addClass("foundation-fixedanchor");
                            doneAttrs.add("data-foundation-fixedanchor-attr", "data-granite-form-saveactivator-href");

                            AttrBuilder saveChevronAttrs = new AttrBuilder(request, xssAPI);
                            saveChevronAttrs.addClass("granite-ActionGroup-icon");
                            saveChevronAttrs.add("is", "coral-button");
                            saveChevronAttrs.add("icon", "chevronDown");
                            saveChevronAttrs.add("variant", saveBtnVariant);
                            saveChevronAttrs.add("iconsize", "xs");
                            saveChevronAttrs.add("aria-label", i18n.get("More actions"));
                            saveChevronAttrs.add("aria-haspopup", true);

                            AttrBuilder saveAttrs = new AttrBuilder(request, xssAPI);
                            saveAttrs.add("id", "shell-propertiespage-saveactivator");
                            saveAttrs.addClass("granite-ActionGroup-item");
                            saveAttrs.add("type", "submit");
                            saveAttrs.add("form", formId);
                            saveAttrs.add("is", "coral-buttonlist-item");
                            saveAttrs.add("data-foundation-command", "ctrl+s");
                            saveAttrs.addClass("granite-form-saveactivator");
                            saveAttrs.add("role", "menuitem");


                            AttrBuilder doneAttrs1 = new AttrBuilder(request, xssAPI);
                            doneAttrs1.add("id", "shell-propertiespage-bulksave-publish");
                            doneAttrs1.add("type", "submit");
                            doneAttrs1.add("form", formId);
                            doneAttrs1.add("is", "coral-button");
                            doneAttrs1.add("isValidated", true);
                            doneAttrs1.add("variant", saveBtnVariant);
                            doneAttrs1.addClass("granite-form-saveactivator");

                            AttrBuilder unpublishAttrs = new AttrBuilder(request, xssAPI);
                            unpublishAttrs.add("id", "shell-propertiespage-bulkmedia-unpublish");
                            unpublishAttrs.add("type", "submit");
                            unpublishAttrs.add("form", formId);
                            unpublishAttrs.add("is", "coral-button");
                            unpublishAttrs.add("variant", saveBtnVariant);
                            unpublishAttrs.addClass("granite-form-saveactivator");

                            %>

                            <coral-buttongroup class="betty-ActionBar-item granite-ActionGroup">
                                <button <%= doneAttrs %>><%= xssAPI.encodeForHTML(i18n.get("Save & Close")) %></button>
                                <button <%= saveChevronAttrs %>></button>
                                <coral-popover role="presentation" placement="center" alignMy="right top" alignAt="right bottom" target="_prev">
                                    <coral-popover-content>
                                        <coral-buttonlist role="menu" class="granite-ActionGroup-list">
                                            <button <%= saveAttrs %>><%= xssAPI.encodeForHTML(i18n.get("Save")) %></button>
                                        </coral-buttonlist>
                                    </coral-popover-content>
                                </coral-popover>
                            </coral-buttongroup>

                            <coral-buttongroup class="betty-ActionBar-item granite-ActionGroup">
                                <button <%= doneAttrs1 %> ><%= xssAPI.encodeForHTML(i18n.get("Save & Publish")) %></button>
                            </coral-buttongroup>

                            <coral-buttongroup class="betty-ActionBar-item granite-ActionGroup">
                                <button <%= unpublishAttrs %> ><%= xssAPI.encodeForHTML(i18n.get("Unpublish")) %></button>
                            </coral-buttongroup>

                            <%
                        }
                    %></coral-actionbar-item><%
                }
            %></coral-actionbar-secondary>
        </coral-actionbar>
        <betty-titlebar>
            <betty-titlebar-title><%
                Resource titleRes = resource.getChild("title");
                if (titleRes != null) {
                    %><span class="granite-title" role="heading" aria-level="1"><sling:include resource="<%= titleRes %>" /></span><%
                } else {
                    %><span class="granite-title" role="heading" aria-level="1"><%= xssAPI.encodeForHTML(title) %></span><%
                }
            %></betty-titlebar-title>
            <betty-titlebar-primary><%
                if (rails != null) {
            %><coral-cyclebutton
                class="granite-toggleable-control"
                icon="railLeft"
                displaymode="icontext"
                data-granite-toggleable-control-savekey="<%= xssAPI.encodeForHTMLAttr(railSaveKey) %>">
                <%
                            if (!railAlwaysActive) {
                                AttrBuilder itemAttrs = new AttrBuilder(request, xssAPI);
                                itemAttrs.add("data-granite-toggleable-control-target", "#shell-propertiespage-rail");
                                itemAttrs.add("data-granite-toggleable-control-action", "hide");
                                itemAttrs.add("displaymode", "icon");
                                itemAttrs.add("data-foundation-command", "`");
                                itemAttrs.add("data-granite-toggleable-control-name", "content-only");


                                String text;
                                if (formId != null) {
                                    text = i18n.get("Properties");
                                } else {
                                    text = i18n.get("Content Only");
                                }
                                %><coral-cyclebutton-item <%= itemAttrs %>><%= text %></coral-cyclebutton-item><%
                            }

                            // we map all rail items to the numbers in the keyboard using control + {index}
                            int railItemIndex = 1;
                            for (Iterator<Resource> it = rails.listChildren(); it.hasNext(); railItemIndex++) {
                                Resource item = it.next();
                                if (cmp.getRenderCondition(item, true).check()) {
                                    Config itemCfg = new Config(item);

                                    String href = ex.getString(itemCfg.get("href", String.class));

                                    AttrBuilder itemAttrs = new AttrBuilder(request, xssAPI);
                                    itemAttrs.add("icon", itemCfg.get("icon", String.class));

                                    // we stop once we reach 9 since all available numbers have been used
                                    if (railItemIndex < 10) {
                                        itemAttrs.add("data-foundation-command", "alt+" + railItemIndex);
                                    }

                                    if (href != null) {
                                        itemAttrs.add("data-granite-toggleable-control-action", "navigate");
                                        itemAttrs.addHref("data-granite-toggleable-control-href", href);
                                    } else {
                                        String railPanelTarget = ".shell-propertiespage-rail-panel[data-shell-propertiespage-rail-panel='" + item.getName() + "']";

                                        itemAttrs.add("data-granite-toggleable-control-action", "show");
                                        itemAttrs.add("data-granite-toggleable-control-target", railPanelTarget);
                                    }
                                    itemAttrs.add("data-granite-toggleable-control-name", item.getName());
                                    if (!hasActiveRail && savedRailTarget != null && savedRailTarget.equals(item.getName())) {
                                        itemAttrs.addSelected(true);
                                        savedRailTargetFound = true;
                                    }

                                    %><coral-cyclebutton-item <%= itemAttrs %>><%= outVar(xssAPI, i18n, itemCfg.get("jcr:title", String.class)) %></coral-cyclebutton-item><%
                                }
                            }
                        %></coral-cyclebutton><%
                }
            %></betty-titlebar-primary>
            <betty-titlebar-secondary></betty-titlebar-secondary>
        </betty-titlebar><%

        Resource header = resource.getChild("header");
        if (header != null) {
            %><sling:include resource="<%= header %>" /><%
        }
    %></div>
    <div class="foundation-layout-panel-bodywrapper">
        <div class="foundation-layout-panel-body"><%
            if (rails != null) {
                AttrBuilder railAttrs = new AttrBuilder(request, xssAPI);
                railAttrs.add("id", "shell-propertiespage-rail");
                railAttrs.addClass("foundation-toggleable foundation-layout-panel-rail granite-rail");

                if (railAlwaysActive || cmp.getExpressionHelper().getBoolean(rails.getValueMap().get("active", "false")) || savedRailTargetFound) {
                    railAttrs.addClass("foundation-layout-panel-rail-active");
                    railAttrs.addClass("foundation-layout-panel-rail-activate-panel");
                }

                %><div <%= railAttrs %>>
                    <coral-panelstack maximized><%
                        for (Iterator<Resource> it = rails.listChildren(); it.hasNext();) {
                            Resource item = it.next();
                            Config itemCfg = new Config(item);

                            AttrBuilder itemAttrs = new AttrBuilder(request, xssAPI);
                            itemAttrs.addClass("shell-propertiespage-rail-panel");
                            itemAttrs.add("data-shell-propertiespage-rail-panel", item.getName());
                            if (!hasActiveRail && savedRailTarget != null && savedRailTarget.equals(item.getName())) {
                                itemAttrs.addSelected(true);
                            }
                            cmp.include(item, new Tag(itemAttrs));
                        }
                    %></coral-panelstack>
                </div><%
            }

            AttrBuilder contentAttrs = new AttrBuilder(request, xssAPI);
            contentAttrs.addClass("foundation-layout-panel-content");

            // Also set `.foundation-layout-form-mode-edit` to also support field content that set `renderReadOnly=true`.
            contentAttrs.addClass("foundation-layout-form foundation-layout-form-mode-edit");

            %><div <%= contentAttrs %>>
                <sling:include resource="<%= resource.getChild("content") %>" />
            </div>
        </div>
    </div><%

    Resource footer = resource.getChild("footer");
    if (footer != null) {
        %><div class="foundation-layout-panel-footer">
            <sling:include resource="<%= footer %>" />
        </div><%
    }
%></div><%

    Resource globalFooter = resourceResolver.getResource("/mnt/overlay/granite/ui/content/globalfooter");
    if (globalFooter != null) {
        for (Iterator<Resource> it = globalFooter.listChildren(); it.hasNext();) {
            %><sling:include resource="<%= it.next() %>" /><%
        }
    }
%>
</body><%
} catch (Exception e) {
    logger.error("Unable to render properties page correctly", e);
    throw e;
}
logger.debug("Render ends");
%>

<script type="text/javascript">
    var data = {};
</script>


<%
if (StringUtils.isNotEmpty(assetId)) {

    String[] assetPaths = assetId.split(",");

    for(String assetPath : assetPaths){

%>
<script type="text/javascript">
    var media = [];
</script>
<%

    Resource asset = resourceResolver.getResource(assetPath);
    if (asset != null) {
        if (asset.getValueMap().get("jcr:primaryType") != null && StringUtils.equals(asset.getValueMap().get("jcr:primaryType").toString(), "sling:Folder")) {
            Iterator<Resource> it = asset.listChildren();
            while (it.hasNext()) {
                Resource child = it.next();
                if (!StringUtils.equals(child.getName(), "jcr:content") && !StringUtils.equals(child.getName(), "rep:policy")) {
%>
<script>
   media.push({
       name: 'payload',
       value: "<%= child.getPath() %>"
   });
</script>
<%
        }
    }
}
%>
<script type="text/javascript">
    data["<%= assetPath %>"] = media;
</script>
<%
} else {
%>
<script>
    media.push({
        name: 'payload',
        value: "<%= assetId %>"
    });
    data["<%= assetId %>"] = media;
</script>
<%
            }
        }
    }
%>


<script type="text/javascript">



  function isChildrenDeactivated() {
      var ui = $(window).adaptTo("foundation-ui");
      var failureMessage = Granite.I18n.get("Kindly Deactivate Assets inside Media Folder");
      ui.prompt(Granite.I18n.get("Kindly Deactivate Assets inside Media Folder"), failureMessage, "error", [{
          text: Granite.I18n.get("OK"),
          primary: true
      }]);
  }

  function internalPublish(isValidated, event, isFolderMetadataMissing, isMediaValidated, path) {
      var workflowData = [];
      workflowData.push({name: '_charset_', value: 'UTF-8'});
      workflowData.push({name: 'payloadType', value: 'JCR_PATH'});
      workflowData.push({name: 'model@Delete', value: ''});
      workflowData.push({name: 'workflowTitle', value: 'Internal Publish'});

      workflowData.push({name: 'model', value: '/var/workflow/models/mediahub/mediahub---validation'});
      workflowData.push({name: 'path', value: path});

      if(data[path] && data[path].length > 0){
        data[path].forEach(function(item){
          workflowData.push({name: 'payload', value: item.value});
        });
      }

      $.ajax({
          type: "POST",
          url: "/etc/workflow/instances",
          data: workflowData,
          async: true,
          cache: false,
          success: function (response) {
              if (response) {
                  var processedHtml = Granite.UI.Foundation.Utils.processHtml(response);
              }
          }
      }).done(function (html) {
          // code is removed to handle the issue of popup in save and publish
      });
  }
</script>

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

private String getPreference(UserProperties props, String name, String defaultValue) throws Exception {
    if (props != null) {
        return props.getProperty(name, defaultValue, String.class);
    } else {
        return defaultValue;
    }
}


private String getSavedRail(SlingHttpServletRequest request, String key) {
    try {
        Cookie cookie = request.getCookie(key);

        if (cookie == null) {
            return null;
        }

        return URLDecoder.decode(cookie.getValue(), "utf-8");
    } catch (UnsupportedEncodingException impossible) {
        throw new RuntimeException(impossible);
    }
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

private RenderCondition getEditCondition(PageContext pageContext, SlingHttpServletRequest request, Resource resource) throws Exception {
    RenderCondition rc = null;
    Resource condition = resource.getChild("editcondition");

    if (condition != null) {
        String rt = condition.getValueMap().get("sling:resourceType", String.class);
        rc = fetchData(pageContext, request, condition, rt, RenderCondition.class);
    }

    if (rc == null) {
        rc = SimpleRenderCondition.TRUE;
    }

    return rc;
}

@SuppressWarnings("unchecked")
private <T> T fetchData(PageContext pageContext, SlingHttpServletRequest request, Resource resource, String resourceType, Class<T> type) throws Exception {
    if (resourceType == null) return null;

    try {
        RequestDispatcher dispatcher = request.getRequestDispatcher(resource, new RequestDispatcherOptions(resourceType));

        if (dispatcher != null) {
            dispatcher.include(request, new JspSlingHttpServletResponseWrapper(pageContext));
            return (T) request.getAttribute(type.getName());
        }

        return null;
    } finally {
        request.removeAttribute(type.getName());
    }
}
%>
