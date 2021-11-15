<%--
  ADOBE CONFIDENTIAL

  Copyright 2013 Adobe Systems Incorporated
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
%><%@page session="false" contentType="text/html; charset=utf-8"%><%
%><%@page import="java.util.Iterator,
                org.apache.sling.api.resource.Resource,
                org.apache.jackrabbit.api.security.user.Authorizable,
                org.apache.sling.api.resource.ResourceResolver,
                com.day.cq.dam.api.Asset,
				org.apache.sling.jcr.base.util.AccessControlUtil,
                com.day.cq.dam.api.checkout.AssetCheckoutService,
                com.day.cq.dam.commons.util.UIHelper,
                com.day.cq.wcm.msm.api.LiveRelationshipManager,
                com.adobe.granite.security.user.UserProperties,
                com.adobe.granite.security.user.UserPropertiesManager,
                com.adobe.granite.security.user.UserPropertiesService,
                com.adobe.granite.ui.components.AttrBuilder,
                com.adobe.granite.ui.components.Config,
				org.apache.jackrabbit.api.security.user.User,
          		org.apache.jackrabbit.api.security.user.UserManager,
          		org.apache.jackrabbit.api.security.user.Group,
                javax.jcr.Session,
                com.day.cq.i18n.I18n" %><%
%><%@taglib prefix="sling" uri="http://sling.apache.org/taglibs/sling/1.0" %><%
%><%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0" %><%
%><cq:defineObjects /><%

    I18n i18n = new I18n(slingRequest);
    Config cfg = new Config(resource);

    Session session = resourceResolver.adaptTo(Session.class);

    Resource photo = null;
    ResourceResolver resolver = resource.getResourceResolver();
    Authorizable user = resolver.adaptTo(Authorizable.class);
    UserPropertiesManager userPropertiesManager = resolver.adaptTo(UserPropertiesManager.class);
    UserProperties profile = userPropertiesManager.getUserProperties(user.getID(), UserPropertiesService.PROFILE_PATH);

    Resource assetResource = null;
    if (slingRequest.getRequestParameter("item") != null) {
        String assetPath = slingRequest.getRequestParameter("item").getString("UTF-8");
        assetResource = resourceResolver.getResource(assetPath);
    } else {
        assetResource = UIHelper.getCurrentSuffixResource(slingRequest);
    }
    Asset asset = assetResource.adaptTo(Asset.class);
    AssetCheckoutService assetCheckoutService = sling.getService(AssetCheckoutService.class);
    boolean isCheckedOut = false;
    boolean isCheckedOutByCurrentUser = false;
    boolean isSubAsset = false;
    boolean isParentAssetCheckedOut = false;
    boolean isParentAssetCheckedOutByCurrentUser = false;
    boolean canEdit = false;
    String selfId = user.getID();

    if (asset != null && asset.isSubAsset()) {
      isSubAsset = true;
    }
    if (!isSubAsset && asset != null && assetCheckoutService != null) {
        isCheckedOut = assetCheckoutService.isCheckedOut(asset);
    }

    if (isCheckedOut) {
        String checkedOutBy = assetCheckoutService.getCheckedOutBy(asset);
        isCheckedOutByCurrentUser = selfId.equals(checkedOutBy);
    }

    if (isSubAsset && assetCheckoutService != null) {
        Resource parentResource = assetResource.getParent().getParent(); //since immediate parent is subaasets
        Asset parentAsset = parentResource.adaptTo(Asset.class);
        isParentAssetCheckedOut = assetCheckoutService.isCheckedOut(parentAsset);

        if (isParentAssetCheckedOut) {
          String parentAssetCheckedOutBy = assetCheckoutService.getCheckedOutBy(parentAsset);
          isParentAssetCheckedOutByCurrentUser = selfId.equals(parentAssetCheckedOutBy);
        }
    }

    LiveRelationshipManager liveRelationshipManager = resourceResolver.adaptTo(LiveRelationshipManager.class);
    boolean isLiveCopy = liveRelationshipManager.hasLiveRelationship(assetResource);

    canEdit =  !isLiveCopy && (isSubAsset ? (!isParentAssetCheckedOut || isParentAssetCheckedOutByCurrentUser) : (!isCheckedOut || isCheckedOutByCurrentUser));

    if (profile != null) {
        photo = profile.getResource(UserProperties.PHOTOS);
    }
%>

    <div class="cq-common-admin-timeline-toolbar">

        <div class="cq-common-admin-timeline-toolbar-actions cq-common-admin-timeline-toolbar-actions-main cq-common-admin-timeline-toggleable" role="region" hidden><%
            // iterate to render the action buttons (menu to select an action)
            Iterator<Resource> items = cfg.getItems();
            while (items.hasNext()) {
                Config itemCfg = new Config(items.next());
                String title = itemCfg.get("jcr:title", String.class);
                if (title != null) {
                    // only components with a title render a button
                    %><button is="coral-button" class="cq-common-admin-timeline-toolbar-actions-button" data-rel="<%= xssAPI.encodeForHTMLAttr(itemCfg.get("rel")) %>"><%= xssAPI.encodeForHTML(i18n.getVar(title)) %></button> <%
                }
            }
        %></div>

        <%
            // iterate again to render the actions layers
            items = cfg.getItems(cfg.get("itemsName"));
            while (items.hasNext()) {
                Resource item = items.next();
                %><sling:include path="<%= item.getPath() %>" resourceType="<%= (String) item.getResourceType() %>" replaceSelectors="toolbar" /><%
            }
        %>

        <%-- visible part of the toolbar (comments field and button to open actions menu --%>
        <%
            AttrBuilder formAttrs = new AttrBuilder(request, xssAPI);
            String action = cfg.get("actions/createcomment/action");
            formAttrs.addHref("action", action);

            UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
            boolean isAdmin = false;
            if(userManager != null){
                User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
                Group group = (Group)userManager.getAuthorizable("administrators");
                if(currentUser != null){
                    if(group != null){
                        isAdmin = group.isMember(currentUser) || "admin".equals(resourceResolver.getUserID());
                    }
                    if( (!isAdmin) && (userManager.getAuthorizable("mediahub-super-administrators") != null) ){
                        isAdmin = ((Group)userManager.getAuthorizable("mediahub-super-administrators")).isMember(currentUser);
                    }
                }
            }
            if (isAdmin) {
        %>
        <section class="cq-common-admin-timeline-item cq-common-admin-timeline-toolbar-bar">
            <div class="cq-common-admin-timeline-event-icon">
                <%
                    if (photo != null) {
                        String thumbnail = photo.getPath() + "/primary/image.prof.thumbnail.28.png";
                        %><img src="<%= xssAPI.getValidHref(thumbnail) %>" alt=""><%
                    } else {
                        %><coral-icon icon="user" size="S"></coral-icon><%
                    }

                %>
            </div>
            <div class="cq-common-admin-timeline-event-text">
                <form <%= formAttrs.build() %> data-operation="granite:comment">

        <label for="cq-common-admin-timeline-toolbar-actions-comment-text"><%= i18n.get("Comment","Default timeline text") %></label>
        <%
            if (canEdit) {
        %>
                    <input type="text" class="supportmetaphors" id="cq-common-admin-timeline-toolbar-actions-comment-text" is="coral-textfield" name="cq-common-admin-timeline-toolbar-actions-comment" autocomplete="off" data-userpickerurl="/mnt/overlay/dam/gui/content/commons/metaphors/userpicker">
        <%
            } else {
        %>
                    <input type="text" class="supportmetaphors" id="cq-common-admin-timeline-toolbar-actions-comment-text" is="coral-textfield" name="cq-common-admin-timeline-toolbar-actions-comment" autocomplete="off" data-userpickerurl="/mnt/overlay/dam/gui/content/commons/metaphors/userpicker" disabled="disabled">
        <%
            }
        %>


                </form>
        <%
            if (canEdit) {
        %>
                <div id="versionPopup" class="cq-common-admin-timeline-toolbar-actions-button" data-rel="cq-common-admin-timeline-toolbar-actions-main" role="button" tabindex="0" aria-expanded="false" aria-label="<%= i18n.get("Save As Version or Start Workflow") %>">
                    <coral-icon icon="chevronUp" size="S"></coral-icon>
                </div>
        <%
            }
        %>
            </div>
        </section>

        <%
            }
        %>

    </div>