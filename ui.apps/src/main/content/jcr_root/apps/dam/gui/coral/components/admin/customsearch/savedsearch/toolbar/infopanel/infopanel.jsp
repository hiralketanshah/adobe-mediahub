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
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page import="com.day.cq.i18n.I18n,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  com.day.cq.dam.commons.util.DamUtil,
				  org.apache.sling.jcr.base.util.AccessControlUtil,
				  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.Group,
				  javax.jcr.Session"
%><%
    final String SAVE_COLLECTION = "Save";
    final String EDIT_COLLECTION = "Edit";
    Authorizable user = resourceResolver.adaptTo(Authorizable.class);
    String userCollsHome = DamUtil.getUserCollectionsPath(resourceResolver);

    UserManager userManager = AccessControlUtil.getUserManager(resourceResolver.adaptTo(Session.class));
      boolean isAdmin = false;
      if(userManager != null){
          User currentUser = (User) userManager.getAuthorizable(resourceResolver.getUserID());
          Group group = (Group)userManager.getAuthorizable("administrators");
          if(currentUser != null){
              if(group != null){
                isAdmin = group.isMember(currentUser) || "admin".equals(resourceResolver.getUserID());
              } 
              if((!isAdmin) && (userManager.getAuthorizable("mediahub-administrators") != null) ){
                isAdmin = ((Group)userManager.getAuthorizable("mediahub-administrators")).isMember(currentUser);
              }
              if((!isAdmin) && (userManager.getAuthorizable("mediahub-basic-entity-manager") != null) ){
                isAdmin = ((Group)userManager.getAuthorizable("mediahub-basic-entity-manager")).isMember(currentUser);
              }
          }
      }

%><ui:includeClientLib categories="dam.search.smartcollection.infopanel.coral"/>
<div id="cq-dam-admin-search-toolbar-actions-<%= SAVE_COLLECTION.toLowerCase() %>" class="section action cq-dam-admin-search-toolbar-actions">
        <div class="heading"><%= i18n.getVar(SAVE_COLLECTION) %></div>
        <form class="foundation-form coral-Form--vertical" id="dam-smartcollection-form-action-save" action="<%=request.getContextPath() %>/libs/granite/omnisearch/savedsearch" method="post">
        	<input type="hidden" name="_charset_" value="utf-8">
            <input type="hidden" name=":operation" value="create">
            <input type="hidden" name="collectionPath" value="<%=xssAPI.getValidHref(userCollsHome)%>">
            <input type="hidden" name="teamMemberRole" value="owner">
            <input type="hidden" name="teamMemberPrincipalName" value="<%=xssAPI.encodeForHTMLAttr(user.getID())%>">
            <input type="hidden" name="skipteamupdate" value="false">
            <input type="hidden" name="autoAdjustTitle" value="false">
            <input type="hidden" name="createsmartcollection" value="true">
            <label class="greyText coral-Form-fieldlabel"><%= i18n.get("Name") %></label>
            <input is="coral-textfield" placeholder="<%= i18n.get("Enter name of collection") %>" name="title" value="">
			<%
    			if(isAdmin){
   			%>
            <div class="coral-Form-fieldwrapper coral-Form-fieldwrapper--singleline make-public" style="margin-top: 0.5rem">
                <coral-checkbox class="greyText" name="public" value="true">
                    <%= i18n.get("Public") %>
                </coral-checkbox>
            </div>
            <%
                }
            %>
            <div class="cq-dam-admin-search-toolbar-actions-buttonbar">
                <button is="coral-button" class="cq-dam-admin-search-toolbar-actions-cancel smallText withLabel" block title="<%= i18n.get("Cancel") %>" ><%= i18n.get("Cancel") %></button>
                <button is="coral-button" class="cq-dam-admin-search-toolbar-actions-save smallText withLabel" variant="primary" trackingfeature="aem:assets:search:smartcollection:save" disabled block title="<%= i18n.get("Save") %>" type="button" disabled><%= i18n.get("Save") %></button>
            </div>
        </form>
</div>

<div id="cq-dam-admin-search-toolbar-actions-<%= EDIT_COLLECTION.toLowerCase() %>" class="section action cq-dam-admin-search-toolbar-actions">
        <div class="heading"><%= i18n.getVar(EDIT_COLLECTION) %></div>

        <form class="foundation-form coral-Form--vertical" id="dam-smartcollection-form-action-edit" action="<%=request.getContextPath() %>/libs/granite/omnisearch/savedsearch" method="post">
        	<input type="hidden" name="_charset_" value="utf-8">
            <input type="hidden" name=":operation" value="update">
            <input type="hidden" name="collectionPath" value="<%=xssAPI.getValidHref(userCollsHome)%>">
            <input type="hidden" name="teamMemberRole" value="owner">
            <input type="hidden" name="teamMemberPrincipalName" value="<%=xssAPI.encodeForHTMLAttr(user.getID())%>">
            <input type="hidden" name="skipteamupdate" value="true">
            <input type="hidden" name="createsmartcollection" value="true">
            <label class="greyText coral-Form-fieldlabel"><%= i18n.get("Name") %></label>
            <input is="coral-textfield" class="field" placeholder="<%= i18n.get("Enter name of collection") %>" name="title">
			<%
    			if(isAdmin){
   			%>
            <div class="coral-Form-fieldwrapper coral-Form-fieldwrapper--singleline make-public" style="margin-top: 0.5rem">
                <coral-checkbox class="greyText" name="public" value="true">
                    <%= i18n.get("Public") %>
                </coral-checkbox>
            </div>
            <%
                }
            %>
            <div class="cq-dam-admin-search-toolbar-actions-buttonbar">
                <button is="coral-button" class="cq-dam-admin-search-toolbar-actions-cancel smallText withLabel" block title="<%= i18n.get("Cancel") %>" ><%= i18n.get("Cancel") %></button>
                <button is="coral-button" class="cq-dam-admin-search-toolbar-actions-save smallText withLabel" variant="primary" disabled block title="<%= i18n.get("Save") %>" type="button" disabled><%= i18n.get("Save") %></button>
            </div>
        </form>
</div>