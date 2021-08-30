<%--
  ADOBE CONFIDENTIAL
  ___________________
  Copyright 2019 Adobe
  All Rights Reserved.
  NOTICE: All information contained herein is, and remains
  the property of Adobe and its suppliers, if any. The intellectual
  and technical concepts contained herein are proprietary to Adobe
  and its suppliers and are protected by all applicable intellectual
  property laws, including trade secret and copyright laws.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe.
--%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
           import="java.net.URLEncoder,
			     java.util.Iterator,
			     com.adobe.granite.ui.components.AttrBuilder,
			     com.adobe.granite.ui.components.Config,
				   org.apache.commons.lang3.StringUtils,
				   java.util.Set,
				   org.apache.sling.settings.SlingSettingsService,
				   com.adobe.granite.ui.components.ExpressionHelper,
				   org.apache.jackrabbit.api.security.user.UserManager,
				   org.apache.jackrabbit.api.security.user.User,
				   org.apache.jackrabbit.api.security.user.Group,
				   org.apache.jackrabbit.api.security.user.Authorizable" %><%

    Config cfg = cmp.getConfig();
    ExpressionHelper ex = cmp.getExpressionHelper();

    String path = ex.getString(cfg.get("path", String.class));

    String consoleId = StringUtils.trimToNull(ex.getString(cfg.get("consoleId", String.class)));
    String navigationId = consoleId;

    String navigationUrl = "/mnt/overlay/granite/ui/content/shell/globalnav.html";
    if (navigationId != null) {
        navigationUrl += "?consoleId=" + URLEncoder.encode(navigationId, "utf-8");
    }

    AttrBuilder headerHome = new AttrBuilder(request, xssAPI);
    headerHome.addClass("globalnav-toggle");
    headerHome.addHref("data-globalnav-toggle-href", navigationUrl);

    String runmode = " ";
    Set<String> runmodes = sling.getService(SlingSettingsService.class).getRunModes();
    if( runmodes.contains("dev")){
      runmode = " Dev";
    } else if(runmodes.contains("stage")){
      runmode = " Stage";
    }

    boolean isMediaAdmin = false;
    UserManager userManager = resourceResolver.adaptTo(UserManager.class);
    User currentUser = (User)userManager.getAuthorizable(resourceResolver.getUserID());
    if(StringUtils.equals("admin", resourceResolver.getUserID()) || (userManager.getAuthorizable("administrators") != null && ((Group)userManager.getAuthorizable("administrators")).isMember(currentUser)) ){
        isMediaAdmin = true;
    }else if(userManager.getAuthorizable("mediahub-super-administrators") != null && ((Group)userManager.getAuthorizable("mediahub-super-administrators")).isMember(currentUser) ){
        isMediaAdmin = true;
    }else if(userManager.getAuthorizable("mediahub-basic-entity-manager") != null && ((Group)userManager.getAuthorizable("mediahub-basic-entity-manager")).isMember(currentUser) ){
        isMediaAdmin = true;
    }

%><coral-shell-header-home <%= headerHome %>> 
	<a href="#" style="text-decoration: none;">
		<img src="/content/dam/technique/mediahub/logo-bnp.svg" height="34" width="165" alt="BNP Paribas">
	</a>
	<coral-shell-homeanchor-label class="bnp-text-style"> | Mediahub MVP<%= runmode %></coral-shell-homeanchor-label>
</coral-shell-header-home>

<coral-shell-header-actions>
    <coral-shell-menubar><%
        final Resource headerActions = resourceResolver.getResource("/mnt/overlay/granite/ui/content/shell/header/actions");
        if (headerActions != null) {
            for (Iterator<Resource> it = headerActions.listChildren(); it.hasNext();) {
                %><sling:include resource="<%= it.next() %>" /><%
            }
        }
    %></coral-shell-menubar>
</coral-shell-header-actions>

<coral-shell-header-content id="mediahub-menu">
    <coral-shell-menubar>
      <coral-shell-menubar-item class="bnpmedialibrary">
        <a href="/assets.html/content/dam/medialibrary">
          <coral-shell-homeanchor-label class="bnp-text-style bnpmedialibrary"> Medialibrary </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item class="bnpprojects">
        <a href="/projects.html/content/projects">
          <coral-shell-homeanchor-label class="bnp-text-style bnpprojects"> Projects </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <% if(isMediaAdmin){%>
      <coral-shell-menubar-item>
        <a href="#">
          <coral-shell-homeanchor-label class="bnp-text-style"> Photostock </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <coral-shell-menubar-item>
        <a href="/mnt/overlay/dam/gui/content/stock/search.html">
          <coral-shell-homeanchor-label class="bnp-text-style"> Adobe Stock </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
      <%}%>
      <coral-shell-menubar-item class="bnpcollections">
        <a href="/mnt/overlay/dam/gui/content/collections.html/content/dam/collections">
          <coral-shell-homeanchor-label class="bnp-text-style bnpcollections"> Collections </coral-shell-homeanchor-label>
        </a>
      </coral-shell-menubar-item>
    </coral-shell-menubar>
</coral-shell-header-content>

<script>


    $("#mediahub-menu coral-shell-homeanchor-label").css('background-color','');
    $("#mediahub-menu coral-shell-homeanchor-label").css('color','');

    if(window.location.href.includes('/content/dam/medialibrary')) {
        $(".bnpmedialibrary").css('background-color', '#00915a');
        $(".bnpmedialibrary").css('color', '#FFFFFF');
        $(".foundation-layout-panel-bodywrapper").css('background-color', '#bfe4d6');
    }else if(window.location.href.includes('/content/dam/collections')) {
        $(".bnpcollections").css('background-color','#00915a');
        $(".bnpcollections").css('color','#FFFFFF');
    }else if(window.location.href.includes('/content/dam/projects') || window.location.href.includes('/content/projects')) {
        $(".bnpprojects").css('background-color','#00915a');
        $(".bnpprojects").css('color','#FFFFFF');
    }

    $(".foundation-layout-panel-content").ready(function(){
        if(window.location.href.includes('/content/dam/medialibrary')) {
            $(".foundation-layout-panel-bodywrapper").css('background-color', '#bfe4d6');
        }else{
            $(".foundation-layout-panel-bodywrapper").css('background-color', '');
        }


        var observerHeader = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
                if (mutation.type == "attributes") {
                    if(window.location.href.includes('/content/dam/medialibrary')) {
                        $(".foundation-layout-panel-bodywrapper").css('background-color', '#bfe4d6');
                    }else{
                        $(".foundation-layout-panel-bodywrapper").css('background-color', '');
                    }

                }
            });
        });

        observerHeader.observe(document.querySelector('coral-shell-header'), {
            attributes: true //configure it to listen to attribute changes
        });


    });


</script>
