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
%><%@page session="false"
          import="com.adobe.cq.projects.api.Project,
                  com.adobe.cq.projects.api.ProjectMember,
                  com.adobe.cq.projects.api.ProjectMemberRole,
                  com.adobe.granite.security.user.UserProperties,
                  com.adobe.granite.security.user.UserPropertiesManager,
                  com.adobe.granite.security.user.util.AuthorizableUtil,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.xss.XSSAPI,
                  com.day.cq.i18n.I18n,
                  org.apache.commons.lang.StringUtils,
                  org.apache.jackrabbit.api.JackrabbitSession,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ResourceResolver,
                  org.apache.sling.api.resource.ValueMap,
                  org.apache.sling.commons.json.JSONException,
                  org.apache.sling.commons.json.JSONObject,
                  javax.jcr.Node,
                  javax.jcr.RepositoryException,
                  javax.jcr.Session,
                  java.util.Calendar,
                  java.util.Iterator,
                  java.util.Map,
                  org.apache.sling.api.resource.ResourceUtil" %>
<%@include file="/libs/granite/ui/global.jsp"%><%
%><ui:includeClientLib categories="cq.projects.admin.projecteam"/><%
%><sling:include path="<%= resource.getPath() %>" resourceType="cq/gui/components/projects/admin/datasource/projectroledefaults" /> <%

AttrBuilder attrs = new AttrBuilder(request, xssAPI);
Config cfg = new Config(resource);
attrs.addClass(cfg.get("class", String.class));
attrs.addClass("team-table");
attrs.addOthers(cfg.getProperties(), "class", "renderReadOnly");
String membersTitle = xssAPI.filterHTML(i18n.getVar(cfg.get("fieldLabel", String.class)));


    Resource projectResource = resource.getResourceResolver().getResource(request.getParameter("item"));

    Project masterProject = null;

    Resource masterProjectResource = null;

    if (projectResource==null) {
        masterProjectResource = resource.getResourceResolver().getResource(request.getParameter("masterproject"));
    } else {
        // might be a cloned project -> look up the parent project name
        String masterProjectReferencePath = projectResource.getValueMap().get("masterProjectReference", String.class);
        if (StringUtils.isNotBlank(masterProjectReferencePath)) {
            masterProjectResource = resource.getResourceResolver().getResource(masterProjectReferencePath);
        }
    }

    if (masterProjectResource !=null && !ResourceUtil.isNonExistingResource(masterProjectResource)) {
        masterProject = masterProjectResource.adaptTo(Project.class);
        projectResource = masterProjectResource;
    }

    boolean allowRemove = true;
    if (masterProjectResource != null) {
        allowRemove = false;
    }

    Project project = null;
    if (projectResource != null) {
        project = projectResource.adaptTo(Project.class);
    }
%>

<div class="team coral-Form-fieldwrapper team-members-table"><%
    if (masterProject != null) {
        %><label class="coral-Form-fieldlabel"><%= membersTitle %> (<%=i18n.get("Team provided by Master Project")%>:&nbsp;<span><a href="<%= xssAPI.getValidHref("/libs/cq/core/content/projects/masterproperties.html?item=" + masterProjectResource.getPath())%>" target="_blank"><%= xssAPI.encodeForHTML(masterProject.getTitle()) %></a></span>)</label><%
    } else {
        %><label class="coral-Form-fieldlabel"><%= membersTitle %></label><%
    }
%><table <%=attrs.build()%>>
        <tbody><%
        if (project != null) {
            Iterator<ProjectMember> teamMemberIterator = project.getMembers().iterator();
            while (teamMemberIterator.hasNext()) {
                ProjectMember projectMember = teamMemberIterator.next();
                ProjectMemberRole role = projectMember.getRoles().iterator().next();

                 JSONObject obj = loadUserProfile(resourceResolver, projectMember.getId());
                 if (obj !=null) {
                    out.write(addUser(xssAPI, i18n, false, obj, role, allowRemove, request.getContextPath()));
                }
            }
        } else {
            {
                JSONObject obj;

                String ownerId = resourceResolver.getUserID();
                obj = loadUserProfile(resourceResolver, ownerId);
                if (obj != null) {
                    out.write(addUser(xssAPI, i18n, false, obj, (ProjectMemberRole)request.getAttribute("ownerRole"), false, request.getContextPath()));
               }
            }

            Map<ProjectMemberRole, String[]> roleDefaults = (Map<ProjectMemberRole, String[]>) request.getAttribute("roleDefaults");

            if (roleDefaults != null) {
                Iterator<Map.Entry<ProjectMemberRole, String[]>> entries = roleDefaults.entrySet().iterator();

                while (entries.hasNext()) {
                    Map.Entry<ProjectMemberRole, String[]> entry = entries.next();

                    if (entry.getValue() != null && entry.getValue().length > 0 ) {

                        for (String userId : entry.getValue()) {
                            // add default members
                            JSONObject defaultUserJSON = loadUserProfile(resourceResolver, userId);
                            if (defaultUserJSON != null) {
                                out.write(addUser(xssAPI, i18n, false, defaultUserJSON, entry.getKey(), false, request.getContextPath()));
                            }
                        }
                    }
                }
            }
        }   %>
        </tbody>
    </table>
</div>

<%!
    String addUser(XSSAPI xssAPI, I18n i18n, boolean isNew,  JSONObject user, ProjectMemberRole role, boolean allowRemove, String ctx) throws JSONException {
        StringBuilder builder = new StringBuilder();
        builder.append("<tr class=\"" + isNew + "\">");
        builder.append("<td class=\"avatar\">");
        if (user.has("avatarUrl") && StringUtils.isNotEmpty(user.getString("avatarUrl"))) {
            String avatarUrl = user.getString("avatarUrl");
            builder.append("<img src=" + xssAPI.getValidHref(avatarUrl) +">");
        }
        builder.append("</td>");
        builder.append("<td class=\"name\">");
        String xssName = user.has("name") ? xssAPI.encodeForHTMLAttr(user.getString("name")) : "";
        builder.append("<span>" + xssName + "</span>");

        String xssUserId = user.has("userId") ? xssAPI.encodeForHTMLAttr(user.getString("userId")) : "";
        builder.append("<input type=\"hidden\" name=\"teamMemberUserId\" value=\"" + xssUserId + "\">");
        builder.append("</td>");
        builder.append("<td class=\"email\">");

        String xssEmail = user.has("email") ? xssAPI.encodeForHTMLAttr(user.getString("email")) : "";
        builder.append("<span class=\"greyText\">" + xssEmail + "</span>");
        builder.append("</td>");

        builder.append("<td class=\"role greyText\">");
        builder.append("<span>" + xssAPI.encodeForHTML(i18n.getVar(role.getDisplayName())) + "</span>");
        builder.append("<input type=\"hidden\" name=\"teamMemberRoleId\" value=\"" + xssAPI.encodeForHTMLAttr(role.getId()) + "\">");
        builder.append("</td>");

    	/*builder.append("<td class=\"path\">");
        builder.append("<button is=\"coral-button\" type=\"button\" variant=\"quiet\" onclick=\"window.location.href='/mnt/overlay/granite/security/content/v2/usereditor.html"+ xssAPI.encodeForHTMLAttr(user.getString("path")) +"'\" class=\"foundation-field-edit\" title=\"" + "Edit" + "\">");
        builder.append("<i class=\"coral-Icon coral-Icon--sizeXS coral-Icon--edit\"></i>");
        builder.append("</button>");
        builder.append("</td>");*/

        builder.append("<td class=\"remove\">");
        if (allowRemove) {
            String buttonTitle = xssAPI.encodeForHTMLAttr(i18n.get("Remove"));
            builder.append("<button is=\"coral-button\" type=\"button\" variant=\"quiet\" class=\"foundation-field-edit project-members-remove-user\" title=\"" + buttonTitle + "\" data-dismiss=\"modal\">");
            builder.append("<i class=\"coral-Icon coral-Icon--sizeXS coral-Icon--delete\"></i>");
            builder.append("</button>");

        }
        builder.append("</tr>");

        return builder.toString();
    }

    JSONObject loadUserProfile(ResourceResolver resourceResolver, String userId) throws RepositoryException, JSONException {
        Authorizable authorizable = fastGetAuthorizable(resourceResolver, userId);
        if (authorizable == null) {
            return null;
        }
        UserProperties up = fastGetUserProperties(resourceResolver, authorizable);

        if (up == null) {
            return null;
        }

        JSONObject teamMemberData = new JSONObject();
        teamMemberData.put("userId", userId);
        teamMemberData.put("path", authorizable.getPath());

        teamMemberData.put("name", up.getDisplayName());
        String avatarPath = up.getResource("photos/primary/image") != null
                ? up.getResource("photos/primary/image").getPath()
                : null;
        String avatarCk = avatarPath != null
                ? imageCK(up.getResource(avatarPath))
                : null;
        String avatarUrl = null;
        if (StringUtils.isEmpty(avatarPath)) {
            if (authorizable.isGroup()) {
                avatarUrl = "/libs/granite/security/clientlib/themes/default/resources/sample-group-thumbnail.36.png";
            } else {
                avatarUrl = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.36.png";
            }
        } else {
            avatarUrl = avatarPath + avatarCk;
        }

        teamMemberData.put("avatarUrl", avatarUrl);

        teamMemberData.put("email", up.getProperty(UserProperties.EMAIL));

        return teamMemberData;
    }

    private UserProperties fastGetUserProperties(ResourceResolver resolver, Authorizable authorizable) throws RepositoryException {
        UserPropertiesManager upm = resolver.adaptTo(UserPropertiesManager.class);
        String authorizablePath = authorizable.getPath();
        if (authorizablePath != null) {
            return AuthorizableUtil.getProfile(upm, authorizable.getID());
        }
        return null;
    }

    private Authorizable fastGetAuthorizable(ResourceResolver resourceResolver, String authorizableId) throws RepositoryException {
        JackrabbitSession session = (JackrabbitSession) resourceResolver.adaptTo(Session.class);
        Authorizable authorizable = session.getUserManager().getAuthorizable(authorizableId);
        return authorizable;
    }

    String imageCK(Resource resource) {
        if (resource == null) return "";
        ValueMap metadata = resource.adaptTo(ValueMap.class);
        Calendar cal = metadata.get("jcr:created", Calendar.class);
        if (cal == null) return "";

        return "?ck=" + (cal.getTimeInMillis() / 1000);
    }
%>
