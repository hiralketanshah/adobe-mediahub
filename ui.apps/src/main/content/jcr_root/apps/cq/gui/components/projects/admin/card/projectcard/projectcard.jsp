<%--
  ADOBE CONFIDENTIAL

  Copyright 2016 Adobe Systems Incorporated
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
                  com.adobe.cq.projects.ui.ProjectHelper,
                  com.day.cq.wcm.api.NameConstants,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.jackrabbit.util.Text,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ValueMap,
                  javax.jcr.RepositoryException,
                  javax.jcr.Session,
                  javax.jcr.security.AccessControlManager,
                  javax.jcr.security.Privilege,
                  java.net.URLEncoder,
                  java.text.SimpleDateFormat,
                  org.apache.sling.commons.json.JSONObject,
                  org.apache.sling.commons.json.JSONArray,
                  com.day.cq.commons.date.RelativeTimeFormat,
                  java.util.ArrayList,
                  java.util.List,
                  java.util.Calendar,
                  java.util.Date,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Tag,
                  java.util.ResourceBundle"
%><%@include file="/libs/granite/ui/global.jsp"%>
<%@ taglib prefix="cq" uri="http://www.adobe.com/taglibs/granite/ui/1.0" %><%

    final Project project = resource.adaptTo(Project.class);

    if (project == null) {
        log.error("Unable to render null project for resource at path: {}", resource.getPath());
        return;
    }

    AccessControlManager acm =  null;
    try {
        acm = resourceResolver.adaptTo(Session.class).getAccessControlManager();
    } catch (RepositoryException e) {
        log.error("Unable to get access manager.", e);
    }

    boolean isProjectMaster = false;
    boolean bIsTranslationProject = false;
    Resource jcrContent = resource.getChild("jcr:content");
    if (jcrContent!=null && jcrContent.getValueMap()!=null) {
        isProjectMaster = "true".equals(jcrContent.getValueMap().get("ismaster", String.class));
    }

    List<String> translationJobNames = new ArrayList<String>();
    List<String> actionRels = new ArrayList<String>();
    actionRels.add("foundation-collection-item-activator");
    if (isProjectMaster) {
        actionRels.add("cq-projects-admin-actions-masterproperties-activator");
    }
    else {
        actionRels.add("cq-projects-admin-actions-properties-activator");
        // only allow delete for regular projects, not master projects
        if (hasPermission(acm, resource, Privilege.JCR_REMOVE_NODE)) {
            actionRels.add("cq-projects-admin-actions-delete-activator");
        }
    }

    if(isTranslationProject(jcrContent)){
        actionRels.add("cq-projects-admin-actions-translationproject-activator");
        getStartTranslationJobsName(jcrContent, translationJobNames);
        bIsTranslationProject = true;
        if (!translationJobNames.isEmpty()) {
            actionRels.add("cq-projects-admin-actions-starttranslation-activator");
        }
    }

    boolean active = project.isActive();

    String thumbnailUrl = request.getContextPath() + resource.getPath() + ".thumb.319.319.png";
    String title = project.getTitle() != null ? project.getTitle() : "";
    String xssTitle = xssAPI.encodeForHTML(title);

    String description = project.getDescription() != null ? project.getDescription() : "";
    if (description.length() >= 200) {
        String ELLIPSIS = "\u2026";
        description = description.substring(0, 200) + ELLIPSIS;
    }
    String xssDescription = xssAPI.encodeForHTML(description);

    String xssPageUrl;
    String xssPropertiesUrl;
    if (isProjectMaster) {
        xssPageUrl = xssAPI.getValidHref(request.getContextPath() + "/libs/cq/core/content/projects/masterproperties.html?item=" + encodeURIComponent(resource.getPath()));
        xssPropertiesUrl = xssAPI.getValidHref(request.getContextPath() + "/mnt/overlay/cq/core/content/projects/masterproperties.html?item=" + encodeURIComponent(resource.getPath()));
    } else {
        String detailsHref = getProjectDetailsUrl(resource);
        xssPageUrl = xssAPI.getValidHref(request.getContextPath() + detailsHref + Text.escapePath(resource.getPath()));
        xssPropertiesUrl = xssAPI.getValidHref(request.getContextPath() + "/apps/mediahub/projects/project-properties/properties.html?item=" + encodeURIComponent(resource.getPath()));
    }

    ResourceBundle resourceBundle = slingRequest.getResourceBundle(slingRequest.getLocale());

    ValueMap map = jcrContent.getValueMap();
    Calendar dueDate = map.get("project.dueDate", Calendar.class);
    Calendar modifiedDate = resource.getValueMap().get("jcr:lastModified",Calendar.class);

    int totalTaskCnt = 0;
    int activeTasks = 0;
    int totalOverdue = 0;
    int completedTasks = 0;
    float percentComplete = 0.0f;

    Resource tasks = jcrContent.getChild("tasks");

    if (tasks != null) {
        ValueMap props = tasks.adaptTo(ValueMap.class);

        if (props.containsKey("totalTasks")) {
            totalTaskCnt = props.get("totalTasks", 0);
        }

        if (props.containsKey("activeTasks")) {
            activeTasks = props.get("activeTasks", 0);
        }

        if (props.containsKey("overdueTasks")) {
            totalOverdue = props.get("overdueTasks", 0);
        }

        completedTasks = totalTaskCnt - (activeTasks + totalOverdue);
        percentComplete = ((float) completedTasks / (float) totalTaskCnt) *  100.0f;
    }

    JSONObject data = new JSONObject();

    JSONArray yArray = new JSONArray();
    yArray.put(completedTasks); //Completed
    yArray.put(activeTasks); //Active
    yArray.put(totalOverdue); //Overdue

    JSONArray xArray = new JSONArray();
    xArray.put(i18n.get("Completed"));
    xArray.put(i18n.get("Active"));
    xArray.put(i18n.get("Overdue"));

    JSONArray sArray = new JSONArray();
    sArray.put("");
    sArray.put("");
    sArray.put("");

    data.put("y", yArray);
    data.put("x", xArray);
    data.put("series", sArray);

    Tag tag = cmp.consumeTag();
    AttrBuilder attrs = tag.getAttrs();
    attrs.addClass("foundation-collection-navigator");
    attrs.addOther("foundation-collection-navigator-href", xssPageUrl);
    attrs.addOther("timeline", "true");
    if(!translationJobNames.isEmpty()){
        attrs.addOther("translationjob-names", new JSONArray(translationJobNames).toString());
    }
%>
<coral-card <%= attrs %>>
    <meta class="foundation-collection-quickactions" data-foundation-collection-quickactions-rel="<%= StringUtils.join(actionRels, " ") %>">
    <coral-card-asset><img class="cq-projects-CardDashboard-image" src="<%= xssAPI.getValidHref(thumbnailUrl) %>"/></coral-card-asset>
    <% if (!isProjectMaster && active && ProjectHelper.isOverdue(resource)) { %>
        <coral-card-info>
            <coral-alert variant="error">
                <coral-alert-content><%= xssAPI.encodeForHTML(i18n.get("Overdue")) %></coral-alert-content>
            </coral-alert>
        </coral-card-info>
    <% } %>
    <coral-card-content>
        <% if (isProjectMaster) { %>
            <coral-card-context><%= xssAPI.encodeForHTML(i18n.get("Master Project")) %></coral-card-context>
        <% } else { %>
            <coral-card-context><%= xssAPI.encodeForHTML(i18n.get("Project")) %></coral-card-context>
        <% } %>
        <coral-card-title class="foundation-collection-item-title"><%= xssTitle %></coral-card-title>
        <% if (!isProjectMaster) { %>
        <coral-card-description>
            <% if (!xssDescription.isEmpty()) { %>
                <span class="cq-projects-CardDashboard-description"><%=xssDescription%></span><br/>
            <% }
                if (bIsTranslationProject) {
                    String jobsInDraft = jcrContent.getValueMap().get("DRAFT", "0");
                    String jobsInProgress = jcrContent.getValueMap().get("TRANSLATION_IN_PROGRESS", "0");
                    String jobsComplete = jcrContent.getValueMap().get("COMPLETE", "0");
                    if (!"0".equals(jobsInDraft) || !"0".equals(jobsInProgress) || !"0".equals(jobsComplete)) { %>
                        <span><%=i18n.get("Translation Job Status")%></span><br/>
                        <% if (!"0".equals(jobsInDraft)) {%>
                            <span style="padding-left: 5%;"><%=i18n.get("New")%>: <%= xssAPI.encodeForHTML(jobsInDraft) %></span><br/>
                        <% }
                        if (!"0".equals(jobsInProgress)) {%>
                            <span style="padding-left: 5%;"><%=i18n.get("In Progress")%>: <%= xssAPI.encodeForHTML(jobsInProgress) %></span><br/>
                        <% }
                        if (!"0".equals(jobsComplete)) {%>
                            <span style="padding-left: 5%;"><%=i18n.get("Completed")%>: <%= xssAPI.encodeForHTML(jobsComplete) %></span>
                        <% }
                    }
                }
             %>
        </coral-card-description>
        <% if (!isProjectMaster && tasks!=null) { %>
            <div class="project-stats-wrapper">
                <div><project-taskstats stats='<%=data.toString()%>' size="XS" outerRadius="0"></project-taskstats></div>
                <div class="project-stats-label"><%=Math.round(percentComplete)%>%, <%= (active) ? i18n.get("Active") : i18n.get("Not Active") %></div>
            </div>
        <% } %>
        <% if (!isProjectMaster && tasks==null) { %>
            <div class="project-stats-wrapper">
                <div class="project-stats-wrapper"><%= (active) ? i18n.get("Active") : i18n.get("Not Active") %></div>
            </div>
        <% } %>
        <coral-card-propertylist>
            <% if(modifiedDate!=null) { %>
            <coral-card-property icon="edit"><foundation-time format="short" type="datetime" value="<%= xssAPI.encodeForHTMLAttr(modifiedDate.getTime().toInstant().toString()) %>"></foundation-time></coral-card-property>
            <% }
            if (dueDate!=null) { %>
            <coral-card-property icon="clock"><foundation-time format="short" type="datetime" value="<%= xssAPI.encodeForHTMLAttr(dueDate.getTime().toInstant().toString()) %>"></foundation-time></coral-card-property>
            <% } %>
        </coral-card-propertylist>
        <% } else { %>
            <coral-card-description><%= xssDescription %></coral-card-description>
        <% } %>
    </coral-card-content>
</coral-card>
<coral-quickactions target="_prev" alignmy="left top" alignat="left top">
    <coral-quickactions-item icon="check" class="foundation-collection-item-activator"><%
    %><%= xssAPI.encodeForHTML(i18n.get("Select")) %><%
    %></coral-quickactions-item>
    <coral-quickactions-item icon="infoCircle" class="foundation-anchor" data-foundation-anchor-href="<%= xssPropertiesUrl %>"><%
    %><%= xssAPI.encodeForHTML(i18n.get("Properties")) %><%
    %></coral-quickactions-item>
</coral-quickactions>
<%!
    boolean hasPermission(AccessControlManager acm, Resource resource, String privilege) {
        try {
            if (acm != null) {
                Privilege p = acm.privilegeFromName(privilege);
                return acm.hasPrivileges(resource.getPath(), new Privilege[]{p});
            }
        } catch (RepositoryException e) {
            // if we have a error then we will return false.
        }
        return false;
    }

    private String getProjectDetailsUrl(Resource resource) {
        // if the template specified a details href then we can use that
        // otherwise default to projects/details.html
        Resource content = resource.getChild(NameConstants.NN_CONTENT);
        if (content != null) {
            ValueMap map = content.adaptTo(ValueMap.class);
            String detailsHref = map.get("detailsHref", String.class);
            if (StringUtils.isNotEmpty(detailsHref)) {
                return detailsHref;
            }
        }

        //TODO - osgi this value
        return "/projects/details.html";
    }

    private String encodeURIComponent(String s) {
        try {
            return URLEncoder.encode(s, "UTF-8");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    boolean isTranslationProject(Resource jcrContent)throws Exception {
        boolean bRetVal = false;
        if (jcrContent != null) {
            if(StringUtils.isNotBlank(jcrContent.getValueMap().get("cq:template", String.class))){
                String strTemplate = jcrContent.getValueMap().get("cq:template", String.class);
                if("/libs/cq/core/content/projects/templates/create-translation-project".equals(strTemplate) 
                    || "/libs/cq/core/content/projects/templates/translation-project".equals(strTemplate)){
                        bRetVal = true;
                }
            }
        }
        return bRetVal;
    }

    void getStartTranslationJobsName(Resource jcrContent, List<String> translationJobNames) throws Exception {
        if (jcrContent != null) {
            if(StringUtils.isNotBlank(jcrContent.getValueMap().get("translationProvider", String.class))){
                String strProvider = jcrContent.getValueMap().get("translationProvider", String.class);
                if(strProvider!=null && strProvider.length()>0){
                    Resource gadgetResource = jcrContent.getChild("dashboard/gadgets");
                    if (gadgetResource != null) {
                        // traverse all child nodes now
                        Iterable<Resource> childList = gadgetResource.getChildren();
                        for (Resource child : childList) {
                            if (canStartTranslationOnChild(child)) {
                                translationJobNames.add(getTranslationJobName(child));
                            }
                        }
                    }
                }
            }
        }
    }

    String getTranslationJobName(Resource child) throws Exception {
        return child.getValueMap().get("jcr:title", "");
    }

    boolean canStartTranslationOnChild(Resource child) throws Exception {
        boolean bRetVal = false;
        if (child != null) {
            if ("cq/gui/components/projects/admin/pod/translationjobpod".equals(child.getResourceType())) {
                // now check the status
                if (StringUtils.isNotBlank(child.getValueMap().get("translationStatus", String.class))) {
                    String strStatus = "|" + child.getValueMap().get("translationStatus", String.class) + "|";
                    if ("|DRAFT|SUBMITTED|SCOPE_REQUESTED|SCOPE_COMPLETED|COMMITTED_FOR_TRANSLATION|"
                        .indexOf(strStatus) != -1) {
                        //now check if there is any node or not
                        Resource childPages = child.getChild("child_pages");
                        if(childPages!=null){
                            bRetVal = childPages.hasChildren();
                        }
                    }
                }
            }
        }
        return bRetVal;
    }
%>
