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
--%><%@page session="false"
          import="com.adobe.cq.projects.api.ProjectFilter,
                  com.adobe.cq.projects.api.ProjectManager,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ExpressionHelper,
                  com.adobe.granite.ui.components.ExpressionResolver,
                  com.adobe.granite.ui.components.ds.AbstractDataSource,
                  com.adobe.granite.ui.components.ds.DataSource,
                  com.adobe.granite.ui.components.ds.EmptyDataSource,
                  com.adobe.granite.ui.components.ds.SimpleDataSource,
                  com.day.cq.wcm.api.Template,
                  org.apache.commons.collections.Transformer,
                  org.apache.commons.collections.iterators.TransformIterator,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.sling.api.resource.Resource,
                  org.apache.sling.api.resource.ResourceWrapper,
				  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.api.security.user.User,
                  org.apache.jackrabbit.api.security.user.Group,
				  org.apache.jackrabbit.api.security.user.Authorizable,
                  java.util.ArrayList,
                  java.util.Collection,
                  java.util.Collections,
                  java.util.Comparator,
                  java.util.Iterator,
                  java.util.List"%><%
%><%@include file="/libs/foundation/global.jsp"%><%
    ExpressionHelper ex = new ExpressionHelper(sling.getService(ExpressionResolver.class), pageContext);

    Config cfg = new Config(resource.getChild(Config.DATASOURCE));
    final String itemRT = cfg.get("itemResourceType", String.class);
    String parentPath = ex.getString(cfg.get("path", String.class));
    String targetPath = slingRequest.getRequestPathInfo().getSuffix();

    DataSource ds;
    if (parentPath == null) {
        ds = EmptyDataSource.instance();
    } else {
        // for now always show templates when creating a project
        // (instead show the 'teams' on the team page of the create project)
        if (StringUtils.startsWith(targetPath, "/content/projects/masters")) {
            // create a master project -> only show templates
            final Collection<Template> templates = pageManager.getTemplates("/content/projects");
            final List<Template> templateList = new ArrayList<Template>(templates);
            Collections.sort(templateList, new Comparator<Template>() {
                public int compare(Template c1, Template c2) {
                    if (c1.getRanking() != null && c2.getRanking() != null) {
                        return c1.getRanking().compareTo(c2.getRanking());
                    }
                    return -1;
                }
            });

            ds = new AbstractDataSource() {
                public Iterator<Resource> iterator() {
                    return new TransformIterator(templateList.iterator(), new Transformer() {
                        public Object transform(Object input) {
                            ResourceWrapper wrapper = new ResourceWrapper(((Template) input).adaptTo(Resource.class)) {
                                public String getResourceType() {
                                    return itemRT;
                                }
                            };
                            return wrapper;
                        }
                    });
                }
            };
        } else {
            // create a regular project, i.e. show the master projects or enabled templates.
            ProjectManager pm = resourceResolver.adaptTo(ProjectManager.class);
            ProjectFilter projectFilter = new ProjectFilter();
            projectFilter.setActive(true);

            List<Resource> result = new ArrayList<Resource>();
            final Iterator<Resource> masterProjects = new TransformIterator(findMasterProjects(pm, projectFilter), new Transformer() {
                public Object transform(Object input) {
                    ResourceWrapper wrapper = new ResourceWrapper((Resource) input) {
                        public String getResourceType() {
                            return "cq/gui/components/projects/admin/card/masterprojectcard";
                        }
                    };
                    return wrapper;
                }
            });
            while(masterProjects.hasNext()) {
                result.add(masterProjects.next());
            }

            final List<Template> templateList;
            UserManager userManager = resourceResolver.adaptTo(UserManager.class);
			      User currentUser = (User)userManager.getAuthorizable(resourceResolver.getUserID());
            // Changes as per MED-263 entity manager create project type
            if(userManager.getAuthorizable("mediahub-project-administrator") != null && ((Group)userManager.getAuthorizable("mediahub-project-administrator")).isMember(currentUser)){
                Template template = pageManager.getTemplate("/apps/mediahub/projects/templates/mediahub-projects");
                templateList = new ArrayList<Template>();
                templateList.add(template);
            } else {
                // final Collection<Template> templates;
				        final Collection<Template> templates = pageManager.getTemplates("/content/projects");
                templateList = new ArrayList<Template>(templates);
                Collections.sort(templateList, new Comparator<Template>() {
                public int compare(Template c1, Template c2) {
                        if (c1.getRanking() != null && c2.getRanking() != null) {
                            return c1.getRanking().compareTo(c2.getRanking());
                        }
                        return -1;
                    }
                });
            }

            Iterator<Resource> adaptToResourceIterator = new TransformIterator(templateList.iterator(), new Transformer() {
                public Object transform(Object input) {
                    ResourceWrapper wrapper = new ResourceWrapper(((Template) input).adaptTo(Resource.class)) {
                        public String getResourceType() {
                            return itemRT;
                        }
                    };
                    return wrapper;
                }
            });
            while(adaptToResourceIterator.hasNext()) {
                Resource templateResource = adaptToResourceIterator.next();
                if (!StringUtils.equals(templateResource.getValueMap().get("includeInCreateProject", String.class), "false")) {
                    result.add(templateResource);
                }
            }

            ds = new SimpleDataSource(result.iterator());
        }
    }

    request.setAttribute(DataSource.class.getName(), ds);

%>

<%!
    private void addMasterProjects(ProjectManager pm, List<Resource> result, String currentFolderPath, ProjectFilter projectFilter) {
        if (StringUtils.isNotBlank(currentFolderPath)) {
            Iterator<Resource> children = pm.getChildren(currentFolderPath, projectFilter, 0, -1);
            if (children != null) {
                while(children.hasNext()) {
                    Resource child = children.next();
                    if (child.isResourceType("cq/gui/components/projects/admin/card/projectcard")) {
                        // if this is a projectcard add it to the resulting list
                        result.add(child);
                    } else if (child.isResourceType("cq/gui/components/projects/admin/card/foldercard")){
                        // iterate over project folders to collect the master projects
                        addMasterProjects(pm, result, child.getPath(), projectFilter);
                    }
                }
            }
        }
    }

    private Iterator<Resource> findMasterProjects(ProjectManager pm, ProjectFilter projectFilter) {
        List<Resource> result = new ArrayList<Resource>();
        addMasterProjects( pm, result, "/content/projects/masters", projectFilter );
        return result.iterator();
    }
%>