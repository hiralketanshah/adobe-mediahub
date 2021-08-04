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


  MEDIAHUB NOTICE
  This overlay has been created to allow user and group search only to mediahub administrators

--%><%
%>
<%@ include file="/libs/granite/ui/global.jsp" %>
<%
%>
<%@ page import="com.adobe.granite.omnisearch.api.core.OmniSearchService,
                 com.adobe.granite.ui.components.Config,
                 com.adobe.granite.ui.components.ExpressionHelper,
                 com.adobe.granite.ui.components.ds.AbstractDataSource,
                 com.adobe.granite.ui.components.ds.DataSource,
                 com.adobe.granite.ui.components.ds.EmptyDataSource,
                 com.day.cq.search.result.SearchResult,
                 org.apache.commons.collections4.Transformer,
                 org.apache.commons.collections4.iterators.TransformIterator,
                 org.apache.jackrabbit.api.security.user.Authorizable,
                 org.apache.jackrabbit.api.security.user.Group,
                 org.apache.sling.api.resource.ResourceResolver,
                 org.apache.sling.api.resource.ResourceWrapper,
                 org.apache.sling.api.resource.SyntheticResource,
                 org.apache.sling.api.wrappers.ValueMapDecorator,
                 java.util.*,
                 java.util.Map.Entry" %>
<%
%><%
    final OmniSearchService searchService = sling.getService(OmniSearchService.class);
    final ExpressionHelper ex = cmp.getExpressionHelper();
    final Config dsCfg = new Config(resource.getChild(Config.DATASOURCE));

    // Clone the paramMap since it returns an UnmodifiableMap
    final Map<String, String[]> predicateParams = new HashMap<String, String[]>(request.getParameterMap());

    // Remove the location since we want mixed results
    predicateParams.remove("location");

    final long limit = ex.get(dsCfg.get("limit", "20"), long.class);
    final String moduleRT = dsCfg.get("moduleResourceType", String.class);

    final Map<String, SearchResult> result = searchService.getSearchResults(resourceResolver, predicateParams, limit, 0);

    final ArrayList<Resource> items = new ArrayList<Resource>();

    final DataSource ds;

    Authorizable auth = resourceResolver.adaptTo(Authorizable.class);
    boolean canViewPrincipals = false;
    if (null != auth) {
        Iterator<Group> projectGroups = auth.memberOf();
        List<String> userGroups = new ArrayList();
        while (projectGroups.hasNext()) {
            Group group = projectGroups.next();
            userGroups.add(group.getID());
        }
        if (userGroups.contains("mediahub-basic-entity-manager") || userGroups.contains("mediahub-basic-project-manager") || userGroups.contains("mediahub-administrators") || userGroups.contains("mediahub-super-administrators") || userGroups.contains("administrators")) {
            canViewPrincipals = true;
        }
    }

    for (Entry<String, SearchResult> entry : result.entrySet()) {
        SearchResult searchResult = entry.getValue();

        if (searchResult.getTotalMatches() > 0) {
            String moduleID = entry.getKey();

            if (isSearchAllowed(moduleID, canViewPrincipals)) {
                Resource configRes = searchService.getModuleConfiguration(resourceResolver, moduleID);

                String itemRT = "";
                String moduleName = moduleID;

                if (configRes != null) {
                    ValueMap props = configRes.getValueMap();

                    itemRT = props.get("cardPath", "granite/ui/components/shell/omnisearch/defaultcard");

                    moduleName = i18n.getVar(props.get("jcr:title", String.class));
                    if (moduleName == null) {
                        moduleName = moduleID;
                    }
                }

                items.add(new SearchModuleResource(resourceResolver, resource.getPath(), moduleID, moduleName, moduleRT, itemRT, searchResult));
            }
        }
    }

    if (items.isEmpty()) {
        ds = EmptyDataSource.instance();
    } else {
        ds = new AbstractDataSource() {
            public Iterator<Resource> iterator() {
                return items.iterator();
            }
        };
    }
    request.setAttribute(DataSource.class.getName(), ds);


%><%!

    private boolean isSearchAllowed(String module, boolean canViewPrincipals) {
        if (("user".equalsIgnoreCase(module) || "group".equalsIgnoreCase(module)) && !canViewPrincipals) {
            return false;
        }
        return true;
    }


    private class SearchModuleResource extends SyntheticResource {
        private SearchResult result;
        private String itemRT;
        private Map<String, Object> values;

        public SearchModuleResource(ResourceResolver resolver, String path, String moduleID, String moduleName, String moduleRT, String itemRT, SearchResult result) {
            super(resolver, path, moduleRT);
            this.result = result;
            this.itemRT = itemRT;

            values = new HashMap<String, Object>();
            values.put("granite-omnisearch-location-display", moduleName);
            values.put("granite-omnisearch-location-value", moduleID);
            values.put("granite-omnisearch-count", result.getTotalMatches());
            values.put("granite-omnisearch-count-hasmore", result.hasMore());
        }

        @SuppressWarnings("unchecked")
        public <AdapterType> AdapterType adaptTo(Class<AdapterType> type) {
            if (type.equals(ValueMap.class)) {
                return (AdapterType) new ValueMapDecorator(values);
            } else {
                return super.adaptTo(type);
            }
        }

        public Iterator<Resource> listChildren() {
            return new TransformIterator<>(result.getResources(), new Transformer<Resource, Resource>() {
                public Resource transform(Resource r) {
                    return new ResourceWrapper(r) {
                        public String getResourceType() {
                            return itemRT;
                        }
                    };
                }
            });
        }
    }
%>