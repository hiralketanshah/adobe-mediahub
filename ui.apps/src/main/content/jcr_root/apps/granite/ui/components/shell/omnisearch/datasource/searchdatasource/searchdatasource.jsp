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
%>
<%@include file="/libs/granite/ui/global.jsp" %>
<%
%>
<%@page import="com.adobe.granite.omnisearch.api.core.OmniSearchService,
                com.adobe.granite.ui.components.Config,
                com.adobe.granite.ui.components.ExpressionHelper,
                com.adobe.granite.ui.components.ds.DataSource,
                com.adobe.granite.ui.components.ds.EmptyDataSource,
                com.day.cq.search.result.SearchResult,
                org.apache.commons.collections4.Transformer,
                org.apache.commons.collections4.iterators.TransformIterator,
                org.apache.commons.lang3.StringUtils,
                org.apache.jackrabbit.api.security.user.Authorizable,
                org.apache.jackrabbit.api.security.user.Group,
                org.apache.sling.api.resource.ResourceWrapper,
                java.util.ArrayList,
                java.util.Iterator,
                java.util.List,
                java.util.Map" %>
<%

    final DataSource ds;

    final Map<String, String[]> predicateParameters = request.getParameterMap();
    final String location = request.getParameter("location");

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

    if (StringUtils.isBlank(location) || (("user".equalsIgnoreCase(location) || "group".equalsIgnoreCase(location)) && !canViewPrincipals)) {
        // if the location is not set, return an empty collection because we are not able to mix the types
        ds = EmptyDataSource.instance();
    } else {
        final ExpressionHelper ex = cmp.getExpressionHelper();
        final Config dsCfg = new Config(resource.getChild(Config.DATASOURCE));

        final long offset = ex.get(dsCfg.get("offset", String.class), Long.class);
        final long limit = ex.get(dsCfg.get("limit", String.class), Long.class);

        final OmniSearchService searchService = sling.getService(OmniSearchService.class);
        final Map<String, SearchResult> result = searchService.getSearchResults(resourceResolver, predicateParameters, limit, offset);
        final SearchResult searchResult = result.get(location);

        if (searchResult == null) {
            ds = EmptyDataSource.instance();
        } else {
            String itemResourceType = null;
            final Resource configRes = searchService.getModuleConfiguration(resourceResolver, location);

            if (configRes != null) {
                final String itemType = dsCfg.get("itemType", "card");
                final String itemTypePropertyName = "list".equals(itemType) ? "listItemPath" : "cardPath";
                final ValueMap vm = configRes.getValueMap();

                if (itemTypePropertyName.equals("cardPath")) {
                    itemResourceType = vm.get(itemTypePropertyName, "granite/ui/components/shell/omnisearch/defaultcard");
                } else {
                    itemResourceType = vm.get(itemTypePropertyName, String.class);
                }

                if (vm.get("facets", false)) {
                    request.setAttribute("com.day.cq.search.result.SearchResult", searchResult);
                }
            }

            final String itemRT = itemResourceType;

            ds = new DataSource() {
                @Override
                public Iterator<Resource> iterator() {
                    Iterator<Resource> it = searchResult.getResources();

                    if (itemRT != null) {
                        it = new TransformIterator<>(it, new Transformer<Resource, Resource>() {
                            @Override
                            public Resource transform(Resource r) {
                                return new ResourceWrapper(r) {
                                    @Override
                                    public String getResourceType() {
                                        return itemRT;
                                    }
                                };
                            }
                        });
                    }

                    return it;
                }

                @Override
                public Long getOffset() {
                    return searchResult.getStartIndex();
                }

                @Override
                public Long getLimit() {
                    return searchResult.getHitsPerPage();
                }

                @Override
                public Long getGuessTotal() {
                    return searchResult.getTotalMatches();
                }
            };
        }
    }

    request.setAttribute(DataSource.class.getName(), ds);
%>