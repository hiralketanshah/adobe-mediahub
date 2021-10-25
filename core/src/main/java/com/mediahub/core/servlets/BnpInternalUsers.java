/*
 *  Copyright 2015 Adobe Systems Incorporated
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.mediahub.core.servlets;

import com.adobe.acs.commons.httpcache.util.UserUtils;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.QueryUtils;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import javax.annotation.Resources;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.jackrabbit.oak.spi.security.user.util.UserUtil;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.ServletResolverConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Servlet that writes some sample content into the response. It is mounted for
 * all resources of a specific Sling resource type. The
 * {@link SlingSafeMethodsServlet} shall be used for HTTP methods that are
 * idempotent. For write operations use the {@link SlingAllMethodsServlet}.
 *
 */
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET,
            ServletResolverConstants.SLING_SERVLET_PATHS + "=" + "/bin/project/internalusers"
        })
public class BnpInternalUsers extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(BnpInternalUsers.class);

    private static final long serialVersionUID = 1L;

    final transient Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doGet(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws ServletException, IOException {
        try {
            ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo);

            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> predicates = QueryUtils.getPredicateMapInternalUsers("/home/users");
            Query query = builder.createQuery(PredicateGroup.create(predicates), resolver.adaptTo(Session.class));

            SearchResult result = query.getResult();
            LOGGER.info("Query {}", result.getQueryStatement());

            Iterator<Resource> resources = result.getResources();
            StringBuilder sb = new StringBuilder();
            while(resources.hasNext()){
                Resource userResource = resources.next();
                User user = userResource.adaptTo(User.class);
                sb.append("<li class='coral-SelectList-item coral-SelectList-item--option' data-value='" + user.getID() +"'>" + user.getID() + "</li>");
            }
            response.getWriter().write(sb.toString());
        } catch (LoginException e) {
            LOGGER.error("Error while fecthing system user : {0}", e);
        } catch (RepositoryException e) {
            LOGGER.error("Error while fecthing user and user info : {0}", e);
        }

    }
}
