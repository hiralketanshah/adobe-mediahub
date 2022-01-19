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

import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Query;
import org.apache.jackrabbit.api.security.user.QueryBuilder;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
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

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

/**
 * Servlet that writes some sample content into the response. It is mounted for
 * all resources of a specific Sling resource type. The
 * {@link SlingSafeMethodsServlet} shall be used for HTTP methods that are
 * idempotent. For write operations use the {@link SlingAllMethodsServlet}.
 */
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET,
                ServletResolverConstants.SLING_SERVLET_PATHS + "=" + "/bin/mediahub/internalusers"
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
            Session session = resolver.adaptTo(Session.class);

            String queryString = request.getParameter("query");
            Query query = new Query() {
                @Override
                public <T> void build(QueryBuilder<T> builder) {
                    T condition = null;

                    if (queryString != null && queryString.length() > 0) {
                        condition = builder.or(builder.contains(".", queryString), builder.contains(".", queryString + "*"));
                    }

                    T internalCondition = null;
                    try {
                        internalCondition = builder.eq("profile/@type", session.getValueFactory().createValue("internal"));
                    } catch (RepositoryException e) {
                    }
                    condition = (condition == null) ? internalCondition : builder.and(internalCondition, condition);

                    if (condition != null) {
                        builder.setCondition(condition);
                    }

                    builder.setLimit(0, 25);
                }
            };

            UserManager um = resolver.adaptTo(UserManager.class);
            Iterator<Authorizable> authorizablesIt = um.findAuthorizables(query);

            StringBuilder sb = new StringBuilder();
            while (authorizablesIt.hasNext()) {
                Authorizable auth = authorizablesIt.next();
                String name = auth.getID();
                Value[] familyName = auth.getProperty("./profile/familyName");
                Value[] firstname = auth.getProperty("./profile/givenName");
                if (familyName != null && firstname != null && !StringUtils.isEmpty(familyName[0].toString()) && !StringUtils.isEmpty(firstname[0].toString())) {
                    name = firstname[0].toString() + " " + familyName[0].toString();
                }
                sb.append("<li class='coral-SelectList-item coral-SelectList-item--option' data-value='" + auth.getID() + "'>" + name + "</li>");
            }
            response.getWriter().write(sb.toString());
        } catch (LoginException e) {
            LOGGER.error("Error while fecthing system user : {0}", e);
        } catch (RepositoryException e) {
            LOGGER.error("Error while fecthing user and user info : {0}", e);
        }

    }


}