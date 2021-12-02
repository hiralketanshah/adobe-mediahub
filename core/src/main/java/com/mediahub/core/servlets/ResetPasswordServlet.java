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

import com.day.cq.commons.Externalizer;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Session;
import javax.jcr.Value;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.*;

/**
 * Servlet that writes token in user node and to send reset password link. It is mounted for
 * all resources of a specific Sling resource type. The
 * {@link SlingSafeMethodsServlet} shall be used for HTTP methods that are
 * idempotent. For write operations use the {@link SlingAllMethodsServlet}.
 */
@Component(service = Servlet.class,
        property = {
                "sling.servlet.methods=" + HttpConstants.METHOD_POST,
                "sling.servlet.resourceTypes=" + "granite/core/components/login",
                "sling.servlet.selectors=" + "changePassword",
                "sling.servlet.extensions=" + "json"})
@ServiceDescription("To send Forgot Password Link")
public class ResetPasswordServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ResetPasswordServlet.class);

    private static final long serialVersionUID = 1L;

    final transient Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @Reference
    private transient Externalizer externalizer;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Reference
    private transient GenericEmailNotification genericEmailNotification;

    @Override
    protected void doPost(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws ServletException, IOException {
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String userToken = request.getRequestParameterMap().getValue("userToken").getString();
            String password = request.getRequestParameterMap().getValue("j_newpassword").getString();
            if (StringUtils.isNotEmpty(userToken)) {
                Map<String, String> predicateMap = getPredicateMap("/home/users", userToken);
                QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
                Query query = builder.createQuery(PredicateGroup.create(predicateMap), resolver.adaptTo(Session.class));
                SearchResult result = query.getResult();
                Iterator<Resource> userResources = result.getResources();
                while (userResources.hasNext()) {
                    Resource user = userResources.next();
                    User userByToken = user.adaptTo(User.class);
                    Value[] expiryDates = userByToken.getProperty("tokenExpiryDate");
                    if (null != expiryDates && expiryDates.length > 0 && Calendar.getInstance().before(expiryDates[0].getDate())) {
                        user.adaptTo(User.class).changePassword(password);
                    }
                }
                resolver.commit();
            }
        } catch (Exception e) {
            LOGGER.error("Error while changing password", e);
            setErrorResponse(response, "not_able_reset");
        }

    }

    /**
     * @param response
     * @param errorCode
     * @throws IOException
     */
    private void setErrorResponse(SlingHttpServletResponse response, String errorCode) throws IOException {
        response.getWriter().print("User ID not found");
        response.setHeader("X-Reason-Code", errorCode);
        response.setStatus(500);
    }

    /**
     * @return predicate map for query to be framed
     */
    protected Map<String, String> getPredicateMap(String path, String token) {
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, path);
        map.put(BnpConstants.TYPE, BnpConstants.REP_USERS);
        map.put(BnpConstants.FIRST_PROPERTY, "userToken");
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, token);
        map.put(BnpConstants.P_LIMIT, "-1");
        return map;
    }
}
