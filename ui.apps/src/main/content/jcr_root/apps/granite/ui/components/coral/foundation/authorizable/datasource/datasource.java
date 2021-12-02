/*************************************************************************
 * ADOBE CONFIDENTIAL
 * ___________________
 *
 * Copyright 2016 Adobe
 * All Rights Reserved.
 *
 * NOTICE: All information contained herein is, and remains
 * the property of Adobe and its suppliers, if any. The intellectual
 * and technical concepts contained herein are proprietary to Adobe
 * and its suppliers and are protected by all applicable intellectual
 * property laws, including trade secret and copyright laws.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from Adobe.
 **************************************************************************/
package apps.granite.ui.components.coral.foundation.authorizable.datasource;

import java.io.IOException;
import java.util.Iterator;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.ValueFactory;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;

import com.adobe.granite.security.user.UserManagementService;
import org.apache.commons.collections4.Transformer;
import org.apache.commons.collections4.iterators.TransformIterator;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.Query;
import org.apache.jackrabbit.api.security.user.QueryBuilder;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.scripting.SlingBindings;
import org.apache.sling.api.scripting.SlingScriptHelper;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;

import com.adobe.granite.security.user.UserProperties;
import com.adobe.granite.security.user.UserPropertiesService;
import com.adobe.granite.ui.components.Config;
import com.adobe.granite.ui.components.ExpressionHelper;
import com.adobe.granite.ui.components.ExpressionResolver;
import com.adobe.granite.ui.components.ds.DataSource;
import com.adobe.granite.ui.components.ds.SimpleDataSource;

@SuppressWarnings("serial")
public class datasource extends SlingSafeMethodsServlet {

    private static final String PATH_GIVEN_NAME = UserPropertiesService.PROFILE_PATH + "/@" + UserProperties.GIVEN_NAME;
    private static final String PATH_FAMILY_NAME = UserPropertiesService.PROFILE_PATH + "/@" + UserProperties.FAMILY_NAME;
    private static final String PATH_DISPLAY_NAME = UserPropertiesService.PROFILE_PATH + "/@" + UserProperties.DISPLAY_NAME;
    private static final String PATH_EMAIL = UserPropertiesService.PROFILE_PATH + "/@" + UserProperties.EMAIL;

    @Override
    protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response)
            throws ServletException, IOException {
        try {
            SlingScriptHelper sling = getScriptHelper(request);
            final ResourceResolver resolver = request.getResourceResolver();
            Session session = resolver.adaptTo(Session.class);
            UserManager um = resolver.adaptTo(UserManager.class);
            UserManagementService ums = sling.getService(UserManagementService.class);

            ExpressionHelper ex = new ExpressionHelper(sling.getService(ExpressionResolver.class), request);
            User currentUser = resolver.adaptTo(User.class);

            Config dsCfg = new Config(request.getResource().getChild(Config.DATASOURCE));

            String queryString = ex.getString(dsCfg.get("query", String.class));
            long offset = ex.get(dsCfg.get("offset", "0"), long.class);
            long limit = ex.get(dsCfg.get("limit", "20"), long.class);
            QuerySelector selector = QuerySelector.fromName(ex.getString(dsCfg.get("selector", String.class)));
            Boolean serviceUserFilter = parseBooleanFilter(ex.getString(dsCfg.get("serviceUserFilter", "exclude")));
            Boolean impersonableUserFilter = parseBooleanFilter(ex.getString(dsCfg.get("impersonableUserFilter", String.class)));

            Query query = createQuery(session, ums, currentUser, offset, limit, queryString, selector, serviceUserFilter, impersonableUserFilter);

            Iterator<Authorizable> authorizables = um.findAuthorizables(query);

            final DataSource datasource = new SimpleDataSource(new TransformIterator<>(authorizables, new Transformer<Authorizable, Resource>() {
                @Override
                public Resource transform(Authorizable o) {
                    try {
                        return resolver.getResource(o.getPath());
                    } catch (RepositoryException e) {
                        throw new RuntimeException(e);
                    }
                }
            }));

            request.setAttribute(DataSource.class.getName(), datasource);
        } catch (RepositoryException e) {
            throw new ServletException(e);
        }
    }

    private static SlingScriptHelper getScriptHelper(ServletRequest request) {
        SlingBindings bindings = (SlingBindings) request.getAttribute(SlingBindings.class.getName());
        return bindings.getSling();
    }

    private static Boolean parseBooleanFilter(String value) {
        if ("includeonly".equals(value)) {
            return true;
        } else if ("exclude".equals(value)) {
            return false;
        }
        return null;
    }

    private static Query createQuery(final Session session, final UserManagementService ums, final User currentUser, final long offset, final long limit,
                                     final String queryString, final QuerySelector selector, final Boolean serviceUserFilter, final Boolean impersonableUserFilter) {

        return new Query() {
            @Override
            public <T> void build(QueryBuilder<T> builder) {
                try {
                    builder.setLimit(offset, limit);
                    builder.setSelector(selector.clazz);

                    T condition = createTermCondition(queryString, builder);

                    if (serviceUserFilter != null) {
                        if (serviceUserFilter) {
                            condition = and(builder, condition, createServiceUserCondition(builder, session.getValueFactory()));
                        } else {
                            condition = and(builder, condition, builder.not(createServiceUserCondition(builder, session.getValueFactory())));
                        }
                    }

                    if (impersonableUserFilter != null) {
                        if (impersonableUserFilter) {
                            condition = and(builder, condition, createImpersonateCondition(ums, currentUser, builder));
                        } else {
                            condition = and(builder, condition, builder.not(createImpersonateCondition(ums, currentUser, builder)));
                        }
                    }

                    if (condition != null) {
                        builder.setCondition(condition);
                    }
                } catch (RepositoryException e) {
                    throw new RuntimeException(e);
                }
            }
        };
    }

    private static <T> T createTermCondition(String query, QueryBuilder<T> builder) {
        T condition = null;

        if (StringUtils.isNotBlank(query)) {
            condition = builder.or(builder.contains(".", query), builder.contains(".", query + "*"));
        }

        return condition;
    }

    private static <T> T createServiceUserCondition(QueryBuilder<T> builder, ValueFactory valueFactory) {
        return builder.eq("@jcr:primaryType", valueFactory.createValue("rep:SystemUser"));
    }

    private static <T> T createImpersonateCondition(UserManagementService ums, User currentUser, QueryBuilder<T> builder) throws RepositoryException {
        if (currentUser.isAdmin()) {
            // give all users EXCEPT oneself and "anonymous"
            return builder.and(builder.not(builder.nameMatches(currentUser.getID())), builder.not(builder.nameMatches(ums.getAnonymousId())));
        } else {
            return builder.impersonates(currentUser.getPrincipal().getName());
        }
    }

    private static <T> T and(QueryBuilder<T> builder, T condition1, T condition2) {
        return (condition1 != null) ? builder.and(condition1, condition2) : condition2;
    }

    private static enum QuerySelector {
        All(Authorizable.class), User(User.class), Group(Group.class);

        private Class<? extends Authorizable> clazz;

        private QuerySelector(Class<? extends Authorizable> clazz) {
            this.clazz = clazz;
        }

        public static QuerySelector fromName(String name) {
            if (StringUtils.isNotBlank(name)) {
                for (QuerySelector e : QuerySelector.values()) {
                    if (name.equals(e.name().toLowerCase())) {
                        return e;
                    }
                }
            }
            return All;
        }
    }
}
