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
%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
          import="java.util.HashMap,
                  java.util.Iterator,
                  javax.jcr.Session,
                  org.apache.commons.collections4.Transformer,
                  org.apache.commons.collections4.iterators.TransformIterator,
                  org.apache.sling.api.resource.ResourceResolver,
                  org.apache.sling.api.wrappers.ValueMapDecorator,
                  org.apache.jackrabbit.api.security.user.Authorizable,
                  org.apache.jackrabbit.api.security.user.UserManager,
                  org.apache.jackrabbit.commons.jackrabbit.user.AuthorizableQueryManager,
                  com.adobe.granite.security.user.util.AuthorizableUtil,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ds.DataSource,
                  com.adobe.granite.ui.components.ds.SimpleDataSource,
                  com.adobe.granite.ui.components.ds.ValueMapResource" %><%--###
SelectDatasource
================

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/authorizable/selectdatasource
   :datasource:

   A datasource providing authorizable. It is compatible with
   :granite:servercomponent:`Select </libs/granite/ui/components/coral/foundation/form/select>` and :granite:servercomponent:`Autocomplete </libs/granite/ui/components/coral/foundation/form/autocomplete>`.
   Internally it uses `AuthorizableQueryManager`_.

   It has the following content structure:

   .. gnd:gnd::

      [granite:AuthorizableSelectDatasource]

      /**
       * The query JSON string as documented at `AuthorizableQueryManager`_.
       */
      - query (String)


.. _AuthorizableQueryManager: http://jackrabbit.apache.org/api/2.4/org/apache/jackrabbit/commons/jackrabbit/user/AuthorizableQueryManager.html
###--%><%

Config cfg = new Config(resource.getChild(Config.DATASOURCE));
final ResourceResolver resolver = resourceResolver;
Session session = resourceResolver.adaptTo(Session.class);

UserManager userManager = resourceResolver.adaptTo(UserManager.class);
AuthorizableQueryManager queryManager = new AuthorizableQueryManager(userManager, session.getValueFactory());

Iterator<Authorizable> results = queryManager.execute(cfg.get("query", String.class));

DataSource ds = new SimpleDataSource(new TransformIterator<>(results, new Transformer<Authorizable, Resource>() {
    public Resource transform(Authorizable auth) {
        try {
	        ValueMap vm = new ValueMapDecorator(new HashMap<String, Object>());
	        vm.put("value", auth.getPrincipal().getName());
	        vm.put("text", AuthorizableUtil.getFormattedName(resolver, auth));

	        return new ValueMapResource(resolver, auth.getPath(), "nt:unstructured", vm);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}));

request.setAttribute(DataSource.class.getName(), ds);
%>