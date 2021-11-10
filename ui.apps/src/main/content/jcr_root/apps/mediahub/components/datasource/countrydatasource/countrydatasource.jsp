<%--
  ADOBE CONFIDENTIAL

  Copyright 2014 Adobe Systems Incorporated
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
%><%@page session="false" import="java.util.ArrayList,
                                  java.util.HashMap,
                                  java.util.List,
                                  org.apache.commons.collections.Transformer,
                                  org.apache.commons.collections.iterators.TransformIterator,
                                  org.apache.sling.api.resource.ValueMap,
                                  org.apache.sling.api.wrappers.ValueMapDecorator,
                                  com.adobe.granite.ui.components.ds.DataSource,
                                  com.adobe.granite.ui.components.ds.EmptyDataSource,
                                  com.adobe.granite.ui.components.ds.SimpleDataSource,
                                  com.adobe.granite.ui.components.ds.ValueMapResource,
                                  org.apache.sling.api.resource.ResourceResolver,
                                  java.util.TreeSet,
                                  java.util.Comparator,
                                  java.util.Locale"%><%
%><%@include file="/libs/granite/ui/global.jsp" %><%

    final ResourceResolver resolver = resourceResolver;
    final String path = resource.getPath();
    Locale[] locales = Locale.getAvailableLocales();

    final Locale emptyLocale = new Locale("", "", "");

    //used TreeSet to remove the duplicate countries and sort them after the country names
    TreeSet<Locale> localesSet = new TreeSet<Locale>(new Comparator<Locale>() {
        public int compare(Locale o1, Locale o2) {
            return o1.getDisplayCountry().compareTo(o2.getDisplayCountry());
        }
    });
    for (Locale locale : locales) {
        String country = locale.getDisplayCountry(Locale.ENGLISH);
        if (!country.isEmpty()) {
            localesSet.add(locale);
        }
    }

    List<Locale> localesList = new ArrayList<Locale>();
    localesList.addAll(localesSet);
    localesList.add(0, emptyLocale);

    i18n.get("Serbia and Montenegro");
    i18n.get("Select");

    DataSource ds;
    if (localesSet.isEmpty()) {
        ds = EmptyDataSource.instance();
    } else {
        ds = new SimpleDataSource(new TransformIterator(localesList.iterator(), new Transformer() {
            public Object transform(Object input) {
                try {
                    Locale locale = (Locale) input;
                    String country = locale.getDisplayCountry(Locale.ENGLISH);
                    String value = locale.getCountry();
                    if (locale.equals(emptyLocale)) {
                        country = "Select";
                        value = "";
                    }

                    ValueMap vm = new ValueMapDecorator(new HashMap<String, Object>());
                    vm.put("value", value);
                    vm.put("text", country);

                    return new ValueMapResource(resolver, path, "nt:unstructured", vm);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }));
    }

    request.setAttribute(DataSource.class.getName(), ds);
%>