<%
%><%@ include file="/libs/granite/ui/global.jsp" %><%
%><%@ page session="false"
          import="java.util.HashMap,
                  org.apache.sling.api.wrappers.ValueMapDecorator,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.Field" %><%

    Config cfg = cmp.getConfig();

    Object value;
    if (cfg.get("multiple", false)) {
        String name = cfg.get("name", String.class);
        value = cmp.getValue().getContentValue(name, new String[0]);
    } else {
        value = cmp.getValue().val(cmp.getExpressionHelper().getString(cfg.get("value", "")));
    }

    ValueMap vm = new ValueMapDecorator(new HashMap<String, Object>());
    vm.put("value", value);

    request.setAttribute(Field.class.getName(), vm);
%>