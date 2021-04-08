<%--
  ADOBE CONFIDENTIAL

  Copyright 2016 Adobe Systems Incorporated
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
--%>
<%@page session="false"
        import="com.adobe.granite.ui.components.Config,
                com.adobe.granite.ui.components.ExpressionHelper,
                com.adobe.granite.ui.components.ExpressionResolver,
                com.adobe.granite.ui.components.rendercondition.RenderCondition,
                com.adobe.granite.ui.components.rendercondition.SimpleRenderCondition,
                org.apache.commons.lang3.StringUtils,
                org.apache.jackrabbit.api.security.user.Authorizable,
                org.apache.jackrabbit.api.security.user.Group,
                org.apache.sling.api.resource.Resource,
                java.util.Iterator" %>
<%
%>
<%@include file="/libs/granite/ui/global.jsp" %>
<%
    ExpressionHelper ex = new ExpressionHelper(sling.getService(ExpressionResolver.class), pageContext);
    boolean render = true;

    Config cfg = new Config(resource);
    String projectPath = ex.getString(cfg.get("projectPath", String.class));

    if (StringUtils.isNotBlank(projectPath)) {
        Resource projectResource = resourceResolver.getResource(projectPath);
        String masterReference = projectResource.getValueMap().get("masterProjectReference", String.class);
        if (StringUtils.isNotBlank(masterReference)) {
            render = false;
        }


        String roleOwner = projectResource.getValueMap().get("role_owner", String.class);
        if (StringUtils.isNotBlank(roleOwner)) {
            Authorizable auth = resourceResolver.adaptTo(Authorizable.class);
            if (null != auth && render) {
                render = false;
                Iterator<Group> projectGroups = auth.memberOf();
                while (projectGroups.hasNext()) {
                    Group group = projectGroups.next();
                    if (StringUtils.equals(group.getID(), roleOwner) || StringUtils.equals(group.getID(), "mediahub-project-administrator") || StringUtils.equals(group.getID(), "administrators") || StringUtils.equals(auth.getID(), "admin")) {
                        render = true;
                    }
                }
            }


        }
    }


    request.setAttribute(RenderCondition.class.getName(), new SimpleRenderCondition(render));
%>
