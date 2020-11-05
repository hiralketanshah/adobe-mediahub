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
%><%@page import="com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  org.apache.sling.api.resource.Resource,
                  com.adobe.granite.ui.components.Tag,
                  java.util.Iterator" %><%
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><ui:includeClientLib categories="cq.projects.admin.pod.assetpod"/><%
Config cfg = new Config(resource);

    Tag tag = cmp.consumeTag();
    tag.setName("form");

    AttrBuilder attrs = tag.getAttrs();
    attrs.addClass("coral-Form--vertical");
    attrs.addClass("foundation-form");
    attrs.addOther("foundation-form-ajax", "true");
    attrs.addOther("foundation-form-ui", "none");
    attrs.add("method", "post");
    attrs.add("id", cfg.get("id", String.class));
    attrs.addRel(cfg.get("rel", String.class));
    attrs.addClass("coral-Modal");
    attrs.addHref("action", cmp.getExpressionHelper().getString(cfg.get("action", String.class)));
    attrs.addOthers(cfg.getProperties(), "id", "rel", "class", "action");

    tag.printlnStart(out);%>

    <div class="coral-Modal-header">
        <i class="coral-Modal-typeIcon coral-Icon coral-Icon--sizeS"></i>
        <h2 class="coral-Modal-title coral-Heading coral-Heading--2"><%=i18n.get("Configure Asset Pod")%></h2>
    </div>
    <div class="coral-Modal-body cq-projects-layout-overflow-visible">
        <sling:include path="<%= resource.getPath() %>" resourceType="granite/ui/components/foundation/container" />
    </div>
    <div class="coral-Modal-footer">
        <button class="coral-Button" type="reset" data-dismiss="modal"><%= i18n.get("Cancel") %></button>
        <button class="coral-Button coral-Button--primary" type="submit"><%= i18n.get("Accept") %></button>
    </div>
<%
    tag.printlnEnd(out);
%>