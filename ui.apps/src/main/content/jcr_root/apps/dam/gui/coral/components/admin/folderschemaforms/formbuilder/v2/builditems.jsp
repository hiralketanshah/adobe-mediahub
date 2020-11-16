<%--

  ADOBE CONFIDENTIAL
  __________________

   Copyright 2013 Adobe Systems Incorporated
   All Rights Reserved.

  NOTICE:  All information contained herein is, and remains
  the property of Adobe Systems Incorporated and its suppliers,
  if any.  The intellectual and technical concepts contained
  herein are proprietary to Adobe Systems Incorporated and its
  suppliers and are protected by trade secret or copyright law.
  Dissemination of this information or reproduction of this material
  is strictly forbidden unless prior written permission is obtained
  from Adobe Systems Incorporated.

--%>
<%@ page session="false"
         contentType="text/html"
         pageEncoding="utf-8"
         import="com.adobe.granite.ui.components.formbuilder.FormResourceManager,
                 com.day.cq.commons.LabeledResource,
                 com.adobe.granite.ui.components.Config,
                 com.day.cq.i18n.I18n,
                 java.util.HashMap,
                 org.apache.sling.api.resource.Resource,
                 org.apache.sling.api.resource.ValueMap,
                 java.util.ArrayList,
                 java.util.Iterator,
                 com.adobe.granite.confmgr.Conf,
                 com.day.cq.dam.commons.util.DamConfigurationConstants,
                 java.util.List,
                 javax.jcr.Node"
        %><%
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><cq:defineObjects /><%
	Config cfg = new Config(resource);
    String sfx = slingRequest.getRequestPathInfo().getSuffix();
    sfx = sfx == null ? "" : sfx;

%>
<div class="editor-right">
    <coral-tabview>
  		<coral-tablist target="coral-demo-panel-1">
            <coral-tab id="tab-add" href="#" data-target="#field-add"><%= i18n.get("Build Form") %></coral-tab>
            <coral-tab id="tab-edit" href="#" data-target="#field-edit"><%= i18n.get("Settings") %></coral-tab>
            <% if(resource.getPath().contains("foldermetadataschemaeditor")) { %>
            <coral-tab id="tab-edit-rules" href="#" data-target="#field-rules"><%= i18n.get("Rules") %></coral-tab>
            <% } %>
        </coral-tablist>
		<coral-panelstack>
	        <coral-panel id="field-add">
				<ul id="formbuilder-field-templates" role="menubar">
	            <%
	                FormResourceManager formResourceManager = sling.getService(FormResourceManager.class);
	                Resource fieldTemplateResource = formResourceManager.getFormFieldResource(resource);
	            %>
	                <li class="field" data-fieldtype="section" tabindex="0" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="viewSingle" alt="" size="M"></coral-icon><span><%= i18n.get("Section Header") %></span>
	                    </div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/sectionfield" />
                    	</script>
	                </li>

                    <li class="field" data-fieldtype="textarea" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="text" alt="" size="M"></coral-icon><span><%= i18n.get("Text Area") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/textarea" />
                    </script>
	                </li>
	
	                <li class="field" data-fieldtype="text" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="text" alt="" size="M"></coral-icon><span><%= i18n.get("Single Line Text") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/textfield" />
                    </script>
	                </li>
	
	                <li class="field" data-fieldtype="text" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="text" alt="" size="M"></coral-icon><span><%= i18n.get("Multi Value Text") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/mvtextfield" />
                    </script>
	                </li>
	
	
	                <li class="field" data-fieldtype="number" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="dashboard" alt="" size="M"></coral-icon><span><%= i18n.get("Number","form builder option") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/numberfield" />
                    </script>
	                </li>

	                <%
                        HashMap<String, Object> values = new HashMap<String, Object>();
                        values.put("sling.resolutionPath", "Field Label");
                        values.put("fieldLabel", "Default Value");
                        values.put("value", "");
                        Resource dateFieldResource = formResourceManager.getDefaultPropertyFieldResource(resource, values);
                    %>
	                <li class="field" data-fieldtype="datepicker" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="calendar" alt="" size="M"></coral-icon><span><%= i18n.get("Date") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= dateFieldResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/datepickerfield" />
                    </script>
	                </li>
	
	                <li class="field" data-fieldtype="dropdown" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="dropdown" alt="" size="M"></coral-icon><span><%= i18n.get("Dropdown") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/dropdownfield" />
                    </script>
	                    <script id="dropdown-option-template" type="text/x-handlebars-template">
                        <sling:include resource="<%= formResourceManager.getDropdownOptionResource(fieldTemplateResource) %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/dropdownfield/dropdownitem" />
                    </script>
	                </li>
	
	                <li class="field" data-fieldtype="text" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="tag" alt="" size="M"></coral-icon><span><%= i18n.get("Standard Tags") %></span></div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= fieldTemplateResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/tagsfield" />
                    </script>
	                </li>

	                <% Resource hiddenFieldResource = formResourceManager.getHiddenFieldResource(resource); %>
	                <li class="field" data-fieldtype="text" tabindex="-1" role="menuitem">
	                    <div class="formbuilder-template-title"><coral-icon icon="viewSingle" alt="" size="M"></coral-icon><span><%= i18n.get("Hidden Field") %></span>
	                    </div>
	                    <script class="field-properties" type="text/x-handlebars-template">
                        <sling:include resource="<%= hiddenFieldResource %>"
                                       resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/formfields/v2/hiddenfield" />
                    </script>
	                </li>
                </ul>
	        </coral-panel>
            <coral-panel id="formbuilder-tab-name" class="tab-form-settings">
	            <div id="tab-name">
	                <sling:include resource="<%= fieldTemplateResource %>"
	                               resourceType="dam/gui/coral/components/admin/folderschemaforms/formbuilder/tabname" />
	            </div>
	            <div class="placeholder">
                    <i><%= i18n.get("Select a metadata schema editor field to edit settings") %></i>
                </div>
	        </coral-panel>
            <% if(resource.getPath().contains("foldermetadataschemaeditor")) { %>
                <coral-panel id="field-rules" class="tab-form-rules">
                    <div class="placeholder">
		                    <span><%= i18n.get("Select a metadata schema editor field to edit rules.") %></span>
                    </div>
	              </coral-panel>
            <% } %>
	    </coral-panelstack>
	</coral-tabview>
</div>