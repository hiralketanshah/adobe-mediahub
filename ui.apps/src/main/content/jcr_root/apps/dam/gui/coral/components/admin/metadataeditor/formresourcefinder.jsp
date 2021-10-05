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
%><%@include file="/libs/granite/ui/global.jsp" %><%
%><%@page session="false"
          import="java.util.List,
                  org.apache.sling.api.resource.Resource,
                  javax.jcr.Node,
                  com.day.cq.commons.jcr.JcrConstants,
                  com.day.cq.dam.commons.util.DamUtil,
                  com.day.cq.dam.api.DamConstants,
                  com.day.cq.dam.api.ui.editor.metadata.MetadataEditorHelper,
                  com.day.cq.dam.commons.util.SchemaFormHelper,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.confmgr.Conf,
                  org.apache.sling.api.request.RequestParameter,
                  com.day.cq.dam.commons.util.DamConfigurationConstants,
                  org.apache.sling.resource.collection.ResourceCollection"%><%

Resource assetRes = resourceResolver.getResource(slingRequest.getRequestPathInfo().getSuffix());
Boolean isReport = false;
if(assetRes == null){
    RequestParameter[] items = slingRequest.getRequestParameters("item");
    if(items != null && items.length > 0) {
        isReport = items[0].getString().indexOf("/var/dam/reports") == 0 ? true : false;
        assetRes = resourceResolver.getResource(items[0].getString());
    }
}
if(assetRes == null){
    return;
}
Conf conf = assetRes.adaptTo(Conf.class);
Resource schemaExtHomeRes = conf.getItemResource(DamConfigurationConstants.ADMIN_UI_EXTENSION_CONF_RELPATH + "/metadataschema");
String schemaExtHome = (schemaExtHomeRes !=null) ? schemaExtHomeRes.getPath() : DamConfigurationConstants.DEFAULT_METADATA_SCHEMA_HOME;
String schemaHomeOOTB = conf.getItem(DamConfigurationConstants.ADMIN_UI_OOTB_CONF_RELPATH).get(DamConfigurationConstants.METADATA_SCHEMA_HOME, DamConfigurationConstants.OOTB_METADATA_SCHEMA_FORM_HOME);

String type = (String)request.getAttribute("aem.assets.ui.properties.assettype");
String[] content = (String[])request.getAttribute("aem.assets.ui.properties.content");
if(content.length == 1 && content[0] == null)return;
// provide this argument to select a different form than default. This is for extensibility in forms/contentfragments
String defaultFormTypeName = (String)request.getAttribute("aem.assets.ui.properties.defaultformtype");
if(defaultFormTypeName == null || defaultFormTypeName.trim().isEmpty()){
    defaultFormTypeName = "default";
}
defaultFormTypeName = "/" + defaultFormTypeName + "/";

String relativeFormPath = "";

RequestParameter forcedFormTypeName = slingRequest.getRequestParameter("forcedFormTypeName");
if (forcedFormTypeName != null && !forcedFormTypeName.toString().isEmpty()) {
	//CQ-91662 : if a user has selected a schema form, then use that as default and no need to determine the schema from from selected assets.
	relativeFormPath = forcedFormTypeName.toString()+ "/"; 
}
else{
	//determine the schema form from content/assets.
	String commonParent = null;
	if (content.length > 1) {
		if (hasCommonParent(content)) {
			commonParent = getParent(content[0]);
		}
	} else {
		commonParent = getParent(content[0]);
	}
	
	Config cfg = new Config(resource);
	
	boolean isCollection = true;
	for (String each : content) {
	    if (resourceResolver.resolve(each).adaptTo(ResourceCollection.class) == null) {
	        isCollection = false;
	        break;
	    }
	}
	
	Resource appsBulkForm = resourceResolver.getResource(schemaExtHome + "/bulkview");
	
// find relative path. (relative to schema home)
if (isReport) {
    relativeFormPath = "/report";
} else if (DamUtil.isLivefyreFragment(assetRes)) {
    relativeFormPath = "/ugc-contentfragment";
} else if (isCollection) {
    MetadataEditorHelper helper = sling.getService(MetadataEditorHelper.class);
    Resource tabsResource = helper.getEditorFormResource(assetRes);
    relativeFormPath = tabsResource == null ? "/collection" : tabsResource.getParent().getParent().getPath();
} else if (content.length > 1 && null != appsBulkForm) {
		// Go to bulkview form in case of bulk view for backward compatibilty
		relativeFormPath = "/bulkview";
	} else {
	    if (commonParent != null) {
	    	Resource parent = resourceResolver.resolve(commonParent);
	        Node parentNode = parent.adaptTo(Node.class);
	        while((!parentNode.hasProperty("jcr:content/metadataSchema")) && (!parent.getPath().equals(DamConstants.MOUNTPOINT_ASSETS))) {
	            parent = parent.getParent();
	            parentNode = parent.adaptTo(Node.class);
	        }
	        if (parentNode.hasProperty("jcr:content/metadataSchema")) {
	    		String absoluteFormPath = parentNode.getProperty("jcr:content/metadataSchema").getString();
	    		if (resourceResolver.resolve(absoluteFormPath).getResourceType().equals(Resource.RESOURCE_TYPE_NON_EXISTING)) {
	    			relativeFormPath = defaultFormTypeName + type;
	    		} else {
	    			relativeFormPath = absoluteFormPath.substring(absoluteFormPath.lastIndexOf("/")) + "/" + type;
	    		}
	        } else {
	            relativeFormPath = defaultFormTypeName + type;
	        }
	    } else {
	        relativeFormPath = defaultFormTypeName + type;
	    }
	}
}
String formPath = null;

// find form path using relative path found above
if (resourceResolver.getResource(relativeFormPath) != null || resourceResolver.getResource(relativeFormPath.toLowerCase()) != null) {
    formPath = relativeFormPath;
} else if (resourceResolver.getResource(schemaExtHome + relativeFormPath) != null || resourceResolver.getResource(schemaExtHome + relativeFormPath.toLowerCase()) != null) {
    formPath = schemaExtHome + relativeFormPath;
} else {
    formPath = schemaHomeOOTB + relativeFormPath;
}

//Making the form path to an existing form
Resource  res = resourceResolver.getResource(formPath);
if (res == null) {
 //try for the lowercase match
 res = resourceResolver.getResource(formPath.toLowerCase());
}

while (res == null || res.getChild("items/tabs") == null) {
	if (relativeFormPath.trim().isEmpty()) {
        relativeFormPath = "/default";
	}
    //try for the exact match (required for the upgrade scenario)
	res = resourceResolver.getResource(schemaHomeOOTB + relativeFormPath);
	if (res == null) {
	    //try for the lowercase match
	    res = resourceResolver.getResource((schemaHomeOOTB + relativeFormPath).toLowerCase());
    }
    if ((res == null || res.getChild("items/tabs") == null) && relativeFormPath.lastIndexOf('/') > -1) {
		relativeFormPath = relativeFormPath.substring(0, relativeFormPath.lastIndexOf('/'));
     //try for the exact match (required for the upgrade scenario)
		res = resourceResolver.getResource(schemaExtHome + relativeFormPath);
     if (res == null) {
         //try for the lowercase match
         res = resourceResolver.getResource((schemaExtHome + relativeFormPath).toLowerCase());
     }
	} else {
		break;
	}
}

//Merge forms
List<Resource> masterTabList = SchemaFormHelper.getMasterForms(res);
Resource rootTabRes = null;
if (!masterTabList.isEmpty()) {
    Resource root = masterTabList.get(0);
    rootTabRes = root.getChild("items/tabs");
    for (int i = 1; i < masterTabList.size(); i++) {
        Resource formRes = masterTabList.get(i);
        Resource formTabRes = formRes.getChild("items/tabs");
        rootTabRes = SchemaFormHelper.mergeFormTabResource(rootTabRes, formTabRes);
    }
}

//get form for current Resource
Resource currentFormResource = res;
Resource currentTabListRes =  currentFormResource.getChild("items/tabs");
if (rootTabRes != null) {
	rootTabRes = SchemaFormHelper.mergeFormTabResource(rootTabRes, currentTabListRes);
} else {
	rootTabRes = currentTabListRes;
}

// add additional tabs
    String[] additionalTabs = new String[] {"/insights", "/references", "/stock", "/msm/livecopysource", "/msm/livecopy"};
    for(int i = 0; i < additionalTabs.length; i++) {
    Resource tabsResource = resourceResolver.getResource(schemaHomeOOTB + additionalTabs[i]);
        if("/insights".equalsIgnoreCase(additionalTabs[i])){
			tabsResource = resourceResolver.getResource("/apps/dam/content/schemaeditors/forms" + additionalTabs[i]);

        }
    if(tabsResource != null && cmp.getRenderCondition(tabsResource, false).check()){
    tabsResource = tabsResource.getChild("items/tabs");
        if(null != tabsResource){
            rootTabRes = SchemaFormHelper.mergeFormTabResource(rootTabRes, tabsResource);
        }
    }
}

if (rootTabRes != null) {
	request.setAttribute("aem.assets.ui.properties.formresource", rootTabRes);
}

%><%!

private String getParent (String path) {
	if (null != path && path.lastIndexOf("/") > 0) {
		return path.substring(0, path.lastIndexOf("/"));
	} else {
		return null;
	}
}

private boolean hasCommonParent (String[] paths) {
	String parent = "";
	for (String each : paths) {
		String temp = "";
		if (null != each && each.lastIndexOf("/") > 0) {
			temp = each.substring(0, each.lastIndexOf("/"));
		}
		if ("".equals(parent)) {
			parent = temp;
		} else if (!parent.equals(temp)) {
			return false;
		}
	}
	return true;
}
%>
