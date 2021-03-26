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
--%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@page import="org.apache.sling.api.resource.Resource,
                  javax.jcr.Node,
                  javax.jcr.Value,
                  javax.jcr.RepositoryException,
                  javax.jcr.ValueFormatException,
                  javax.jcr.PropertyType,
                  com.day.cq.commons.jcr.JcrConstants,
                  org.apache.sling.resource.collection.ResourceCollection,
                  org.apache.sling.api.resource.ValueMap,
                  org.apache.sling.api.wrappers.ValueMapDecorator,
                  com.day.cq.dam.commons.util.UIHelper,
                  java.util.HashMap,
                  javax.jcr.Property,
                  javax.jcr.PropertyIterator,
                  javax.jcr.nodetype.NodeType,org.slf4j.Logger,
                  org.slf4j.LoggerFactory"%><%!
    /**
     * default logger
     */
    private final Logger log = LoggerFactory.getLogger(getClass());
%><%
Resource currentResource = UIHelper.getCurrentSuffixResource(slingRequest);
if (currentResource == null) {
    return;
}
HashMap<String, Object> hm = new HashMap<String, Object>();
Node currentResNode = currentResource.adaptTo(Node.class);
if (null != currentResNode) {
    if (currentResNode.hasProperty(JcrConstants.JCR_TITLE)) {
        hm.put("title", currentResNode.getProperty(JcrConstants.JCR_TITLE).getString());
        // If the title of folder is read from foldernode, then update title on that node only
        hm.put("titleReadFrom","folderNode");
    } else if (currentResNode.hasNode(JcrConstants.JCR_CONTENT)) {
        Node contentNode = currentResNode.getNode(JcrConstants.JCR_CONTENT);
        if ( null != contentNode && contentNode.hasProperty(JcrConstants.JCR_TITLE)) {
            hm.put("title", contentNode.getProperty(JcrConstants.JCR_TITLE).getString());
        }
    }
    Node metadataNode = currentResNode;
    Node metadataContentNode = null;
    if (metadataNode.getDepth() > 1 && metadataNode.hasNode(JcrConstants.JCR_CONTENT)) {
        metadataContentNode = metadataNode.getNode(JcrConstants.JCR_CONTENT);
        if (metadataContentNode.hasProperty("metadataSchema")) {
            hm.put("metadataSchema",metadataContentNode.getProperty("metadataSchema").getString());
        }
    }
    hm.put("jcr:primaryType", currentResNode.getPrimaryNodeType().getName());

    if (currentResNode.hasNode(JcrConstants.JCR_CONTENT)) {
        Node jcrContentNode = currentResNode.getNode(JcrConstants.JCR_CONTENT);
        if(jcrContentNode.hasProperty("autotag")) {
            hm.put("autotag", jcrContentNode.getProperty("autotag").getBoolean());
        }
        if(jcrContentNode.hasProperty("dam:disableVideoSmartTag")){
            hm.put("dam:disableVideoSmartTag", jcrContentNode.getProperty("dam:disableVideoSmartTag").getBoolean());
        }
        if(jcrContentNode.hasProperty("dam:disableTextSmartTag")){
            hm.put("dam:disableTextSmartTag", jcrContentNode.getProperty("dam:disableTextSmartTag").getBoolean());
        }
        if(jcrContentNode.hasProperty("sourcing")) {
            hm.put("./jcr:content/sourcing", jcrContentNode.getProperty("sourcing").getBoolean());
        }
        if(jcrContentNode.hasNode("metadata")) {
            Node metadataChildNode = jcrContentNode.getNode("metadata");
            PropertyIterator props = metadataChildNode.getProperties();
            while(props.hasNext()) {
                Property prop = props.nextProperty();
                if(prop.isMultiple()) {
                	hm.put("./jcr:content/metadata/"+prop.getName(), prop.getValues());
                    
                } else {
                	writeSingleValue(prop, hm);
                    
                }
            }
        }

        if (jcrContentNode.hasProperty("policies/cfm/policy-cfm-allowedModelsByPaths")) {
            hm.put("policy-cfm-allowedModelsByPaths", jcrContentNode.getProperty("policies/cfm/policy-cfm-allowedModelsByPaths").getValues());
        }
        if (jcrContentNode.hasProperty("policies/cfm/policy-cfm-allowedModelsByTags")) {
            hm.put("policy-cfm-allowedModelsByTags", jcrContentNode.getProperty("policies/cfm/policy-cfm-allowedModelsByTags").getValues());
        }
    }

    NodeType[] mixinTypes = currentResNode.getMixinNodeTypes();
    for (int i=0; i < mixinTypes.length;i++){
        if("granite:AuthenticationRequired".equals(mixinTypes[i].getName())){
            hm.put("granite:AuthenticationRequired",true);
        }
    }


    ValueMap map = currentResource.getValueMap();
    String currentValue = (String) map.get("granite:loginPath");
    hm.put("granite:loginPath",currentValue);

}
ValueMap vm = new ValueMapDecorator(hm);
request.setAttribute("granite.ui.form.values", vm);
%>

<%!private void writeSingleValue(Property prop, HashMap<String, Object> hm)
                throws RepositoryException {
            try {
                switch(prop.getValue().getType()) {
                    case PropertyType.DATE:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getDate());
                        break;
                    case PropertyType.BINARY:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getBinary());
                        break;
                    case PropertyType.BOOLEAN:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getBoolean());
                        break;
                    case PropertyType.DECIMAL:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getDecimal());
                        break;
                    case PropertyType.DOUBLE:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getDouble());
                        break;
                    case PropertyType.LONG:
							          hm.put("./jcr:content/metadata/" + prop.getName(), prop.getLong());
                        break;
                    default:
                        hm.put("./jcr:content/metadata/" + prop.getName(), prop.getString());
                        break;
                    }
            } catch (RepositoryException ex) {
                log.error("Unable to update metadata properties", ex);
            }
        }%>