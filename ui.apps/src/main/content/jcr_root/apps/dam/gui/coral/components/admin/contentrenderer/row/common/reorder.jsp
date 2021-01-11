<%@include file="/libs/granite/ui/global.jsp"%>
<%@ page import="org.apache.sling.api.resource.ValueMap" %>
<%@ page import="org.apache.sling.api.resource.Resource" %>
<%@taglib prefix="cq" uri="http://www.day.com/taglibs/cq/1.0"%>
 
<%
    final String ASSET_RES_TYPE = "dam/gui/coral/components/admin/contentrenderer/row/asset";
 
    Resource assetResource = resource;
    String downloadauthorisation = "";
    String broadcaststatus = "";
    if(assetResource.getResourceType().equals(ASSET_RES_TYPE)){
        ValueMap vm = assetResource.getChild("jcr:content/metadata").getValueMap();
 
        downloadauthorisation = (String)vm.get("bnpp-download-auth", "");
        String[] statuses = (String[])vm.get("bnpp-broadcast-status");

          if(statuses != null){
            for(String value : statuses){
              broadcaststatus += value + " ";
            }
          }
    }

%>
<td is="coral-table-cell" value="<%= downloadauthorisation %>">
    <%= downloadauthorisation %>
</td>
 
<td is="coral-table-cell" value="<%= broadcaststatus %>">
    <%= broadcaststatus %>
</td>


<cq:include script = "/libs/dam/gui/coral/components/admin/contentrenderer/row/common/reorder.jsp"/>