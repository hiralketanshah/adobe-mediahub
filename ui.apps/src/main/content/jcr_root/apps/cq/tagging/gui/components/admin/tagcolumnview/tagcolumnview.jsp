<%--
  ADOBE CONFIDENTIAL

  Copyright 2015 Adobe Systems Incorporated
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
%><%@include file="/libs/granite/ui/global.jsp"%><%
%><%@page session="false"
          import="java.util.ArrayList,
      			  java.util.Comparator,
                  java.util.Iterator,
                  javax.servlet.http.HttpServletRequest,
                  org.apache.jackrabbit.util.Text,
                  org.apache.sling.commons.json.io.JSONStringer,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ExpressionHelper,
                  com.adobe.granite.ui.components.Tag,
                  com.day.cq.tagging.TagManager,
                  com.adobe.granite.ui.components.ComponentHelper.Options,
                  com.adobe.granite.ui.components.ds.DataSource"%><%--###
ColumnView
==========

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/columnview

   A Miller columns component.
   
   It implements :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/selections`
   and :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/mode` vocabulary.
   
   It has the following content structure:

   .. gnd:gnd::

      [granite:ColumnView] > granite:commonAttrs, granite:renderCondition, granite:container
      
      /**
       * ``true`` to enable the selection of item; ``false`` otherwise.
       */
      - selectionMode (BooleanEL) = 'true'
      
      /**
       * The count of item to be selected (when the ``selectionMode`` is enabled):
       *
       * single
       *    Only maximum single selection allowed
       * multiple
       *    Zero or more selection allowed
       */
      - selectionCount (StringEL) = 'multiple'
      
      /**
       * The URI Template that is returning the HTML response of the column view.
       * It is used when the client needs to load data dynamically, such as for pagination.
       *
       * Note that only the items of the current column are rendered using the provided item datasource.
       * The items of the ancestors columns are loaded lazily in different requests.
       *
       * If it is not specified, the feature is disabled.
       *
       * It supports the following variables:
       *
       * It supports the following variables:
       *
       * offset
       *    The item offset of the current request.
       * limit
       *    The item limit of the pagination.
       * id
       *    The path of the target column.
       */
      - src (StringEL)
      
      /**
       * The path of the current column. It will act as the value of ``[data-foundation-collection-id]``.
       *
       * e.g. ``${requestPathInfo.suffix}``
       */
      - path (StringEL)
      
      /**
       * ``true`` to also load the ancestors of the current columns up to the ``rootPath``; ``false`` otherwise.
       */
      - loadAncestors (Boolean)

      /**
       * ``true`` to also render a special column for the root resource when ``path == rootPath``; ``false`` otherwise.
       * This special column is used to allow the user to select the root path.
       *
       * Say you set ``path = /``, this component by default will render a column containing the items of that path (e.g. ``/apps``, ``/etc``).
       * When this property is ``true``, when ``path == rootPath``, this component will also render a column containing a single item representing the root.
       * In other words it will render two columns; first, the special root column; second, the normal column.
       *
       * It can be illustrated with the following visualization::
       *
       *    When showRoot = false, path = /, rootPath = /:
       *
       *    +------+
       *    | apps |
       *    | etc  |
       *    | home |
       *    | libs |
       *    +------+
       *
       *    When showRoot = true, path = /, rootPath = /:
       *
       *    +-----+------+
       *    |  /  | apps |
       *    |     | etc  |
       *    |     | home |
       *    |     | libs |
       *    +-----+------+
       *
       * When rendering the root resource for the item in the special column, this component will also honor the ``itemResourceType`` property.
       */
      - showRoot (Boolean)

      /**
       * The path of the root column.
       */
      - rootPath (StringEL) = '/'
      
      /**
       * The resource type for each item of the column.
       */
      - itemResourceType (String)
      
      /**
       * The item limit of the pagination.
       */
      - limit (Long) = '40'
      
      /**
       * Indicates the size of the items from datasource to be included.
       * If this property is not specified, then all items from datasource will be included, and the next page will be fetched accordingly.
       *
       * This is meant to be a performance optimization to avoid fetching the next page unnecessarily.
       * For example given the fact that the ``size`` is set to 20, and the datasource is configured to fetch ``size + 1``, which is 21,
       * the implementation can check if the datasource actually has more item or not just by checking its size.
       *
       * .. warning:: When ``size`` is set, you have to make sure your datasource is configured to fetch more than the value of ``size``!
       * 
       * =========  ==============  =========
       * ``size``   Actual DS Size  Has More?
       * =========  ==============  =========
       * 20         < 20            ``false``
       * 20         = 20            ``false``
       * 20         > 20            ``true``
       * (not set)  n/a             ``true``
       * =========  ==============  =========
       */
      - size (IntegerEL)
      
      /**
       * The URI Template that is returning the HTML response for the preview of an item.
       *
       * It supports the following variables:
       *
       * id
       *    The id/path of the item.
       */
      - previewSrc (StringEL)
      
      /**
       * The resource type to render :doc:`.foundation-collection-meta </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`.
       *
       * The resource specified at ``path`` will be included using this resource type, where it can be processed accordingly.
       */
      - metaResourceType (String)
      
      /**
       * The value of :doc:`[data-foundation-mode-group] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/mode>`.
       */
      - modeGroup (String)
      
      /**
       * The value of :doc:`[data-foundation-mode-group] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/mode>`
       * this component participates.
       *
       * The component supports the ``default`` or ``selection`` mode.
       *
       * When there is a selection, it will trigger the ``foundation-mode-change`` event with mode = ``selection``,
       * while triggering the event with mode = ``default`` when there is no selection.
       *
       * When other component triggers the event with mode = ``default``, it will react by clearing the selection.  
       */
      - modeGroup (String)
      
   Example::
   
      + mycolumnview
        - sling:resourceType = "granite/ui/components/coral/foundation/columnview"
        - src = "/a/b/c{.offset,limit}.html{+id}"
        - previewSrc = "/a/b/c.preview.html{+id}"
        - path = "${requestPathInfo.suffix}"
        - loadAncestors = true
        - rootPath = "/content"
        + datasource
          - sling:resourceType = "my/datasource"
###--%><%

if (!cmp.getRenderCondition(resource, false).check()) {
    return;
}

Config cfg = cmp.getConfig();
ExpressionHelper ex = cmp.getExpressionHelper();

String src = ex.getString(cfg.get("src", String.class));
if (src != null && src.startsWith("/")) {
    src = request.getContextPath() + src;
}

String previewSrc = ex.getString(cfg.get("previewSrc", String.class));
if (previewSrc != null && previewSrc.startsWith("/")) {
    previewSrc = request.getContextPath() + previewSrc;
}

String layoutName = "foundation-layout-columnview";
String path = ex.getString(cfg.get("path", String.class));
String rootPath = ex.getString(cfg.get("rootPath", "/"));
Integer size = ex.get(cfg.get("size", String.class), Integer.class);
boolean isSelectionMode = ex.getBoolean(cfg.get("selectionMode", "true"));
String selectionCount = ex.getString(cfg.get("selectionCount", "multiple"));
String itemResourceType = cfg.get("itemResourceType");

if(rootPath.isEmpty()){
    rootPath = "/";
}

if(path == null || path.isEmpty()){
    path = rootPath;
}

TagManager tagMgr = resourceResolver.adaptTo(TagManager.class);

rootPath = tagMgr.resolve(rootPath).getPath();
path = tagMgr.resolve(path).getPath();

final Resource rootRes = resourceResolver.getResource(rootPath);
final Resource currentRes = resourceResolver.getResource(path);

Iterator<Resource> items = cmp.getItemDataSource().iterator();

Boolean hasMore = null;

if (size != null) {
    ArrayList<Resource> list = new ArrayList<Resource>();
    
    while (items.hasNext() && list.size() < size) {
        list.add(items.next());
    }

    list.sort(Comparator.comparing(Resource::getName, (v1, v2) -> {
        return v1.compareToIgnoreCase(v2);
    }));
    
    hasMore = items.hasNext();
    items = list.iterator();
}

Tag tag = cmp.consumeTag();
AttrBuilder attrs = tag.getAttrs();
cmp.populateCommonAttrs(attrs);

attrs.addClass("foundation-collection");
attrs.add("data-foundation-collection-id", path);
attrs.add("data-foundation-collection-src", src);
attrs.add("data-foundation-selections-mode", selectionCount);
attrs.add("data-foundation-mode-group", cfg.get("modeGroup", String.class));

String layoutJson = new JSONStringer()
    .object()
        .key("name").value(layoutName)
        .key("limit").value(cfg.get("limit", 40))
        .key("previewSrc").value(previewSrc)
        .key("layoutId").value(resource.getName()) // This is used as an id to identify the layout when there are multiple layouts to represent the same collection.
    .endObject()
    .toString();

attrs.addClass(layoutName);
attrs.add("data-foundation-layout", layoutJson);

attrs.add("selectionmode", isSelectionMode ? selectionCount : "none");

%><coral-columnview <%= attrs %>><%

    if (cfg.get("loadAncestors", false)) {
        ArrayList<Resource> ancestors = getAncestors(currentRes, rootRes);
        for (int i = 0; i < ancestors.size(); i++) {
            Resource r = ancestors.get(i);
            
            String activeId;
            if (i < ancestors.size() - 1) {
                activeId = ancestors.get(i + 1).getPath();
            } else {
                activeId = path;
            }
            
            AttrBuilder parentColumnAttrs = new AttrBuilder(request, xssAPI);
            parentColumnAttrs.add("data-foundation-layout-columnview-columnid", r.getPath());
            parentColumnAttrs.add("data-foundation-layout-columnview-lazy", true);
            parentColumnAttrs.add("data-foundation-layout-columnview-activeitem", activeId);
    
            %><coral-columnview-column <%= parentColumnAttrs %>>
                <coral-columnview-column-content>
                    <coral-wait size="L" centered></coral-wait>
                </coral-columnview-column-content>
            </coral-columnview-column><%
        }
    }

    if (cfg.get("showRoot", false) && path.equals(rootPath)) {
        // Put a special path for columnId to avoid having the same columnId with the next column to avoid breaking the contract of columnId.
        // The contract of columnId is that it should be a path of the current column, i.e. the path should be a path representing a parent.
        // e.g. When columnId = "/", then the column will show the children of this path, such as "/a", "/b".
        // So for showRoot scenario, if we want to show the item with path = "/", we need to generate the column having a columnId with value of the parent of "/".
        // Since the cannot have a parent of "/", then we decide to just use a special convention ("parentof:<path>") to indicate this.
        // Other component (e.g. `.granite-collection-navigator`) reading the columnId can then understand this convention and handle it accordingly.
        String columnId = "parentof:" + path;
        
        AttrBuilder rootColumnAttrs = new AttrBuilder(request, xssAPI);
        rootColumnAttrs.add("data-foundation-layout-columnview-columnid", columnId);
        rootColumnAttrs.add("data-foundation-layout-columnview-hasmore", false);

        AttrBuilder rootItemAttrs = new AttrBuilder(request, xssAPI);
        rootItemAttrs.addClass("foundation-collection-item");
        rootItemAttrs.add("data-foundation-collection-item-id", rootPath);
        rootItemAttrs.addBoolean("active", true);
        
        %><coral-columnview-column <%= rootColumnAttrs %>>
            <coral-columnview-column-content>
                <% cmp.include(currentRes, itemResourceType, new Options().tag(new Tag(rootItemAttrs))); %>
            </coral-columnview-column-content>
        </coral-columnview-column><%
    }
    
    AttrBuilder columnAttrs = new AttrBuilder(request, xssAPI);
    columnAttrs.add("data-foundation-layout-columnview-columnid", currentRes.getPath());
    columnAttrs.add("data-foundation-layout-columnview-hasmore", hasMore);

    %><coral-columnview-column <%= columnAttrs %>><coral-columnview-column-content><%
        while (items.hasNext()) {
            Resource item = items.next();
    
            AttrBuilder itemAttrs = new AttrBuilder(request, xssAPI);
            itemAttrs.addClass("foundation-collection-item");
            itemAttrs.add("data-foundation-collection-item-id", item.getPath());
    
            cmp.include(item, itemResourceType, new Options().tag(new Tag(itemAttrs)));
        }
    
        // Put meta element here instead of under <coral-columnview-column>,
        // as somehow Coral is moving all the elements under <coral-columnview-column> to be under <coral-columnview-column-content>
        // even though <coral-columnview-column-content> is already given. 
        String metaRT = cfg.get("metaResourceType", String.class);
        if (metaRT != null) {
            %><sling:include resource="<%= currentRes %>" resourceType="<%= metaRT %>" /><%
        }
    %></coral-columnview-column-content></coral-columnview-column></coral-columnview><%!

    /**
     * Returns the ancestors of the current resource (inclusive) up to the root.
     * The result is ordered with the root as the first item.
     */
    private ArrayList<Resource> getAncestors(Resource current, Resource root) {
        ArrayList<Resource> results = new ArrayList<Resource>();
        
        if (current == null || root == null || current.getPath().equals(root.getPath())) {
            return results;
        }
    
        Resource r = current.getParent();

        while (r != null) {
            results.add(0, r);

            if (r.getPath().equals(root.getPath())) {
                break;
            }

            r = r.getParent();
        }
        
        return results;
    }
%>
