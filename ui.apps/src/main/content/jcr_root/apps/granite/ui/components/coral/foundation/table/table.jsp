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
%><%@ include file="/libs/granite/ui/components/coral/foundation/datasource/datasourceUtils.jsp" %><%
%><%@ page import="java.util.ArrayList,
                  java.util.Iterator,
                  org.apache.commons.collections4.iterators.ListIteratorWrapper,
                  org.apache.commons.lang3.StringUtils,
                  org.apache.sling.commons.json.io.JSONStringer,
                  org.slf4j.Logger,
                  org.slf4j.LoggerFactory,
                  com.adobe.granite.ui.components.AttrBuilder,
                  com.adobe.granite.ui.components.ComponentHelper.Options,
                  com.adobe.granite.ui.components.Config,
                  com.adobe.granite.ui.components.ds.DataSource,
                  com.adobe.granite.ui.components.Tag" %><%--###
Table
=====

.. granite:servercomponent:: /libs/granite/ui/components/coral/foundation/table

   A table component.

   It implements :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/selections`
   and :doc:`/jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/mode` vocabulary.

   Note that this component's size is maximized to its parent OOTB. Hence it doesn't make sense if this component has a sibling component,
   as it will take the whole parent's space.

   It has the following content structure:

   .. gnd:gnd::

      [granite:Table] > granite:commonAttrs, granite:renderCondition, granite:container

      /**
       * The mode of item to be selected:
       *
       * none
       *    The selection is disabled
       * row
       *    Each row is selectable and represents one selection
       */
      - selectionMode (String) = 'none' < 'none', 'row'

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
       * The URI Template that is returning the HTML response of the table.
       * It is used when the client needs to load data dynamically, such as for sorting, pagination.
       *
       * If it is not specified, the feature is disabled.
       *
       * It supports the following variables:
       *
       * offset
       *    The item offset of the current request.
       * limit
       *    The item limit of the pagination.
       * size
       *    The actual number items to be included(excluding placeholder)
       * id
       *    The id of the collection (:doc:`[data-foundation-collection-id] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`).
       * sortName
       *    The name of sorted column.
       *    This is optional variable that may not be passed when resolving the URI Template.
       * sortDir
       *    The direction of the sorting: ``asc`` or ``desc``.
       *    This is optional variable that may not be passed when resolving the URI Template.
       *
       * e.g. ``/a/b/c{.offset,limit,size}.html{+id}{?sortName,sortDir}``
       */
      - src (StringEL)

      /**
       * The path of the current collection.
       * It will act as the value of :doc:`[data-foundation-collection-id] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`.
       *
       * e.g. ``${requestPathInfo.suffix}``
       */
      - path (StringEL)

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
       * The sorting mode of the column.
       *
       * To enable sorting, the column needs to be configured accordingly.
       *
       * local
       *    The sorting is done locally.
       * remote
       *    The sorting is done remotely.
       *    The new data are fetched remotely as defined by ``src`` property.
       */
      - sortMode (String) = 'local' < 'local', 'remote'

      /**
       * Indicates if the table row is orderable.
       */
      - orderable (BooleanEL)

      /**
       * The URI Template handling the POST request to reorder the item.
       *
       * When it is set, "orderable" is defaulted to true. Note that it means that if "orderable" is explicitly as "false", then even though "rowReorderAction" is set, the ordering feature is still disabled.
       *
       * It supports the following variables:
       *
       * item
       *    The id of the reordered item. (:doc:`[data-foundation-collection-item-id] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`)
       * before
       *    The id of the reference item. The item is ordered before this reference item. (:doc:`[data-foundation-collection-item-id] </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`)
       * beforeName
       *    The name of the before item: `var beforeName = before.substring(before.lastIndexOf("/") + 1);`.
       *
       *    This is meant for convenience for Sling POST Servlet's ``:order`` parameter, which requires the node name (instead of path).
       *
       * The final URL (after URI Template expansion) is converted to ``application/x-www-form-urlencoded`` content type.
       * For example if we have URI Template of ``/reorder{?item,before}`` which is expanded into ``/reorder?item=%2Fcontent%2Fde&before=%2Fcontent%2Fen``,
       * the POST would be against ``/reorder`` with ``item=%2Fcontent%2Fde&before=%2Fcontent%2Fen`` as the request body.
       */
      - rowReorderAction (StringEL)

      /**
       * This property is the equivalence of ``rowReorderAction`` for absolute path.
       *
       * For example if your template is ``{+item}?order{&before}``, since it is not starting with "/",
       * the server is unable to know if it is an absolute path.
       * So use this property if you want to add the context path regardless.
       */
      - 'rowReorderAction.abs' (StringEL)

      /**
       * The path to the resource to render :doc:`.foundation-collection-meta </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`.
       * If not specified, the resource specified at ``path`` will be used instead.
       *
       * The resource will be included using resource type specified at ``metaResourceType``, where it can be processed accordingly.
       */
      - metaPath (StringEL)

      /**
       * The resource type to render :doc:`.foundation-collection-meta </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>`.
       *
       * The resource specified at ``metaPath`` or ``path`` will be included using this resource type, where it can be processed accordingly.
       *
       * This property is mandatory to specified if ``.foundation-collection-meta`` needs to be generated.
       */
      - metaResourceType (String)

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

      /**
       * ``false`` to show the empty message (or render the empty row using ``emptyrow`` subresource) when there is no item; ``true`` to skip showing that message.
       */
      - skipEmptyRow (Boolean)

      /**
       * The columns configuration.
       * Use this instead of ``columnsdatasource`` to configure the columns literally in the content.
       *
       * Each subresource of ``columns`` needs to satisfy ``[granite:TableColumn]``.
       */
      + columns

      /**
       * The datasource for the columns configuration.
       * Use this instead of ``columns`` to programmatically generate the columns.
       *
       * It needs to return the resources satisfying ``[granite:TableColumn]``.
       */
      + columnsdatasource

      /**
       * The component to render the row when there is no item.
       */
      + emptyrow


      [granite:TableColumn]

      /**
       * The name of the column. This name is used as an ID to indicate the column.
       * For example it will be used during server-side sorting.
       *
       * If the property is not specified, the resource name is used instead.
       */
      - name (String)

      /**
       * Indicates if the column is a column to select the item.
       */
      - select (Boolean)

      /**
       * Indicates if the column is a column to order the item.
       */
      - order (Boolean)

      /**
       * The text of the column.
       */
      - jcr:title (String) i18n

      /**
       * The alignment of the column.
       */
      - alignment (String) = 'left' < 'left', 'right', 'center'

      /**
       * Indicates if the column has fixed width.
       */
      - fixedWidth (Boolean)

      /**
       * Allow the column to be reordered visually by the user.
       */
      - draggable (Boolean)

      /**
       * Indicates if the column is hidden.
       */
      - hidden (Boolean)

      /**
       * Indicates if the column is sortable.
       */
      - sortable (Boolean)

      /**
       * The type of the cell value of the column for the purpose of sorting.
       *
       * The cell value is taken from ``value`` attribute of the cell, with the cell content as fallback.
       *
       * alphanumeric
       *    The value is treated as string.
       * number
       *    The value is treated as number.
       * date
       *    The value is treated as date.
       *    The ``value`` attribute needs to store the date integer since epoch.
       * custom
       *    No actual sorting is performed. Rather custom code needs to sort it.
       */
      - sortType (String) = 'alphanumeric' < 'alphanumeric', 'number', 'date', 'custom'


      [granite:TableDatasource]

      /**
       * The value on which layout needs to be sorted..
       */
      - sortName (StringEL)

      /**
       * The direction of the sorting: ``asc`` or ``desc``
       *
       * asc
       *    layout items to be sorted in ascending order
       * desc
       *    layout items to be sorted in descending order
       */
      - sortDir (StringEL) = "asc" < "asc", "desc"


   **Item Markup**

   Each item (the row) needs to render the markup of ``Coral.Table.Row``:

   .. code-block:: html

      <tr is="coral-table-row">
        <td is="coral-table-cell" coral-table-rowselect></td>
        <td is="coral-table-cell">Text</td>
        <td is="coral-table-cell">Text 2</td>
        <td is="coral-table-cell" coral-table-roworder></td>
      </tr>

   Each cell in the row can be any arbitrary content.

   To indicate a cell for selection (to select the row), ``coral-table-rowselect`` attribute needs to be specified at that cell element.
   You can also put arbitrary content in that cell (e.g. a thumbnail). If no content at all, then a checkbox is automatically rendered.

   For the thumbnail (as the content of the selection cell), it is recommended to use 48x48px dimensions.
   e.g. "/content/geometrixx_mobile.thumb.48.48.png?ck=1447060146"

   To indicate a cell for row ordering, ``coral-table-roworder`` attribute needs to be specified at that cell element.

   For navigation when clicking the row, :doc:`.foundation-collection-navigator </jcr_root/libs/granite/ui/components/coral/foundation/clientlibs/foundation/vocabulary/collection>` can be specified at ``<tr>``.
   It supports navigating to arbitrary location (just like ``<a>``), or navigating to the collection item.


   **Example**

   .. code-block:: none

      + mytable
        - sling:resourceType = "granite/ui/components/coral/foundation/table"
        - src = "/a/b/c{.offset,limit,size}.html{+id}{?sortName,sortDir}"
        - rowReorderAction = "/reorder{?item,before}"
        + columns
          + select
            - select = true
          + col1
            - jcr:title = "Column 1"
          + col2
            - jcr:title = "Column 2"
          + order
            - order = true
        + datasource
          - sling:resourceType = "my/datasource"
###--%><%
    final long DEFAULT_PAGINATION_LIMIT = 100;

    final Logger logger = LoggerFactory.getLogger("libs.granite.ui.components.coral.foundation.table");

    if (!cmp.getRenderCondition(resource, false).check()) {
        return;
    }

    Config cfg = cmp.getConfig();
    ExpressionHelper ex = cmp.getExpressionHelper();

    String src = StringUtils.trimToNull(ex.getString(cfg.get("src", String.class)));
    String absSrc = StringUtils.trimToNull(ex.getString(cfg.get("src.abs", String.class)));

    src = handleURITemplate(src, absSrc, request);

    String rowReorderAction = StringUtils.trimToNull(ex.getString(cfg.get("rowReorderAction", String.class)));
    String absRowReorderAction = StringUtils.trimToNull(ex.getString(cfg.get("rowReorderAction.abs", String.class)));

    rowReorderAction = handleURITemplate(rowReorderAction, absRowReorderAction, request);

    boolean orderable = rowReorderAction != null ? true : false;

    String layoutName = "foundation-layout-table";
    String path = StringUtils.trimToNull(ex.getString(cfg.get("path", String.class)));
    String selectionCount = ex.getString(cfg.get("selectionCount", "multiple"));
    String itemResourceType = cfg.get("itemResourceType", String.class);

    Resource datasource = resource.getChild("datasource");
    Integer size = ex.get(cfg.get("size", String.class), Integer.class);
    Long limit = ex.get(cfg.get("limit", String.class), Long.class);
    long offset = datasource != null ? ex.get(datasource.getValueMap().get("offset", "0"), long.class) : 0;


//Below statement for BC
    long totalSize = limit != null && limit >= DEFAULT_PAGINATION_LIMIT ? limit : DEFAULT_PAGINATION_LIMIT;
    totalSize = size != null && size >= totalSize ? size : totalSize;

    String sortBy = datasource != null ? StringUtils.trimToNull(ex.getString(datasource.getValueMap()
            .get("sortName", String.class))) : null;
    String sortOrder = datasource != null ? StringUtils.trimToNull(ex.getString(datasource.getValueMap()
            .get("sortDir", String.class))) : null;

// For backward compatibility
    sortBy = sortBy != null ? sortBy : request.getParameter("sortName");
    sortOrder = sortOrder!= null ? sortOrder : request.getParameter("sortDir");

    DataSource ds;
    if (size == null || size < 20 || size >= totalSize || datasource == null) {
        ds = cmp.getItemDataSource();
        if (size != null) {
            totalSize = size;
        }
    } else {
        try {
            Resource datasourceWrapper = new LimitIncreaseDatasourceWrapper(datasource, ex, totalSize - size + 1);
            Resource resourceWrapper = new DatasourceOverrideWrapper(resource, datasourceWrapper);
            ds = cmp.asDataSource(datasourceWrapper, resourceWrapper);
        } catch (Exception e) {
            logger.debug("Failed to wrap datasource for lookahead {}", e);
            logger.info("Fallback to non-lookahead datasource");
            ds = cmp.getItemDataSource();
            if (size != null) {
                totalSize = size;
            }
        }
    }

    Iterator<Resource> items = ds.iterator();
    Long guessTotal = ds.getGuessTotal();
    Boolean hasMore = null;

    if (size != null) {
        ArrayList<Resource> list = new ArrayList<Resource>();

        while (items.hasNext() && list.size() < totalSize) {
            list.add(items.next());
        }

        hasMore = items.hasNext();
        items = list.iterator();
    }

    boolean isEmpty = !items.hasNext();
    boolean showEmptyRow = isEmpty && !cfg.get("skipEmptyRow", false);
    Resource emptyRow = showEmptyRow ? resource.getChild("emptyrow") : null;

    Tag tag = cmp.consumeTag();
    AttrBuilder attrs = tag.getAttrs();
    cmp.populateCommonAttrs(attrs);

    attrs.addClass("foundation-layout-util-maximized-alt");

    attrs.addClass("foundation-collection");
    attrs.add("data-foundation-collection-id", path);
    attrs.add("data-foundation-collection-src", src);
    attrs.add("data-foundation-selections-mode", selectionCount);
    attrs.add("data-foundation-mode-group", cfg.get("modeGroup", String.class));
    attrs.add("data-foundation-collection-sortby", sortBy);
    attrs.add("data-foundation-collection-sortorder", sortOrder);

    String layoutJson = new JSONStringer()
            .object()
            .key("name").value(layoutName)
            .key("limit").value(limit != null ? limit : 40)
            .key("size").value(size)
            .key("sortMode").value(cfg.get("sortMode", String.class))
            .key("rowReorderAction").value(rowReorderAction)
            .key("layoutId").value(resource.getName()) // This is used as an id to identify the layout when there are multiple layouts to represent the same collection.
            .key("trackingFeature").value(cfg.get("trackingFeature", String.class))
            .key("trackingElement").value(cfg.get("trackingElement", String.class))
            .endObject()
            .toString();

    attrs.addClass(layoutName);
    attrs.add("data-foundation-layout-table-hasmore", hasMore);
    attrs.add("data-foundation-layout-table-guesstotal", guessTotal == null ? null : guessTotal.toString());
    attrs.add("data-foundation-layout", layoutJson);

    if (!isEmpty || cfg.get("skipEmptyRow", false)) {
        attrs.addBoolean("selectable", !cfg.get("selectionMode", "none").equals("none"));
        attrs.addBoolean("multiple", "multiple".equals(selectionCount));
        attrs.addBoolean("orderable", cfg.get("orderable", orderable));
    }

%><table is="coral-table" <%= attrs %>><%
    String metaRT = cfg.get("metaResourceType", String.class);
    if (metaRT != null) {
        String metaPath = StringUtils.trimToNull(ex.getString(cfg.get("metaPath", String.class)));
        if (metaPath == null) {
            metaPath = path;
        }
%><caption hidden><sling:include path="<%= metaPath %>" resourceType="<%= metaRT %>" /></caption><%
    }

    Iterator<Resource> columnsIterator = null;

    Resource columns = resource.getChild("columns");
    if (columns != null) {
        columnsIterator = columns.listChildren();
    } else {
        Resource columnsDataSource = resource.getChild("columnsdatasource");
        if (columnsDataSource != null) {
            columnsIterator = cmp.asDataSource(columnsDataSource).iterator();
        }
    }

    int columnCount = 0;

    if (columnsIterator != null) {
        ListIteratorWrapper<Resource> columnsIt = new ListIteratorWrapper<>(columnsIterator);

%><colgroup><%
    while (columnsIt.hasNext()) {
        columnCount++;

        Resource column = (Resource) columnsIt.next();
        Config columnCfg = new Config(column);

        AttrBuilder columnAttrs = new AttrBuilder(request, xssAPI);
        columnAttrs.add("is", "coral-table-column");

        if (columnCfg.get("select", false)) {
            columnAttrs.add("alignment", "center");
            columnAttrs.addBoolean("fixedwidth", true);
        } else if (columnCfg.get("order", false)) {
            columnAttrs.add("alignment", "center");
            columnAttrs.addBoolean("fixedwidth", true);
        } else {
            columnAttrs.add("alignment", columnCfg.get("alignment", String.class));
            columnAttrs.addBoolean("fixedwidth", columnCfg.get("fixedWidth", false));
            columnAttrs.addBoolean("orderable", columnCfg.get("draggable", false));
            columnAttrs.addBoolean("hidden", columnCfg.get("hidden", false));
            columnAttrs.addBoolean("sortable", columnCfg.get("sortable", false));
            columnAttrs.add("sortabletype", columnCfg.get("sortType", String.class));

            columnAttrs.add("data-foundation-layout-table-column-name", columnCfg.get("name", column.getName()));
        }

%><col <%= columnAttrs %>><%
    }
%></colgroup>
    <thead is="coral-table-head" sticky>
    <tr is="coral-table-row"><%
        columnsIt.reset();

        while (columnsIt.hasNext()) {
            Resource column = columnsIt.next();
            Config columnCfg = new Config(column);
            if (columnCfg.get("select", false)) {
                AttrBuilder checkboxAttrs = new AttrBuilder(request, xssAPI);
                checkboxAttrs.add("trackingelement", "table-select");
                checkboxAttrs.add("trackingfeature", cfg.get("trackingFeature", String.class));
    %><th is="coral-table-headercell">
        <coral-checkbox coral-table-select <%= checkboxAttrs %> labelled="<%= xssAPI.encodeForHTML(i18n.get("Select All")) %>"></coral-checkbox>
    </th><%
    } else {
    %><th is="coral-table-headercell"><%= outVar(xssAPI, i18n, columnCfg.get("jcr:title", "")) %></th><%
            }
        }
    %></tr>
    </thead><%
        }
        if (showEmptyRow && emptyRow != null) {
    %><tfoot is="coral-table-foot">
    <tr is="coral-table-foot">
        <td is="coral-table-cell" colspan="<%= columnCount %>"><sling:include resource="<%= emptyRow %>" /></td>
    </tr>
    </tfoot><%
    } else {
    %><tbody is="coral-table-body"><%
        for (long index = 0; items.hasNext(); index++) {
            Resource item = items.next();

            AttrBuilder itemAttrs = new AttrBuilder(request, xssAPI);
            itemAttrs.addClass("foundation-collection-item");
            itemAttrs.add("data-foundation-collection-item-id", item.getPath());
            itemAttrs.add("data-granite-collection-item-id", item.getPath());
            itemAttrs.add("data-datasource-index", "" + (index + offset));

            String itemPath = item.getPath();
            if(StringUtils.contains(itemPath, "/content/dam/medialibrary") && item.getChild("jcr:content") != null && item.getChild("jcr:content").getChild("metadata") != null) {
                ValueMap metadata = item.getChild("jcr:content").getChild("metadata").getValueMap();
                if( metadata.containsKey("bnpp-download-auth") && StringUtils.equals( metadata.get("bnpp-download-auth", "no"), "yes") ){
                  itemAttrs.add("bnpp-download-auth", "true");
                } else {
                  itemAttrs.add("bnpp-download-auth", "false");
                }
            }

            if (size != null && index >= size) {
                itemAttrs.addClass("is-lazyLoaded");

    %><tr is="coral-table-row" <%= itemAttrs %>>
        <td is="coral-table-cell" colspan="<%= columnCount %>" alignment="center">&nbsp;</td>
    </tr><%
            } else {
                cmp.include(item, itemResourceType, new Options().tag(new Tag(itemAttrs)));
            }
        }

        if (showEmptyRow && emptyRow == null) {
    %><tr is="coral-table-row">
        <td is="coral-table-cell" colspan="<%= columnCount %>" alignment="center"><%= xssAPI.encodeForHTML(i18n.get("There is no item.")) %></td>
    </tr><%
        }
    %></tbody>
    <tfoot class="granite-collection-loading-title" is="coral-table-foot">
    <tr class="granite-collection-loading-container" is="coral-table-row">
        <td is="coral-table-cell" colspan="<%= columnCount %>">
            <coral-wait class="granite-collection-loading-wait"></coral-wait>
            <span><%= xssAPI.encodeForHTML(i18n.get("Loading more items")) %></span>
        </td>
    </tr>
    </tfoot><%
        }
    %></table><%!

    private String handleURITemplate(String template, String absTemplate, HttpServletRequest request) {
        if (template != null && template.startsWith("/")) {
            template = request.getContextPath() + template;
        }

        if (template != null) {
            return template;
        }

        if (absTemplate != null) {
            return request.getContextPath() + absTemplate;
        }
        return null;
    }
%>

<%
if(StringUtils.contains(path, "/content/dam/medialibrary") ){
    %>
<script>
     $(".foundation-layout-panel-bodywrapper").css('background-color','#bfe4d6');
</script>
<%
}else{
    %>
<script>
 	 $(".foundation-layout-panel-bodywrapper").css('background-color','');
</script>
<%
}

%>
