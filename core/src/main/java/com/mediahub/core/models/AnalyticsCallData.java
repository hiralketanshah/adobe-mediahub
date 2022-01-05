package com.mediahub.core.models;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public class AnalyticsCallData {

    private static final String STATISTICS_ENTRY = "statistics";
    private static final String FUNCTIONS_ENTRY = "functions";
    private static final String COL_MIN = "col-min";
    private static final String COL_MAX = "col-max";
    private static final String SETTINGS_ENTRY = "settings";
    private static final String NONES_BEHAVIOR_ENTRY = "nonesBehavior";
    private static final String DIMENSION_SORT_ENTRY = "dimensionSort";
    private static final String PAGE_ENTRY = "page";
    private static final String LIMIT_ENTRY = "limit";
    private static final String COUNT_REPEAT_INSTANCES_ENTRY = "countRepeatInstances";
    private static final String SEARCH_ENTRY = "search";
    private static final String VARIABLES_PREFIX = "variables/";
    private static final String DIMENSION_ENTRY = "dimension";
    private static final String RSID_ENTRY = "rsid";
    private static final String GLOBAL_FILTERS_ENTRY = "globalFilters";
    private static final String METRIC_CONTAINER_ENTRY = "metricContainer";
    private static final String METRIC_FILTERS_ENTRY = "metricFilters";
    private static final String METRICS_ENTRY = "metrics";
    private static final String ITEM_ID_ENTRY = "itemId";
    private static final String TYPE_ENTRY = "type";
    private static final String ID_ENTRY = "id";
    private static final String FILTERS_ENTRY = "filters";
    private static final String SORT_ENTRY = "sort";
    private static final String COLUMN_ID_ENTRY = "columnId";

    private static final String JSON_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";
    private DateFormat jsonDateFormat = new SimpleDateFormat(JSON_DATE_FORMAT);

    public enum Sort {
        asc,
        desc
    }

    public enum FilterType {
        dateRange,
        breakdown,
        other
    }

    private String rsId;

    private List<AnalyticsCallFilter> globalFilters;

    private List<AnalyticsCallMetric> metrics;

    private List<AnalyticsCallFilter> metricFilters;

    private String dimension;

    private Sort dimensionSort;

    private Long limit;

    private String nonesBehavior;

    private AnalyticsCallSearch search;

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallMetric> metrics, List<AnalyticsCallFilter> metricFilters, List<AnalyticsCallFilter> globalFilters, AnalyticsCallSearch search, Sort dimensionSort, Long limit, String nonesBehavior) {
        this.rsId = rsId;
        this.dimension = dimension;
        this.metrics = new ArrayList<>(metrics);
        this.metricFilters = new ArrayList<>(metricFilters);
        this.globalFilters = new ArrayList<>(globalFilters);
        this.search = search;
        this.dimensionSort = dimensionSort;
        this.limit = limit;
        this.nonesBehavior = nonesBehavior;
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallMetric> metrics, List<AnalyticsCallFilter> metricFilters, List<AnalyticsCallFilter> globalFilters, AnalyticsCallSearch search, Sort dimensionSort, Long limit) {
        this(rsId, dimension, metrics, metricFilters, globalFilters, search, dimensionSort, limit, "return-nones");
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallMetric> metrics, List<AnalyticsCallFilter> metricFilters, List<AnalyticsCallFilter> globalFilters, AnalyticsCallSearch search, Sort dimensionSort, String nonesBehavior) {
        this(rsId, dimension, metrics, metricFilters, globalFilters, search, dimensionSort, 400L, nonesBehavior);
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallMetric> metrics, List<AnalyticsCallFilter> metricFilters, List<AnalyticsCallFilter> globalFilters, AnalyticsCallSearch search, Sort dimensionSort) {
        this(rsId, dimension, metrics, metricFilters, globalFilters, search, dimensionSort, 400L, "return-nones");
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallFilter> globalFilters, List<AnalyticsCallMetric> metrics, List<AnalyticsCallFilter> metricFilters) {
        this(rsId, dimension, metrics, metricFilters, globalFilters, null, null);
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallFilter> globalFilters, List<AnalyticsCallMetric> metrics) {
        this(rsId, dimension, globalFilters, metrics, null);
    }

    public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallFilter> globalFilters) {
        this(rsId, dimension, globalFilters, null);
    }

    public AnalyticsCallData(String rsId, String dimension) {
        this(rsId, dimension, null);
    }

    public AnalyticsCallData(String rsId, String dimension, Date startDate, Date endDate) {
        this(rsId, dimension);

        AnalyticsCallFilter globalFilter = new AnalyticsCallFilter("0", FilterType.dateRange, jsonDateFormat.format(startDate) + "/" + jsonDateFormat.format(endDate));

        this.globalFilters = Collections.singletonList(globalFilter);
    }

    public String getRsId() {
        return rsId;
    }

    public List<AnalyticsCallFilter> getGlobalFilters() {
        return globalFilters;
    }

    public List<AnalyticsCallMetric> getMetrics() {
        return metrics;
    }

    public List<AnalyticsCallFilter> getMetricsFilters() {
        return metricFilters;
    }

    public String getDimension() {
        return dimension;
    }

    public Long getLimit() {
        return limit;
    }

    public String getNonesBehavior() {
        return nonesBehavior;
    }

    public Sort getDimensionSort() {
        return dimensionSort;
    }

    public AnalyticsCallSearch getSearch() {
        return search;
    }

    public JsonObject toJson() {
        JsonObject data = new JsonObject();

        //rsid
        data.addProperty(RSID_ENTRY, getRsId());

        //globalFilters
        if (getGlobalFilters() != null) {
            JsonArray globalFiltersArray = new JsonArray();
            for (AnalyticsCallFilter globalFilter : getGlobalFilters()) {
                globalFiltersArray.add(globalFilter.toJson());
            }
            data.add(GLOBAL_FILTERS_ENTRY, globalFiltersArray);
        }

        //metricContainer
        JsonObject metricContainer = new JsonObject();

        JsonArray metricsArray = new JsonArray();
        for (AnalyticsCallMetric metric : getMetrics()) {
            metricsArray.add(metric.toJson());
        }
        metricContainer.add(METRICS_ENTRY, metricsArray);

        if (getMetricsFilters() != null) {
            JsonArray metricFiltersArray = new JsonArray();
            for (AnalyticsCallFilter metricFilter : getMetricsFilters()) {
                metricFiltersArray.add(metricFilter.toJson());
            }
            metricContainer.add(METRIC_FILTERS_ENTRY, metricFiltersArray);
        }

        data.add(METRIC_CONTAINER_ENTRY, metricContainer);

        //dimension
        data.addProperty(DIMENSION_ENTRY, VARIABLES_PREFIX + getDimension());

        //search
        if (getSearch() != null) {
            data.add(SEARCH_ENTRY, getSearch().toJson());
        }

        //settings
        JsonObject settings = new JsonObject();
        settings.addProperty(COUNT_REPEAT_INSTANCES_ENTRY, true);
        settings.addProperty(LIMIT_ENTRY, getLimit());
        settings.addProperty(PAGE_ENTRY, 0);
        if (getDimensionSort() != null) settings.addProperty(DIMENSION_SORT_ENTRY, getDimensionSort().toString());
        settings.addProperty(NONES_BEHAVIOR_ENTRY, getNonesBehavior());

        data.add(SETTINGS_ENTRY, settings);

        //statistics
        JsonObject statistics = new JsonObject();
        JsonArray functionsArray = new JsonArray();
        functionsArray.add(COL_MAX);
        functionsArray.add(COL_MIN);

        statistics.add(FUNCTIONS_ENTRY, functionsArray);

        data.add(STATISTICS_ENTRY, statistics);

        return data;
    }

    public static class AnalyticsCallSearch {

        private String clause;

        private String byField;

        private String[] fieldTerms;

        public AnalyticsCallSearch(String clause) {
            this.clause = clause;
            this.byField = null;
            this.fieldTerms = null;
        }

        public AnalyticsCallSearch(String byField, String... fieldTerms) {
            this.clause = null;
            this.byField = byField;
            this.fieldTerms = fieldTerms.clone();
        }

        public String getClause() {
            return clause;
        }

        public String getByField() {
            return byField;
        }

        public String[] getFieldTerms() {
            return fieldTerms;
        }

        public JsonObject toJson() {
            JsonObject data = new JsonObject();

            if (getClause() != null) {
                data.addProperty("clause", getClause());
            } else if (getByField() != null) {
                JsonArray termsArray = new JsonArray();
                for (String term : getFieldTerms()) {
                    termsArray.add(term);
                }
                data.add(getByField(), termsArray);
            }

            return data;
        }
    }

    public static class AnalyticsCallFilter {

        private String id;

        private FilterType type;

        private String typeValue;

        private String dimension;

        private String itemId;

        public AnalyticsCallFilter(String id, FilterType type, String typeValue) {
            this.id = id;
            this.type = type;
            this.typeValue = typeValue;
        }

        public AnalyticsCallFilter(FilterType type, String typeValue) {
            this(null, type, typeValue);
        }

        public AnalyticsCallFilter(String id, FilterType type, String dimension, String itemId) {
            this.id = id;
            this.type = type;
            this.dimension = dimension;
            this.itemId = itemId;
        }

        public String getId() {
            return id;
        }

        public FilterType getType() {
            return type;
        }

        public String getTypeValue() {
            return typeValue;
        }

        public String getDimension() {
            return dimension;
        }

        public String getItemId() {
            return itemId;
        }

        public JsonObject toJson() {
            JsonObject data = new JsonObject();

            if (getId() != null) data.addProperty(ID_ENTRY, getId());
            data.addProperty(TYPE_ENTRY, getType().toString());
            if (getTypeValue() != null) data.addProperty(getType().toString(), getTypeValue());
            if (getDimension() != null) data.addProperty(DIMENSION_ENTRY, VARIABLES_PREFIX + getDimension());
            if (getItemId() != null) data.addProperty(ITEM_ID_ENTRY, getItemId());

            return data;
        }
    }

    public static class AnalyticsCallMetric {

        private String id;

        private String columnId;

        private Sort sort;

        private List<String> filterIds;

        public AnalyticsCallMetric(String id, String columnId, Sort sort, List<String> filterIds) {
            this.id = id;
            this.columnId = columnId;
            this.sort = sort;
            this.filterIds = filterIds;
        }

        public AnalyticsCallMetric(String id, String columnId, Sort sort) {
            this(id, columnId, sort, null);
        }

        public AnalyticsCallMetric(String id, String columnId, List<String> filterIds) {
            this(id, columnId, null, filterIds);
        }

        public AnalyticsCallMetric(String id, String columnId) {
            this(id, columnId, null, null);
        }

        public String getId() {
            return id;
        }

        public String getColumnId() {
            return columnId;
        }

        public Sort getSort() {
            return sort;
        }

        public List<String> getFilterIds() {
            return filterIds;
        }

        public JsonObject toJson() {
            JsonObject data = new JsonObject();

            data.addProperty(COLUMN_ID_ENTRY, getColumnId());
            data.addProperty(ID_ENTRY, getId());

            if (getSort() != null) data.addProperty(SORT_ENTRY, getSort().toString());

            if (getFilterIds() != null && getFilterIds().size() > 0) {
                JsonArray filtersArray = new JsonArray();
                for (String filterId : getFilterIds()) {
                    filtersArray.add(filterId);
                }
                data.add(FILTERS_ENTRY, filtersArray);
            }

            return data;
        }
    }
}
