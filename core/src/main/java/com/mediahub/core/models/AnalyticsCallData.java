package com.mediahub.core.models;

import java.util.List;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

public class AnalyticsCallData {
	
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
		this.metrics = metrics;
		this.metricFilters = metricFilters;
		this.globalFilters = globalFilters;
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
	
	public AnalyticsCallData(String rsId, String dimension, List<AnalyticsCallMetric> metrics) {
		this(rsId, dimension, metrics, null, null, null, null);
	}

	public String getRsId() {
		return rsId;
	}

	public void setRsId(String rsId) {
		this.rsId = rsId;
	}

	public List<AnalyticsCallFilter> getGlobalFilters() {
		return globalFilters;
	}

	public void setGlobalFilters(List<AnalyticsCallFilter> globalFilters) {
		this.globalFilters = globalFilters;
	}

	public List<AnalyticsCallMetric> getMetrics() {
		return metrics;
	}

	public void setMetrics(List<AnalyticsCallMetric> metrics) {
		this.metrics = metrics;
	}

	public List<AnalyticsCallFilter> getMetricsFilters() {
		return metricFilters;
	}

	public void setMetricsFilters(List<AnalyticsCallFilter> metricsFilters) {
		this.metricFilters = metricsFilters;
	}

	public String getDimension() {
		return dimension;
	}

	public void setDimension(String dimension) {
		this.dimension = dimension;
	}

	public Long getLimit() {
		return limit;
	}

	public void setLimit(Long limit) {
		this.limit = limit;
	}

	public String getNonesBehavior() {
		return nonesBehavior;
	}

	public void setNonesBehavior(String nonesBehavior) {
		this.nonesBehavior = nonesBehavior;
	}
	
	public Sort getDimensionSort() {
		return dimensionSort;
	}

	public void setDimensionSort(Sort dimensionSort) {
		this.dimensionSort = dimensionSort;
	}

	public AnalyticsCallSearch getSearch() {
		return search;
	}

	public void setSearch(AnalyticsCallSearch search) {
		this.search = search;
	}

	public JsonObject toJson() {
		JsonObject data = new JsonObject();
		
		//rsid
		data.addProperty("rsid", getRsId());
		
		//globalFilters
		if (getGlobalFilters() != null) {
			JsonArray globalFiltersArray = new JsonArray();
			for (AnalyticsCallFilter globalFilter: getGlobalFilters()) {
				globalFiltersArray.add(globalFilter.toJson());
			}
			data.add("globalFilters", globalFiltersArray);
		}
		
		//metricContainer
		JsonObject metricContainer = new JsonObject();
		
		JsonArray metricsArray = new JsonArray();
		for (AnalyticsCallMetric metric: getMetrics()) {
			metricsArray.add(metric.toJson());
		}
		metricContainer.add("metrics", metricsArray);
		
		if (getMetricsFilters() != null) {
			JsonArray metricFiltersArray = new JsonArray();
			for (AnalyticsCallFilter metricFilter: getMetricsFilters()) {
				metricFiltersArray.add(metricFilter.toJson());
			}
			metricContainer.add("metricFilters", metricFiltersArray);			
		}
		
		data.add("metricContainer", metricContainer);
		
		//dimension
		data.addProperty("dimension", "variables/" + getDimension());
		
		//search
		if (getSearch() != null) {			
			data.add("search", getSearch().toJson());
		}
		
		//settings
		JsonObject settings = new JsonObject();
		settings.addProperty("countRepeatInstances", true);
		settings.addProperty("limit", getLimit());
		settings.addProperty("page", 0);
		if (getDimensionSort() != null) settings.addProperty("dimensionSort", getDimensionSort().toString());
		settings.addProperty("nonesBehavior", getNonesBehavior());
		
		data.add("settings", settings);
		
		//statistics
		JsonObject statistics = new JsonObject();
		JsonArray functionsArray = new JsonArray();
		functionsArray.add("col-max");
		functionsArray.add("col-min");
		
		statistics.add("functions", functionsArray);
		
		data.add("statistics", statistics);
		
		return data;
	}
	
	public static class AnalyticsCallSearch {
		
		private String clause;
		
		private String byField;
		
		private String[] fieldTerms;
		
		public AnalyticsCallSearch(String clause) {
			this.setClause(clause);
			this.setByField(null);
			this.setFieldTerms(null);
		}
		
		public AnalyticsCallSearch(String byField, String...fieldTerms) {
			this.setClause(null);
			this.setByField(byField);
			this.setFieldTerms(fieldTerms);
		}

		public String getClause() {
			return clause;
		}

		public void setClause(String clause) {
			this.clause = clause;
		}

		public String getByField() {
			return byField;
		}

		public void setByField(String byField) {
			this.byField = byField;
		}

		public String[] getFieldTerms() {
			return fieldTerms;
		}

		public void setFieldTerms(String[] fieldTerms) {
			this.fieldTerms = fieldTerms;
		}
		
		public JsonObject toJson() {
			JsonObject data = new JsonObject();
			
			if (getClause() != null) {
				data.addProperty("clause", getClause());
			} else if (getByField() != null) {
				JsonArray termsArray = new JsonArray();
				for (String term: getFieldTerms()) {
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
		
		public void setId(String id) {
			this.id = id;
		}
		
		public FilterType getType() {
			return type;
		}
		
		public void setType(FilterType type) {
			this.type = type;
		}
		
		public String getTypeValue() {
			return typeValue;
		}
		
		public void setTypeValue(String typeValue) {
			this.typeValue = typeValue;
		}
		
		public String getDimension() {
			return dimension;
		}

		public void setDimension(String dimension) {
			this.dimension = dimension;
		}

		public String getItemId() {
			return itemId;
		}

		public void setItemId(String itemId) {
			this.itemId = itemId;
		}

		public JsonObject toJson() {
			JsonObject data = new JsonObject();
			
			if (getId() != null) data.addProperty("id", getId());
			data.addProperty("type", getType().toString());
			if (getTypeValue() != null) data.addProperty(getType().toString(), getTypeValue());
			if (getDimension() != null) data.addProperty("dimension", "variables/" + getDimension());
			if (getItemId() != null) data.addProperty("itemId", getItemId());
			
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
		
		public void setId(String id) {
			this.id = id;
		}
		
		public String getColumnId() {
			return columnId;
		}
		
		public void setColumnId(String columnId) {
			this.columnId = columnId;
		}
		
		public Sort getSort() {
			return sort;
		}
		
		public void setSort(Sort sort) {
			this.sort = sort;
		}
		
		public List<String> getFilterIds() {
			return filterIds;
		}
		
		public void setFilterIds(List<String> filterIds) {
			this.filterIds = filterIds;
		}
		
		public JsonObject toJson() {
			JsonObject data = new JsonObject();
			
			data.addProperty("columnId", getColumnId());
			data.addProperty("id", getId());
			
			if (getSort() != null) data.addProperty("sort", getSort().toString());
			
			if (getFilterIds() != null && getFilterIds().size() > 0) {
				JsonArray filtersArray = new JsonArray();
				for (String filterId: getFilterIds()) {
					filtersArray.add(filterId);
				}
				data.add("filters", filtersArray);
			}
			
			return data;
		}
	}
}
