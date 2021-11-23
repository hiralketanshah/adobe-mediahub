package com.mediahub.core.services.impl;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import org.apache.commons.lang.text.StrLookup;
import org.apache.commons.lang.text.StrSubstitutor;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.models.AnalyticsCallData;
import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallFilter;
import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallMetric;
import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallSearch;
import com.mediahub.core.models.AnalyticsCallData.FilterType;
import com.mediahub.core.models.AnalyticsCallData.Sort;
import com.mediahub.core.services.AnalyticsGetterService;
import com.mediahub.core.services.AuthService;

@Component(
        service = {AnalyticsGetterService.class},
        configurationPolicy = ConfigurationPolicy.REQUIRE
)

@Designate(ocd = AnalyticsGetterServiceImpl.Config.class)
public class AnalyticsGetterServiceImpl implements AnalyticsGetterService {
	private static final Logger log = LoggerFactory.getLogger(AnalyticsGetterServiceImpl.class);
    private static final String DATE_OUTPUT_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";
    private static final String DATE_INPUT_FORMAT = "dd-MM-yyyy";
    private static final String ANALYTICS_TEMPLATES = "/etc/mediahub/analyticstemplates/";
    private static final String ANALYTICS_ENV = "analyticsEnv";
    private final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

    @ObjectClassDefinition(
            name = "Mediahub Analytics Getter",
            description = "Configuration for getting assets metrics from Adobe Analytics"
    )
    @interface Config {
    	@AttributeDefinition(
                name = "API Key",
                description = "Analytics API key parameter"
        )
        String apiKey() default "";
    	
    	@AttributeDefinition(
                name = "Global Company ID",
                description = "Global company ID on Analytics"
        )
        String globalCompanyId() default "";

        @AttributeDefinition(
                name = "Analytics API",
                description = "Analytics API base url"
        )
        String analyticsApiUrl() default "";
        
        @AttributeDefinition(
                name = "Analytics Env",
                description = "Analytics Environment"
        )
        String analyticsEnv() default "";
    }

    private Config config;
    private ExecutorService executorService;
    private DateFormat outputDateFormat = new SimpleDateFormat(DATE_OUTPUT_FORMAT);
    private DateFormat inputDateFormat = new SimpleDateFormat(DATE_INPUT_FORMAT);
    
    @Reference
    private AuthService authService;
    
    @Reference
	private ResourceResolverFactory resolverFactory;

    @Activate
    protected void activate(Config config) {
        this.executorService = Executors.newCachedThreadPool();
        this.config = config;
    }

    @Deactivate
    protected void deactivate(Config config) {
        this.executorService.shutdown();
    }
    
    /**
     * Custom call by template + parameters
     */
    @Override
    public JsonObject getCustomReport(Map<String, String> parametersMap) {    	
    	JsonObject reports = new JsonObject();
    	
    	try {
	//    	String rootPath = parametersMap.get("rootPath").toString();    		
    		if (parametersMap.containsKey("startDate")) {
    			String startDateStr = parametersMap.get("startDate").toString();
    			Date startDate = inputDateFormat.parse(startDateStr);
    			parametersMap.put("startDate", outputDateFormat.format(startDate));    			
    		}
    		
    		if (parametersMap.containsKey("endDate")) {
    			String endDateStr = parametersMap.get("endDate").toString();
    			Date endDate = inputDateFormat.parse(endDateStr);
    			parametersMap.put("endDate", outputDateFormat.format(endDate));    			
    		}
	    	
    		if (parametersMap.containsKey("dimension")) {    			
    			String dimension = parametersMap.get("dimension").toString();
    			reports = performCustomCall(dimension, parametersMap);
    		} else {
    			reports.addProperty("error", "dimension is mandatory");
    		}
			
    	} catch (ParseException e) {
    		log.error("Failed to parse date", e);
    		reports.addProperty("error", e.getMessage());
		} catch (LoginException e) {
			log.error("Failed to get call Template", e);
			reports.addProperty("error", e.getMessage());
		} catch (IOException e) {
			log.error("Failed to call Analytics service", e);
			reports.addProperty("error", e.getMessage());
		}
		
		return reports;
	}
    
    /**
     * Getting subfolders
     */
    @Override
    public JsonObject getFolders(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("aemassetimpressions", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallSearch search = new AnalyticsCallSearch("( BEGINS-WITH '" + rootPath + "' )");
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop14", metrics, null, globalFilters, search, null, 50L, "exclude-nones");
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
		
		return jObject;
	}
    
    
    /**
     * Number of Impressions
     */
    @Override
    public JsonObject getAssetImpressionsPerDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "2");
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangeday", metrics, metricFilters, globalFilters, null, Sort.asc);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
		//callParams.put("rootPath", rootPath);
		
		return jObject;
	}
    
    @Override
    public JsonObject getAssetImpressionsPerDayCounter(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Sort.desc, Collections.singletonList("0"));
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangeday", metrics, metricFilters, globalFilters, null, null);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
		//callParams.put("rootPath", rootPath);
		
		return jObject;
	}
    
    @Override
    public JsonObject getImpressionsPerMonthTarget(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
		return getImpressionsPerMonth(rootPath, startDate, endDate);
	}
    
    @Override
    public JsonObject getImpressionsPerMonth(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getCurrentMonthStart()) + "/" + outputDateFormat.format(getCurrentMonthEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangemonth", metrics, metricFilters, globalFilters, null, Sort.asc);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
		//callParams.put("rootPath", rootPath);
		
		return jObject;
	}
    
    @Override
    public JsonObject getImpressionsPerDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "2", Sort.desc, Collections.singletonList("0"));
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangeday", metrics, metricFilters, globalFilters, null, null);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
		//callParams.put("rootPath", rootPath);
		
		return jObject;
	}
    
    /**
     * Assets Characteristic
     */
    @Override
    public JsonObject getTopAssetImpressions(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "2", Sort.desc);
    	AnalyticsCallMetric typeMetric = new AnalyticsCallMetric("cm1773_613a1285f5584605697d0cfe", "3");
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc, typeMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangeday", metrics, metricFilters, globalFilters, null, null, 10L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getTopAssetMediaTypes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop6", metrics, null, globalFilters, null, null, 5L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getTopAssetCategories(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop7", metrics, null, globalFilters, null, null, 10L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getTopAssetTypes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop3", metrics, null, globalFilters, null, null, 10L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getTopMediaThemes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop8", metrics, null, globalFilters, null, null, 10L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getTopAssetsSensibility(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "0", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop1", metrics, null, globalFilters, null, null, 5L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getBroadcastStatus(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "2", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop13", metrics, metricFilters, globalFilters, null, null, 50L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getImpressionsWithSubs(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "2", Arrays.asList("0", "1"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "3", Arrays.asList("2", "3"));
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc);
    	
    	AnalyticsCallFilter breakdownMetricFilter = new AnalyticsCallFilter("0", FilterType.breakdown, "prop4", "2711056172");
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("1", FilterType.dateRange, outputDateFormat.format(getTowWeeksBeforeNow()) + "/" + outputDateFormat.format(getEndOfTHeDay()));
    	AnalyticsCallFilter breakdownMetricFilter2 = new AnalyticsCallFilter("2", FilterType.breakdown, "prop4", "3611065747" );
    	AnalyticsCallFilter dateRangeMetricFilter2 = new AnalyticsCallFilter("3", FilterType.dateRange, outputDateFormat.format(getTowWeeksBeforeNow()) + "/" + outputDateFormat.format(getEndOfTHeDay()));
    	List<AnalyticsCallFilter> metricFilters = Arrays.asList(breakdownMetricFilter, dateRangeMetricFilter, breakdownMetricFilter2, dateRangeMetricFilter2);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "daterangeday", metrics, metricFilters, globalFilters, null, Sort.asc);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getSubtitles(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {    	
    	return getImpressionsWithSubs(rootPath, startDate, endDate);
	}
    
    
    @Override
    public JsonObject getDetailedAssetInfo(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	return getImpressionsWithSubs(rootPath, startDate, endDate);
	}
    
    @Override
    public JsonObject getFreeForm(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("assetimpressions", "1", Arrays.asList("0", "2", "4"));
    	AnalyticsCallMetric impressionsMetric2 = new AnalyticsCallMetric("assetimpressions", "2", Arrays.asList("1", "3", "5"));
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetric2);
    	
    	AnalyticsCallFilter breakdownMetricFilter = new AnalyticsCallFilter("0", FilterType.breakdown, "prop1", "3204310542");
    	AnalyticsCallFilter breakdownMetricFilter2 = new AnalyticsCallFilter("1", FilterType.breakdown, "prop1", "1742590979");
    	AnalyticsCallFilter breakdownMetricFilter3 = new AnalyticsCallFilter("2", FilterType.breakdown, "prop11", "1277529264" );
    	AnalyticsCallFilter breakdownMetricFilter4 = new AnalyticsCallFilter("3", FilterType.breakdown, "prop11", "1277529264" );
    	AnalyticsCallFilter breakdownMetricFilter5 = new AnalyticsCallFilter("4", FilterType.breakdown, "prop6", "234358538" );
    	AnalyticsCallFilter breakdownMetricFilter6 = new AnalyticsCallFilter("5", FilterType.breakdown, "prop6", "234358538" );
    	List<AnalyticsCallFilter> metricFilters = Arrays.asList(breakdownMetricFilter, breakdownMetricFilter2, breakdownMetricFilter3, breakdownMetricFilter4, breakdownMetricFilter5, breakdownMetricFilter6);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "prop4", metrics, metricFilters, globalFilters, null, null, 5L, "exclude-nones");
    	
    	return performRegularCall(callData.toJson());
	}
    
    /**
     * Technical Information
     */
    @Override
    public JsonObject getDeviceType(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetric2 = new AnalyticsCallMetric("event1", "2");
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetric2);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(dateRangeMetricFilter);
    	
    	AnalyticsCallSearch search = new AnalyticsCallSearch("itemIds", "0", "2163986270");
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "mobiledevicetype", metrics, metricFilters, globalFilters, search, null, 5000L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getDeviceTypeTop(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "2", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(dateRangeMetricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "mobiledevicetype", metrics, metricFilters, globalFilters, null, null, 5L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getImpressionsPerBrowser(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Arrays.asList("0", "1"));
    	AnalyticsCallMetric impressionsMetric2 = new AnalyticsCallMetric("event1", "2", Collections.singletonList("2"));
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetric2);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	AnalyticsCallFilter breakdownMetricFilter = new AnalyticsCallFilter("1", FilterType.breakdown, "browsertype", "7");
    	AnalyticsCallFilter breakdownMetricFilter2 = new AnalyticsCallFilter("2", FilterType.breakdown, "browsertype", "7");
    	List<AnalyticsCallFilter> metricFilters = Arrays.asList(dateRangeMetricFilter, breakdownMetricFilter, breakdownMetricFilter2);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "browser", metrics, metricFilters, globalFilters, null, null, 5L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getOperatingSystems(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "2", Sort.desc);
    	AnalyticsCallMetric operatingSystemMetric = new AnalyticsCallMetric("cm1773_613a192e405ee86c1753c0b1", "3");
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc, operatingSystemMetric);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(dateRangeMetricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "operatingsystemgroup", metrics, metricFilters, globalFilters, null, null, 10L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getOperatingSystemsTable(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Arrays.asList("0", "1"));
    	AnalyticsCallMetric impressionsMetricDesc = new AnalyticsCallMetric("event1", "2", Collections.singletonList("2"));
    	AnalyticsCallMetric operatingSystemMetric = new AnalyticsCallMetric("cm1773_613a192e405ee86c1753c0b1", "3", Collections.singletonList("3"));
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetricDesc, operatingSystemMetric);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	AnalyticsCallFilter breakdownMetricFilter = new AnalyticsCallFilter("1", FilterType.breakdown, "operatingsystemgroup", "4");
    	AnalyticsCallFilter breakdownMetricFilter2 = new AnalyticsCallFilter("2", FilterType.breakdown, "operatingsystemgroup", "4");
    	AnalyticsCallFilter breakdownMetricFilter3 = new AnalyticsCallFilter("3", FilterType.breakdown, "operatingsystemgroup", "4");
    	List<AnalyticsCallFilter> metricFilters = Arrays.asList(dateRangeMetricFilter, breakdownMetricFilter, breakdownMetricFilter2, breakdownMetricFilter3);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "operatingsystem", metrics, metricFilters, globalFilters, null, null, 5L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getReferrer(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "referrer", metrics, null, globalFilters, null, null, 50L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    /**
     * Geoloc
     */
    @Override
    public JsonObject getMap(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "geocountry", metrics, null, globalFilters, null, null, 50L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getImpressionsPerCountry(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "geocountry", metrics, null, globalFilters, null, null, 50L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getLanguage(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "language", metrics, null, globalFilters, null, null, 50L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    /**
     * Time Parting
     */
    @Override
    public JsonObject getWeekdayImpressions(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetric2 = new AnalyticsCallMetric("event1", "2");
    	AnalyticsCallMetric timepartMetric = new AnalyticsCallMetric("cm1773_613a1cd48ef9e17f9e77f1ca", "3");
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetric2, timepartMetric);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastMonthStart()) + "/" + outputDateFormat.format(getLastMonthEnd()));
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(dateRangeMetricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "timepartdayofweek", metrics, metricFilters, globalFilters, null, Sort.asc, 10L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    @Override
    public JsonObject getWeekday(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	return getWeekdayImpressions(rootPath, startDate, endDate);
	}
    
    @Override
    public JsonObject getHourOfDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + "/" + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric("event1", "1", Collections.singletonList("0"));
    	AnalyticsCallMetric impressionsMetric2 = new AnalyticsCallMetric("event1", "3", Collections.singletonList("1"));
    	List<AnalyticsCallMetric> metrics = Arrays.asList(impressionsMetric, impressionsMetric2);
    	
    	AnalyticsCallFilter dateRangeMetricFilter = new AnalyticsCallFilter("0", FilterType.dateRange, outputDateFormat.format(getLastWeekStart()) + "/" + outputDateFormat.format(getLastWeekEnd()));
    	AnalyticsCallFilter dateRangeMetricFilter2 = new AnalyticsCallFilter("1", FilterType.dateRange, outputDateFormat.format(getCurrentWeekStart()) + "/" + outputDateFormat.format(getCurrentWeekEnd()));
    	List<AnalyticsCallFilter> metricFilters = Arrays.asList(dateRangeMetricFilter, dateRangeMetricFilter2);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), "timeparthourofday", metrics, metricFilters, globalFilters, null, Sort.asc, 25L);
    	
    	return performRegularCall(callData.toJson());
	}
    
    
    /**
     * Call handling methods
     */
    private JsonObject performRegularCall(JsonObject callData) throws IOException, LoginException {	    
	    return performReportsCall(callData.toString());
    }
	
    private JsonObject performCustomCall(String action, Map<String, String> callParams) throws LoginException, IOException {
    	try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
			Resource getFoldersTemplateResource = resourceResolver.getResource(ANALYTICS_TEMPLATES + action + ".json");
			InputStream is = getFoldersTemplateResource.adaptTo(InputStream.class);
			BufferedInputStream bis = new BufferedInputStream(is);
			ByteArrayOutputStream buf = new ByteArrayOutputStream();
			int result = bis.read();
			while (result != -1) {
				byte b = (byte) result;
				buf.write(b);
				result = bis.read();
			}
			
			String callTemplate = buf.toString();
			
			callParams.put(ANALYTICS_ENV, this.config.analyticsEnv());
			
			StrSubstitutor substitutor = new StrSubstitutor(StrLookup.mapLookup(callParams));
		    String messageBody = substitutor.replace(callTemplate);
			
			return performReportsCall(messageBody);
    	}
	}
	
    private JsonObject performReportsCall(String message) throws IOException {
    	String accessToken = authService.getAuthToken();			
    	
    	String reportsUrl = this.config.analyticsApiUrl() + "/" + this.config.globalCompanyId() + "/reports";
    	URL obj = new URL(reportsUrl);
    	HttpsURLConnection con = (HttpsURLConnection)obj.openConnection();
    	
    	con.setRequestMethod("POST");
    	con.setRequestProperty("Content-Type", "application/json; charset=UTF-8");
    	con.setDoOutput(true);
    	// Add access token to Authorization header
    	con.setRequestProperty("Authorization", "Bearer " + accessToken);
    	// Add x-api-key header
    	con.setRequestProperty("x-api-key", this.config.apiKey());
    	con.setRequestProperty("x-proxy-global-company-id", this.config.globalCompanyId());
    	
    	log.info("Sending 'POST' request to URL: " + reportsUrl);
    	
    	OutputStream os = con.getOutputStream();
    	os.write(message.getBytes("UTF-8"));
    	os.close();
    	
    	con.connect();
    	
    	String response = handleResponse(con);
    	
    	log.info("analytics/reports response: " + response);
    	
    	// get the global company id of the first company in the first IMS org
    	JsonObject jObject = new JsonParser().parse(response).getAsJsonObject();
    	
    	return jObject;
    }
    
	private static String handleResponse(HttpsURLConnection con) throws IOException {
		int responseCode = con.getResponseCode();
		log.info("Response Code: " + responseCode);
		
		boolean responseError = false;
		InputStream is;
		if(responseCode < HttpsURLConnection.HTTP_BAD_REQUEST) {
			is = con.getInputStream();
		} else {
			/* error from server */
			is = con.getErrorStream();
			responseError = true;
		}

		BufferedReader in = new BufferedReader(new InputStreamReader(is));
		String inputLine;
		StringBuilder response = new StringBuilder();

		while((inputLine = in.readLine()) != null) {
			response.append(inputLine);
		}
		in.close();

		if(responseError) {
			log.info(response.toString());
		}
		
		return response.toString();
	}
	
    /**
     * Util methods
     */
    private static Calendar resetTime(Calendar calendar) {
    	calendar.set(Calendar.HOUR_OF_DAY, 0);
    	calendar.set(Calendar.MINUTE, 0);
    	calendar.set(Calendar.SECOND, 0);
    	calendar.set(Calendar.MILLISECOND, 0);
    	
    	return calendar;
    }
    
    private static Date getLastWeekStart() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i - 7);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getLastWeekEnd() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getCurrentMonthStart() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.set(Calendar.DAY_OF_MONTH, 1);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getCurrentMonthEnd() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.add(Calendar.MONTH, 1);
        c.set(Calendar.DAY_OF_MONTH, 1);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getTowWeeksBeforeNow() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.add(Calendar.DAY_OF_MONTH, -14);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getEndOfTHeDay() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.add(Calendar.DAY_OF_MONTH, 1);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getLastMonthStart() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.set(Calendar.DAY_OF_MONTH, 1);
        c.add(Calendar.MONTH, -1);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getLastMonthEnd() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        c.set(Calendar.DAY_OF_MONTH, 1);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getCurrentWeekStart() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i);
        c = resetTime(c);
        return c.getTime();
    }
    
    private static Date getCurrentWeekEnd() {
    	Date date = new Date();
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        
        int i = c.get(Calendar.DAY_OF_WEEK) - c.getFirstDayOfWeek();
        c.add(Calendar.DATE, -i + 7);
        c = resetTime(c);
        return c.getTime();
    }
}