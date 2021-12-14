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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
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

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
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
	private static final String METRICS_PREFIX = "metrics/";

	private static final Logger log = LoggerFactory.getLogger(AnalyticsGetterServiceImpl.class);
	
	private static final String JSON_EXTENSION = ".json";
	private static final String EXCLUDE_NONES = "exclude-nones";
	private static final String ASSET_PATH_DIMENSION = "prop14";
	private static final String ZERO = "0";
	private static final String DATE_RANGE_SEPARATOR = "/";
	private static final String AEMASSETIMPRESSIONS_METRIC = "aemassetimpressions";
    private static final String DATE_OUTPUT_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS";
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
    public JsonElement getCustomReport(Map<String, String> parametersMap, Date startDate, Date endDate) {    	
    	JsonElement reports = null;
    	JsonObject error = new JsonObject();
    	
    	try {
	//    	String rootPath = parametersMap.get("rootPath").toString();    		
    		if (startDate != null) {
    			parametersMap.put("startDate", outputDateFormat.format(startDate));    			
    		}
    		
    		if (endDate != null) {
    			parametersMap.put("endDate", outputDateFormat.format(endDate));    			
    		}
	    	
    		if (parametersMap.containsKey("template")) {    			
    			String template = parametersMap.get("template").toString();
    			reports = performCustomCall(template, parametersMap);
    		} else {
    			error.addProperty("error", "Either dimension or template is mandatory");
    			reports = error;
    		}
			
    	} catch (LoginException e) {
			log.error("Failed to get call Template", e);
			error.addProperty("error", e.getMessage());
			reports = error;
		} catch (IOException e) {
			log.error("Failed to call Analytics service", e);
			error.addProperty("error", e.getMessage());
			reports = error;
		}
		
		return reports;
	}
    
    /**
     * Getting dimension data
     */
    @Override
    public JsonArray getDimension(String rootPath, String dimension, Date startDate, Date endDate) throws IOException, LoginException {
    	List<String> folders = getFolderIds(rootPath, startDate, endDate);
    	
    	Map<String, Long> merged = new HashMap<String, Long>();
    	
    	for (String folderId: folders) {
    		Map<String, Long> remaining = getDimensionCall(folderId, dimension, startDate, endDate);
    		remaining.forEach((k, v) -> merged.merge(k, v, Long::sum));
    	}
    	
    	JsonArray result = new JsonArray();
    	for (Map.Entry<String, Long> entry : merged.entrySet()) {
    		JsonObject obj = new JsonObject();
    		obj.addProperty(entry.getKey(), entry.getValue());
    		result.add(obj);
    	}
    	
    	return result;
    }
    
    private Map<String, Long> getDimensionCall(String folderId, String dimension, Date startDate, Date endDate) throws IOException, LoginException {
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + DATE_RANGE_SEPARATOR + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric(METRICS_PREFIX + AEMASSETIMPRESSIONS_METRIC, ZERO, Collections.singletonList(ZERO));
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallFilter metricFilter = new AnalyticsCallFilter(ZERO, FilterType.breakdown, ASSET_PATH_DIMENSION, folderId);
    	List<AnalyticsCallFilter> metricFilters = Collections.singletonList(metricFilter);
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), dimension, globalFilters, metrics, metricFilters);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
    	return toSimpleResult(jObject);
    }
    
    /**
     * Getting subfolders
     */
    private List<String> getFolderIds(String rootPath, Date startDate, Date endDate) throws IOException, LoginException {
    	List<String> result = new ArrayList<String>();
    	AnalyticsCallFilter globalFilter = new AnalyticsCallFilter(FilterType.dateRange, outputDateFormat.format(startDate) + DATE_RANGE_SEPARATOR + outputDateFormat.format(endDate));
    	List<AnalyticsCallFilter> globalFilters = Collections.singletonList(globalFilter);
    	
    	AnalyticsCallMetric impressionsMetric = new AnalyticsCallMetric(METRICS_PREFIX + AEMASSETIMPRESSIONS_METRIC, ZERO, Sort.desc);
    	List<AnalyticsCallMetric> metrics = Collections.singletonList(impressionsMetric);
    	
    	AnalyticsCallSearch search = new AnalyticsCallSearch("( BEGINS-WITH '" + rootPath + "' )");
    	
    	AnalyticsCallData callData = new AnalyticsCallData(this.config.analyticsEnv(), ASSET_PATH_DIMENSION, metrics, null, globalFilters, search, null, EXCLUDE_NONES);
    	
    	JsonObject jObject = performRegularCall(callData.toJson());
    	
    	for (JsonElement elem: jObject.getAsJsonArray("rows")) {
    		if (elem.isJsonObject()) {
    			JsonObject elemObj = elem.getAsJsonObject();
    			JsonElement valueElem = elemObj.get("itemId");
    			result.add(valueElem.getAsString());
    		}
    	}
		
		return result;
	}

	/**
     * Call handling methods
     */
    private JsonObject performCustomCall(String template, Map<String, String> callParams) throws LoginException, IOException {
    	try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
			Resource getFoldersTemplateResource = resourceResolver.getResource(ANALYTICS_TEMPLATES + template + JSON_EXTENSION);
			if (getFoldersTemplateResource == null) {
				log.error("Failed to call Analytics service");
				JsonObject error = new JsonObject();
				error.addProperty("error", "Expected to find a call template at: " + ANALYTICS_TEMPLATES + template + JSON_EXTENSION);
				return error;
			}
			
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
	
    private JsonObject performRegularCall(JsonObject callData) throws IOException, LoginException {	    
    	return performReportsCall(callData.toString());
    }
    
    private JsonObject performReportsCall(String message) throws IOException {
    	String accessToken = authService.getAuthToken();			
    	
    	String reportsUrl = this.config.analyticsApiUrl() + DATE_RANGE_SEPARATOR + this.config.globalCompanyId() + "/reports";
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
    	
    	log.debug("Sending 'POST' request to URL: " + reportsUrl);
    	
    	OutputStream os = con.getOutputStream();
    	os.write(message.getBytes("UTF-8"));
    	os.close();
    	
    	con.connect();
    	
    	String response = handleResponse(con);
    	
    	log.debug("analytics/reports response: " + response);
    	
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
	
	private Map<String, Long> toSimpleResult(JsonObject jObject) {
		Map<String, Long> result = new HashMap<String, Long>();
		JsonArray rows = jObject.getAsJsonArray("rows");
		rows.forEach((elem) -> {
			if (elem.isJsonObject()) {
				JsonObject row = elem.getAsJsonObject();
				if (row.has("value") && row.has("data")) {
					String value = row.get("value").getAsString();
					Long data = null;
					
					JsonElement dataElem = row.get("data");
					if (dataElem.isJsonArray()) {
						data = dataElem.getAsJsonArray().getAsLong();
					} else {
						data = dataElem.getAsLong();
					}
					
					result.put(value, data);					
				}
	        }
	    });
		
		return result;
	}
}