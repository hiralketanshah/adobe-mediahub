package com.mediahub.core.services.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import org.apache.sling.api.resource.Resource;
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
    }

    private Config config;
    private ExecutorService executorService;
    
    @Reference
    private AuthService authService;

    @Activate
    protected void activate(Config config) {
        this.executorService = Executors.newCachedThreadPool();
        this.config = config;
    }

    @Deactivate
    protected void deactivate(Config config) {
        this.executorService.shutdown();
    }

    @Override
    public JsonObject updateMetrics(Resource asset, Date startDate, Date endDate) {
    	JsonObject reports = new JsonObject();
    	try {
			String accessToken = authService.getAuthToken();
			log.info("Access Token: " + accessToken);
			reports = callAnalyticsReports(accessToken, asset, startDate, endDate);
		} catch(Exception e) {
			log.error("Error Occurred while fetching metrics from Analytics:", e);
			reports.addProperty("error", e.getMessage());
		}
    	
    	return reports;
    }
	
	private JsonObject callAnalyticsReports(String accessToken, Resource asset, Date startDate, Date endDate) throws IOException {
		DateFormat dateFormat = new SimpleDateFormat(DATE_OUTPUT_FORMAT);
		
		if (startDate == null) {
			Calendar startCalendar = Calendar.getInstance();
			startCalendar.add(Calendar.DAY_OF_MONTH, -1);
			startDate = startCalendar.getTime();
		}
		
		if (endDate == null) {
			endDate = new Date();
		}
		
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
		
		String jsonBody = "{\"rsid\":\"bnppgcaeminsightsdev\",\"globalFilters\":[{\"type\":\"dateRange\",\"dateRange\":\"" + dateFormat.format(startDate) + "/" + dateFormat.format(endDate) + "\"}],\"metricContainer\":{\"metrics\":[{\"columnId\":\"1\",\"id\":\"metrics/event1\",\"filters\":[\"0\"]},{\"columnId\":\"2\",\"id\":\"metrics/event1\",\"sort\":\"desc\"},{\"columnId\":\"3\",\"id\":\"cm1773_613a1285f5584605697d0cfe\"}],\"metricFilters\":[{\"id\":\"0\",\"type\":\"dateRange\",\"dateRange\":\"" + dateFormat.format(startDate) + "/" + dateFormat.format(endDate) + "\"}]},\"dimension\":\"variables/aemassetid\",\"settings\":{\"countRepeatInstances\":true,\"limit\":10,\"page\":0,\"nonesBehavior\":\"return-nones\"},\"statistics\":{\"functions\":[\"col-max\",\"col-min\"]}}";
		
		OutputStream os = con.getOutputStream();
        os.write(jsonBody.getBytes("UTF-8"));
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
}