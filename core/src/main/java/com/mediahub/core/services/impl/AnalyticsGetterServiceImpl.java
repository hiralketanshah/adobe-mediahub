package com.mediahub.core.services.impl;

import static java.lang.Boolean.TRUE;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.net.ssl.HttpsURLConnection;

import org.apache.sling.api.resource.Resource;
import org.json.JSONException;
import org.json.JSONObject;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.services.AnalyticsGetterService;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

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
                name = "Organization ID",
                description = "JWT Organization ID parameter"
        )
        String orgId() default "";
    	
    	@AttributeDefinition(
                name = "Technical Account ID",
                description = "JWT technical account ID parameter"
        )
        String technicalAccountId() default "";
    	
    	@AttributeDefinition(
                name = "API Key",
                description = "JWT API key parameter"
        )
        String apiKey() default "";
    	
    	@AttributeDefinition(
                name = "Certificate Path",
                description = "JWT signing certificate path"
        )
        String keyPath() default "";
    	
    	@AttributeDefinition(
                name = "IMS host",
                description = "IMS Authentication Service host"
        )
        String imsHost() default "";
    	
    	@AttributeDefinition(
                name = "Metascopes",
                description = "JWT integration scopes"
        )
        String metascopes() default "";
    	
    	@AttributeDefinition(
                name = "IMS Exchange",
                description = "IMS Authentication Service exchange url"
        )
        String imsExchange() default "";
    	
    	@AttributeDefinition(
                name = "Client Secret",
                description = "Secret for exchanging JWT token"
        )
        String secret() default "";
    	
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
    public JSONObject updateMetrics(Resource asset, Date startDate, Date endDate) {
    	JSONObject reports = new JSONObject();
    	try {
			String jwtToken = getJWTToken();
			if(jwtToken != null && jwtToken != "") {
				log.info("JWT Token: " + jwtToken);
				String accessToken = getAccessToken(jwtToken);
				log.info("Access Token: " + accessToken);
				reports = callAnalyticsReports(accessToken, asset, startDate, endDate);
			}
		} catch(Exception e) {
			log.error("Error Occurred while fetching metrics from Analytics:", e);
			try {
				reports.put("error", e.getMessage());
			} catch (JSONException je) {
				log.error("Could not parse error message into response", je);
			}
		}
    	
    	return reports;
    }

	public String getJWTToken()
			throws NoSuchAlgorithmException, InvalidKeySpecException, IOException {
		// Expiration time in seconds
		Long expirationTime = System.currentTimeMillis() / 1000 + 86400L;
		// Metascopes associated to key
		String metascopes[] = this.config.metascopes().split(",");

		// Secret key as byte array. Secret key file should be in DER encoded format.
		byte[] privateKeyFileContent = Files.readAllBytes(Paths.get(this.config.keyPath()));

		// Read the private key
		KeyFactory keyFactory = KeyFactory.getInstance("RSA");
		KeySpec ks = new PKCS8EncodedKeySpec(privateKeyFileContent);
		RSAPrivateKey privateKey = (RSAPrivateKey) keyFactory.generatePrivate(ks);

		// Create JWT payload
		Map<String, Object> jwtClaims = new HashMap<>();
		jwtClaims.put("iss", this.config.orgId());
		jwtClaims.put("sub", this.config.technicalAccountId());
		jwtClaims.put("exp", expirationTime);
		jwtClaims.put("aud", "https://" + this.config.imsHost() + "/c/" + this.config.apiKey());
		for(String metascope : metascopes) {
			jwtClaims.put("https://" + this.config.imsHost() + "/s/" + metascope, TRUE);
		}

		SignatureAlgorithm sa = SignatureAlgorithm.RS256;
		// Create the final JWT token
		String jwtToken = Jwts.builder().setClaims(jwtClaims).signWith(sa, privateKey).compact();

		return jwtToken;
	}

	public String getAccessToken(String jwtToken) throws IOException, JSONException {
		// Load relevant properties from prop file
		String accessToken = "";

		URL obj = new URL(this.config.imsExchange());

		HttpsURLConnection con = (HttpsURLConnection) obj.openConnection();

		// add request header
		con.setRequestMethod("POST");

		// Add parameters to request
		String urlParameters = "client_id=" + this.config.apiKey() + "&client_secret=" + this.config.secret() + "&jwt_token=" + jwtToken;

		// Send post request
		con.setDoOutput(true);
		DataOutputStream wr = new DataOutputStream(con.getOutputStream());
		wr.writeBytes(urlParameters);
		wr.flush();
		wr.close();
		
		log.info("Sending 'POST' request to URL: " + this.config.imsExchange());
		log.info("Post parameters: " + urlParameters);

		String response = handleResponse(con);

		JSONObject jObject = new JSONObject(response);
		accessToken = jObject.getString("access_token");

		return accessToken;
	}
	
	public JSONObject callAnalyticsReports(String accessToken, Resource asset, Date startDate, Date endDate) throws IOException, JSONException {
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
		JSONObject jObject = new JSONObject(response);
		
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