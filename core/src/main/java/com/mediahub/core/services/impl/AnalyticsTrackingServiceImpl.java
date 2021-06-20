package com.mediahub.core.services.impl;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.services.AnalyticsTrackingService;
import org.apache.sling.api.resource.Resource;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.DataOutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Component(
        service = {AnalyticsTrackingService.class},
        configurationPolicy = ConfigurationPolicy.REQUIRE
)

@Designate(ocd = AnalyticsTrackingServiceImpl.Config.class)
public class AnalyticsTrackingServiceImpl implements AnalyticsTrackingService {
    private static final Logger log = LoggerFactory.getLogger(AnalyticsTrackingServiceImpl.class);

    @ObjectClassDefinition(
            name = "Mediahub Asset Tracking",
            description = "Configuration for sending assets tracking to Adobe Analytics"
    )
    @interface Config {
        @AttributeDefinition(
                name = "Namespace",
                description = "Namespace of the Analytics account"
        )
        String namespace() default "";

        @AttributeDefinition(
                name = "Report Suite ID",
                description = "ID of the Report Suite which will receive tracking"
        )
        String report_suite() default "";
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
    public void trackExternal(Resource asset, String format) {
        Map<String, String> parameters = new HashMap<>();
        parameters.put("c.a.assets.source", "AEM");
        parameters.put("c.a.assets.idlist", asset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
        parameters.put("pe", "o");
        parameters.put("pev2", "Asset Insight Event");
        parameters.put("vid", "1234567890123456-6543210987654321");
        sendRequest(parameters);
    }

    @Override
    public void trackInternal(Resource asset, String format) {
        Map<String, String> parameters = new HashMap<>();
        parameters.put("c.a.assets.source", "AEM");
        parameters.put("c.a.assets.idlist", asset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
        parameters.put("pe", "o");
        parameters.put("pev2", "Asset Insight Event");
        parameters.put("vid", "1234567890123456-6543210987654321");
        sendRequest(parameters);
    }

    private void sendRequest(Map<String, String> parameters) {
        this.executorService.execute(() -> {
            try {

                String baseUrl = "http://" + this.config.namespace() + ".sc.omtrdc.net/b/ss/" + this.config.report_suite() + "/0";

                URL url = new URL(baseUrl);
                HttpURLConnection con = (HttpURLConnection) url.openConnection();
                con.setRequestMethod("GET");
                con.setDoOutput(true);
                con.setConnectTimeout(5000);
                con.setReadTimeout(5000);

                //Setting parameters
                DataOutputStream out = new DataOutputStream(con.getOutputStream());
                out.writeBytes(getParamsString(parameters));
                out.flush();
                out.close();

                int status = con.getResponseCode();
                if (status == HttpURLConnection.HTTP_OK) {
                    log.trace("Request to {} done", url);
                } else {
                    log.error("Error when sending tracking to Analytics, response status was {} with url {} and parameters {}", status, url, getParamsString(parameters));
                }

            } catch (Exception e) {
                log.error("An error occurred when sending tracking to Analytics", e);
            }
        });
    }

    private String getParamsString(Map<String, String> params)
            throws UnsupportedEncodingException {
        StringBuilder result = new StringBuilder();

        for (Map.Entry<String, String> entry : params.entrySet()) {
            result.append(URLEncoder.encode(entry.getKey(), "UTF-8"));
            result.append("=");
            result.append(URLEncoder.encode(entry.getValue(), "UTF-8"));
            result.append("&");
        }

        String resultString = result.toString();
        return resultString.length() > 0
                ? resultString.substring(0, resultString.length() - 1)
                : resultString;
    }

}