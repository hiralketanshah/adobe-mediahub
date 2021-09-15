package com.mediahub.core.services.impl;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.filters.GlobalFilter;
import com.mediahub.core.services.AnalyticsTrackingService;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ValueMap;
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
import java.util.Arrays;
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
    public void trackExternal(Resource asset, String format, Map<String, String> properties) {
        Map<String, String> parameters = initParameters(asset);
        sendRequest(parameters, properties);
    }

    @Override
    public void trackInternal(Resource asset, String format, Map<String, String> properties) {
        Map<String, String> parameters = initParameters(asset);
        sendRequest(parameters, properties);
    }

    private Map<String, String> initParameters(Resource asset) {
        ValueMap metadata = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
        Map<String, String> parameters = new HashMap<>();
        parameters.put("c.a.assets.source", "AEM");
        parameters.put("c.a.assets.idlist", asset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
        parameters.put("pe", "o");
        parameters.put("pev2", "Asset Insight Event");
        parameters.put("events", "event1");

        //Fill in assets properties
        if (metadata.containsKey(BnpConstants.BNPP_CONFIDENTIALITY)) {
            parameters.put("prop1", formatMetadata(metadata.get(BnpConstants.BNPP_CONFIDENTIALITY)));
        }
        if (metadata.containsKey(BnpConstants.BNPP_LANGUAGE)) {
            parameters.put("prop2", formatMetadata(metadata.get(BnpConstants.BNPP_LANGUAGE)));
        }
        if (metadata.containsKey(BnpConstants.DAM_FILE_FORMAT)) {
            parameters.put("prop3", formatMetadata(metadata.get(BnpConstants.DAM_FILE_FORMAT)));
        }
        if (metadata.containsKey(BnpConstants.BNPP_SUBTITLES)) {
            parameters.put("prop4", formatMetadata(metadata.get(BnpConstants.BNPP_SUBTITLES)));
        }
        if (metadata.containsKey(BnpConstants.BNPP_SUBTITLE_LANGUAGES)) {
            parameters.put("prop5", formatMetadata(metadata.get(BnpConstants.BNPP_SUBTITLE_LANGUAGES)));
        }
        if (metadata.containsKey(BnpConstants.BNPP_BROADCAST_STATUS)) {
            parameters.put("prop13", formatMetadata(metadata.get(BnpConstants.BNPP_BROADCAST_STATUS)));
        }

        //Fill in media properties
        Resource media = asset.getParent();
        if (media.isResourceType(BnpConstants.DAM_SLING_FOLDER)) {
            ValueMap mediaMetadata = media.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_TYPE)) {
                parameters.put("prop6", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_TYPE)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_CATEGORY)) {
                parameters.put("prop7", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_CATEGORY)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_THEME)) {
                parameters.put("prop8", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_THEME)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_ENTITIES)) {
                parameters.put("prop9", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_ENTITIES)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_GEOGRAPHICAL)) {
                parameters.put("prop10", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_GEOGRAPHICAL)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_COUNTRY)) {
                parameters.put("prop11", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_COUNTRY)));
            }
            if (mediaMetadata.containsKey(BnpConstants.BNPP_MEDIA_SPONSOR)) {
                parameters.put("prop12", formatMetadata(mediaMetadata.get(BnpConstants.BNPP_MEDIA_SPONSOR)));
            }

        }

        return parameters;
    }

    private String formatMetadata(Object metadata) {
        if (metadata.getClass().isArray()) {
            return convertArrayToString((String[]) metadata);
        } else {
            return (String) metadata;
        }
    }

    private String convertArrayToString(String[] array) {
        return String.join(",", Arrays.asList(array));
    }

    private void sendRequest(Map<String, String> parameters, Map<String, String> properties) {
        this.executorService.execute(() -> {
            try {

                String baseUrl = "http://" + this.config.namespace() + ".sc.omtrdc.net/b/ss/" + this.config.report_suite() + "/0";

                URL url = new URL(baseUrl);
                HttpURLConnection con = (HttpURLConnection) url.openConnection();
                con.setRequestMethod("GET");
                con.setDoOutput(true);
                con.setConnectTimeout(5000);
                con.setReadTimeout(5000);

                if (!StringUtils.isEmpty(properties.get(GlobalFilter.IP_ADDRESS_PROPERTY))) {
                    con.setRequestProperty("X-Forwarded-For", properties.get(GlobalFilter.IP_ADDRESS_PROPERTY));
                }
                if (!StringUtils.isEmpty(properties.get(GlobalFilter.LANGUAGE_PROPERTY))) {
                    con.setRequestProperty("Accept-Language", properties.get(GlobalFilter.LANGUAGE_PROPERTY));
                }
                if (!StringUtils.isEmpty(properties.get(GlobalFilter.USER_AGENT_PROPERTY))) {
                    con.setRequestProperty("User-Agent", properties.get(GlobalFilter.USER_AGENT_PROPERTY));
                }
                if (!StringUtils.isEmpty(properties.get(GlobalFilter.REFERER_PROPERTY))) {
                    parameters.put("r", properties.get(GlobalFilter.REFERER_PROPERTY));
                }

                //Setting parameters
                DataOutputStream out = new DataOutputStream(con.getOutputStream());
                out.writeBytes(getParamsString(parameters));
                out.flush();
                out.close();

                log.trace("Sending hit to {} with parameters {}", url, getParamsString(parameters));
                int status = con.getResponseCode();
                if (status != HttpURLConnection.HTTP_OK) {
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