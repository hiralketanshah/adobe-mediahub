package com.mediahub.core.services.impl;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.services.AnalyticsTrackingService;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.HttpClients;
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

import java.util.ArrayList;
import java.util.List;
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
    private HttpClient httpClient;

    @Activate
    protected void activate(Config config) {
        this.executorService = Executors.newCachedThreadPool();
        this.config = config;
        this.httpClient = HttpClients.createDefault();
    }

    @Deactivate
    protected void deactivate(Config config) {
        this.executorService.shutdown();
    }

    @Override
    public void trackExternal(Resource asset, String format) {
        String url = "http://" + this.config.namespace() + ".sc.omtrdc.net/b/ss/" + this.config.report_suite() + "/0?" + buildParameters(asset);
        sendRequest(url);
    }

    @Override
    public void trackInternal(Resource asset, String format) {
        String url = "http://" + this.config.namespace() + ".sc.omtrdc.net/b/ss/" + this.config.report_suite() + "/0?" + buildParameters(asset);
        sendRequest(url);
    }

    private void sendRequest(String url) {
        this.executorService.execute(() -> {
            try {
                log.debug("Tracking URL is {}", url);
                HttpGet httpGet = new HttpGet(url);
                this.httpClient.execute(httpGet);
            } catch (Exception e) {
                log.error("An error occurred when sending tracking to Analytics " + url, e);
            }
        });
    }

    private String buildParameters(Resource asset) {
        List<String> parameters = new ArrayList<>();
        parameters.add(String.join("=", "c.a.assets.source", "AEM"));
        parameters.add(String.join("=", "c.a.assets.idlist", asset.getValueMap().get(JcrConstants.JCR_UUID, String.class)));
        parameters.add(String.join("=", "pe", "o"));
        parameters.add(String.join("=", "pev2", "Asset%20Insight%20Event"));
        parameters.add(String.join("=", "vid", "1234567890123456-6543210987654321"));
        return String.join("&", parameters.toArray(new String[5]));
    }
}