package com.mediahub.core.services;

import org.apache.sling.api.resource.Resource;

import java.util.Map;


public interface AnalyticsTrackingService {

    void trackExternal(Resource asset, String format, Map<String, String> properties);

    void trackInternal(Resource asset, String format, Map<String, String> properties);

}
