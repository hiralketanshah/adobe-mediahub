package com.mediahub.core.services;

import org.apache.sling.api.resource.Resource;


public interface AnalyticsTrackingService {

    void trackExternal(Resource asset, String format);

    void trackInternal(Resource asset, String format);

}
