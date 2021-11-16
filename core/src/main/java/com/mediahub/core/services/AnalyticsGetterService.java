package com.mediahub.core.services;

import java.util.Date;

import org.apache.sling.api.resource.Resource;
import org.json.JSONObject;


public interface AnalyticsGetterService {

	JSONObject updateMetrics(Resource asset, Date startDate, Date endDate);

}
