package com.mediahub.core.services;

import java.util.Date;

import org.apache.sling.api.resource.Resource;

import com.google.gson.JsonObject;


public interface AnalyticsGetterService {

	JsonObject updateMetrics(Resource asset, Date startDate, Date endDate);

}
