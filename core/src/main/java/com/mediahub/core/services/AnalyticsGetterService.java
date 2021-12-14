package com.mediahub.core.services;

import java.io.IOException;
import java.util.Date;
import java.util.Map;

import org.apache.sling.api.resource.LoginException;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;


public interface AnalyticsGetterService {
	
	/**
     * Custom call by template + parameters
     */
	JsonElement getCustomReport(Map<String, String> parametersMap, Date startDate, Date endDate);

	/**
	 * Getting dimension data
	 */
	JsonArray getDimension(String folderId, String dimension, Date startDate, Date endDate) throws IOException, LoginException;
}
