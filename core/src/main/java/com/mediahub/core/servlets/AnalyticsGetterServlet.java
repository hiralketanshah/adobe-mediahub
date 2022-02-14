/*
 *  Copyright 2015 Adobe Systems Incorporated
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.mediahub.core.servlets;

import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.Servlet;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.services.AnalyticsGetterService;

/**
 * Servlet to test analytics getter service
 */
@SuppressWarnings("CQRules:CQBP-75")
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/getanalytics"})
public class AnalyticsGetterServlet extends SlingAllMethodsServlet {
	
	private static final long serialVersionUID = 1L;
    
	private static final Logger logger = LoggerFactory.getLogger(AnalyticsGetterServlet.class);
	
	private static final String ACTION_PARAM = "dimension";
    private static final String END_DATE_PARAM = "endDate";
	private static final String START_DATE_PARAM = "startDate";
	private static final String ROOT_PATH_PARAM = "path";
	private static final String APPLICATION_JSON_CONTENT_TYPE = "application/json";
	
    private DateFormat inputDateFormat = new SimpleDateFormat(DATE_INPUT_FORMAT);
    private static final String DATE_INPUT_FORMAT = "dd-MM-yyyy";
    
    @Reference
    private transient AnalyticsGetterService analyticsService;
    
    @Override
    protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
    	logger.debug("Starting Analytics getter servlet");
    	
    	Date startDate = null, endDate = null;
    	String dimension = request.getParameter(ACTION_PARAM);
    	String startDateStr = request.getParameter(START_DATE_PARAM);
		String endDateStr = request.getParameter(END_DATE_PARAM);
		
    	try {
	    	if (startDateStr != null) {
				startDate = inputDateFormat.parse(startDateStr);
			} else {
				startDate = new Date();
			}
	    	
	    	if (endDateStr != null) {
				endDate = inputDateFormat.parse(endDateStr);
			} else {
				endDate = new Date();
			}
    	} catch (ParseException e) {
    		logger.error("Failed to parse date", e);
    	}
    	
    	response.setContentType(APPLICATION_JSON_CONTENT_TYPE);
    	
    	if (StringUtils.isNotEmpty(dimension)) {
			String rootPath = request.getParameter(ROOT_PATH_PARAM);
    		
    		try {
				response.getWriter().print(analyticsService.getDimension(rootPath, dimension, startDate, endDate));
			} catch (IOException | LoginException e) {
				logger.error("Error trying to call service", e);
    			response.getWriter().print("Error trying to call service");
			}
    	}
    	else {
    		Map<String, String> parametersMap = new HashMap<String, String>();
    		Map<String, RequestParameter[]> requestMap = request.getRequestParameterMap();
    		for (String paramKey : requestMap.keySet()) {
    			RequestParameter[] reqParam = requestMap.get(paramKey);
    			parametersMap.put(paramKey, reqParam[0].getString());
    		}
    		
    		response.getWriter().print(analyticsService.getCustomReport(parametersMap, startDate, endDate));
    	}
    	
        logger.debug("Successfully executed analytics service");
    }
}
