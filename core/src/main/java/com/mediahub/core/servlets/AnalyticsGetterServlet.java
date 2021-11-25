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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.Servlet;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
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
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/getanalytics"})
public class AnalyticsGetterServlet extends SlingAllMethodsServlet {
    
    private static final long serialVersionUID = 1L;

    private static final Logger LOGGER = LoggerFactory.getLogger(AnalyticsGetterServlet.class);
    
    private DateFormat inputDateFormat = new SimpleDateFormat(DATE_INPUT_FORMAT);
    private static final String DATE_INPUT_FORMAT = "dd-MM-yyyy";
    
    @Reference
    private AnalyticsGetterService analyticsService;
    
    @Override
    protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
    	LOGGER.info("Starting Analytics getter servlet");
    	
    	Date startDate = null, endDate = null;
    	String action = request.getParameter("action");
    	String startDateStr = request.getParameter("startDate");
		String endDateStr = request.getParameter("endDate");
		
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
    		LOGGER.error("Failed to parse date", e);
    	}
    	
    	response.setContentType("application/json");
    	
    	if (StringUtils.isNotEmpty(action)) {
    		try {
    			String rootPath = request.getParameter("resource");
	    		Method method = analyticsService.getClass().getMethod(action, String.class, Date.class, Date.class);
	    		
	    		response.getWriter().print(method.invoke(analyticsService, rootPath, startDate, endDate));
	    		
    		} catch (NoSuchMethodException | SecurityException e) {
    			List<String> actions = Arrays.asList(analyticsService.getClass().getMethods()).stream()
    					.map(m -> m.getName())
    					.filter(n -> n.startsWith("get") && !StringUtils.equalsAny(n, "getCustomReport", "getClass"))
    					.collect(Collectors.toList());
    			LOGGER.error("Could not find the requested action", e);
    			response.getWriter().print("Could not find the requested action. Available actions are: " + String.join(", ", actions));
    		} catch (InvocationTargetException | IllegalAccessException | IllegalArgumentException e) {
    			LOGGER.error("Error trying to call action", e);
    			response.getWriter().print("Error trying to call action");
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
    	
        LOGGER.info("Successfully executed analytics service");
    }
}
