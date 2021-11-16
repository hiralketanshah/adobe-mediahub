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

import javax.servlet.Servlet;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
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
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/testanalytics"})
public class AnalyticsTesterServlet extends SlingAllMethodsServlet {
    
    private static final long serialVersionUID = 1L;
    private static final String DATE_INPUT_FORMAT = "dd-MM-yyyy";

    private static final Logger LOGGER = LoggerFactory.getLogger(AnalyticsTesterServlet.class);
    
    @Reference
    private AnalyticsGetterService analyticsService;
    
    @Override
    protected void doGet(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
    	LOGGER.info("Starting Analytics tester servlet");
    	DateFormat dateFormat = new SimpleDateFormat(DATE_INPUT_FORMAT);
    	ResourceResolver resourceResolver = request.getResourceResolver();
    	
    	String resourcePath = request.getRequestParameter("resource").getString();
    	String startDateStr = request.getRequestParameter("startDate").getString();
    	String endDateStr = request.getRequestParameter("endDate").getString();
    	
    	Date startDate = null, endDate = null;
		try {
			startDate = dateFormat.parse(startDateStr);
			endDate = dateFormat.parse(endDateStr);
		} catch (ParseException e) {
			LOGGER.error("Failed to parse date", e);
		}
    	
    	Resource resource = resourceResolver.getResource(resourcePath);
    	
    	response.setContentType("application/json");
        response.getWriter().print(analyticsService.updateMetrics(resource, startDate, endDate));
        
        LOGGER.info("Successfully executed analytics service");
    }
}
