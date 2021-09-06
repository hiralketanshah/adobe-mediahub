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
package com.mediahub.core.filters;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.http.whiteboard.HttpWhiteboardConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

@Component(service = Filter.class,
        property = {
                HttpWhiteboardConstants.HTTP_WHITEBOARD_CONTEXT_SELECT + "=(" + HttpWhiteboardConstants.HTTP_WHITEBOARD_CONTEXT_NAME + "=org.apache.sling)",
                HttpWhiteboardConstants.HTTP_WHITEBOARD_FILTER_PATTERN + "=/",
        }
)
public class GlobalFilter implements Filter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    public static final String IP_ADDRESS_PROPERTY = "IP_ADDRESS";
    public static final String USER_AGENT_PROPERTY = "USER_AGENT";
    public static final String LANGUAGE_PROPERTY = "LANGUAGE";
    public static final String REFERER_PROPERTY = "REFERER";

    private static final ThreadLocal<Map<String, String>> threadLocal = new ThreadLocal<>();

    @Override
    public void doFilter(final ServletRequest servletRequest, final ServletResponse servletResponse,
                         final FilterChain filterChain) throws IOException, ServletException {
        try {
            HttpServletRequest request = (HttpServletRequest) servletRequest;
            Map<String, String> globalProperties = new HashMap<>();
            String ipAddress = request.getHeader("X-Forwarded-For");
            if (ipAddress == null) {
                ipAddress = request.getRemoteAddr();
            }
            globalProperties.put(IP_ADDRESS_PROPERTY, ipAddress.split(",")[0].trim());
            globalProperties.put(USER_AGENT_PROPERTY, request.getHeader("User-Agent"));
            globalProperties.put(LANGUAGE_PROPERTY, request.getHeader("Accept-Language"));
            globalProperties.put(REFERER_PROPERTY, request.getHeader("Referer"));
            setGlobalProperties(globalProperties);
            filterChain.doFilter(servletRequest, servletResponse);
        } finally {
            removeGlobalProperties();
        }
    }

    public static Map<String, String> getGlobalProperties() {
        return threadLocal.get();
    }

    public static void setGlobalProperties(Map<String, String> properties) {
        threadLocal.set(properties);
    }

    public static void removeGlobalProperties() {
        threadLocal.remove();
    }

    @Override
    public void init(FilterConfig filterConfig) {
    }

    @Override
    public void destroy() {
    }

}