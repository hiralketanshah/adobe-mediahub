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

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Servlet that writes some sample content into the response. It is mounted for
 * all resources of a specific Sling resource type. The
 * {@link SlingSafeMethodsServlet} shall be used for HTTP methods that are
 * idempotent. For write operations use the {@link SlingAllMethodsServlet}.
 */
@SuppressWarnings("CQRules:CQBP-75")
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST, "sling.servlet.paths=" + "/bin/mediahub/downloadmetadata"})
public class DownloadAssetMetadataServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(DownloadAssetMetadataServlet.class);

    private static final long serialVersionUID = 1L;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Override
    protected void doPost(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws ServletException, IOException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            RequestParameter[] paths = request.getRequestParameterMap().getOrDefault(BnpConstants.PATH, new RequestParameter[]{});
            for(RequestParameter path : paths){
                String charset = request.getRequestParameter("_charset_").getString();
                Resource asset = resolver.getResource(URLDecoder.decode(path.getString(),charset));
                List<String> value = new ArrayList<>();
                if(null != asset && asset.getChild(JcrConstants.JCR_CONTENT) != null && asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null){
                    ModifiableValueMap values = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).adaptTo(ModifiableValueMap.class);
                    value.add(getParameterValue(request, charset, "dateOfUse"));
                    value.add(getParameterValue(request, charset, "useTextArea"));
                    value.add(request.getResourceResolver().getUserID());
                    String users = StringUtils.EMPTY;
                    if(null != request.getRequestParameterMap().getValues("internalUserContact")){
                        users = String.join(",", request.getParameterMap().getOrDefault("internalUserContact", new String[]{}));
                    }
                    value.add(users);
                    value.add(ProjectExpireNotificationUtil.getCurrentDate(new SimpleDateFormat(BnpConstants.DD_MM_YYYY)).toString());
                    String areas = StringUtils.EMPTY;
                    if(null != request.getRequestParameterMap().getValues(BnpConstants.GEOGRAPHICALAREA)){
                        areas = String.join(",", request.getParameterMap().getOrDefault(BnpConstants.GEOGRAPHICALAREA, new String[]{}));
                    }
                    value.add(areas);

                    if(values.containsKey(BnpConstants.DOWNLOAD_DETAILS)){
                        String[] downloadDetails = values.get(BnpConstants.DOWNLOAD_DETAILS, new String[]{});
                        List<String> list = new ArrayList<>(Arrays.asList(downloadDetails));
                        list.add(String.join("|", value));
                        values.put(BnpConstants.DOWNLOAD_DETAILS, list.toArray(new String[list.size()]));
                    } else {
                        values.put(BnpConstants.DOWNLOAD_DETAILS, new String[]{String.join("|", value)});
                    }
                    values.put(BnpConstants.DOWNLOAD_COUNT, (long)values.get(BnpConstants.DOWNLOAD_COUNT, 0) +1);
                }
            }
            resolver.commit();
        } catch (LoginException e) {
            LOGGER.error("Error while fecthing system user : {0}", e);
        }
    }

    private String getParameterValue(SlingHttpServletRequest request, String charset, String dateOfUse) throws UnsupportedEncodingException {
        if(request.getRequestParameter(dateOfUse) != null){
            return request.getRequestParameter(dateOfUse).getString(charset);
        } else {
            return StringUtils.EMPTY;
        }
    }
}
