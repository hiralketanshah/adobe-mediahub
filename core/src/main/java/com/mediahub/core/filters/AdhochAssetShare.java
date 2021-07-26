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

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.engine.EngineConstants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.component.propertytypes.ServiceRanking;
import org.osgi.service.component.propertytypes.ServiceVendor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple servlet filter component that logs incoming requests.
 */
@Component(service = Filter.class,
           property = {
                EngineConstants.SLING_FILTER_SCOPE + "=" + EngineConstants.FILTER_SCOPE_REQUEST,
                EngineConstants.SLING_FILTER_SELECTORS + "=" + "adhocassetshare",
                EngineConstants.SLING_FILTER_METHODS + "=" +  "POST",
                EngineConstants.SLING_FILTER_EXTENSIONS + "=" + "html"
           })
@ServiceDescription("To filter incoming share asset requests")
@ServiceRanking(-700)
@ServiceVendor("Adobe")
public class AdhochAssetShare implements Filter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Override
    public void doFilter(final ServletRequest request, final ServletResponse response,
                         final FilterChain filterChain) throws IOException, ServletException {

        final SlingHttpServletRequest slingRequest = (SlingHttpServletRequest) request;
        String requestURI = slingRequest.getRequestURI();

        logger.debug("requestURI {}" , requestURI);

        if(slingRequest.getRequestParameterMap().getValue("shareLink") != null){
            String shareLink = slingRequest.getRequestParameterMap().getValue("shareLink").getString();

            try {

                List<NameValuePair> params = URLEncodedUtils.parse(new URI(shareLink), Charset.forName("UTF-8"));
                Map<String, String> keyValueMap = new HashMap<>();
                for(NameValuePair param : params){
                    keyValueMap.put(param.getName(), param.getValue());
                }

                String secured = keyValueMap.getOrDefault("secured", Boolean.FALSE.toString());
                String sh = keyValueMap.getOrDefault("sh", StringUtils.EMPTY);

                if( StringUtils.equals(secured, Boolean.TRUE.toString()) && StringUtils.isNotBlank(sh) ){
                    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");
                    setSecuredProperty(sh, authInfo);
                }
            } catch (URISyntaxException e) {
                logger.error("Error while framing URI {0}", e);
            }
        }


        filterChain.doFilter(request, response);
    }

    private void setSecuredProperty(String sh, Map<String, Object> authInfo)
        throws PersistenceException {
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            Resource resource = resolver.getResource("/var/dam/share/" + sh.split("\\.")[0]);
            ModifiableValueMap modifiableValueMap = resource.adaptTo(ModifiableValueMap.class);
            modifiableValueMap.put("secured", Boolean.TRUE);
            resolver.commit();
        } catch (LoginException e) {
            logger.error("Error while fetching service user {0}", e);
        }
    }

    @Override
    public void init(FilterConfig filterConfig) {
        // overridden method for filter interface
    }

    @Override
    public void destroy() {
        // overridden method for filter interface
    }

}