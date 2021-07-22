package com.mediahub.core.filters;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.security.Principal;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
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
        EngineConstants.SLING_FILTER_PATTERN + "=" + "/.*.html",
        EngineConstants.SLING_FILTER_METHODS + "=" + "GET"
    })
@ServiceDescription("To filter incoming asset or page requests")
@ServiceRanking(-700)
@ServiceVendor("Adobe")
public class LoginPageFilter implements Filter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Override
    public void doFilter(final ServletRequest request, final ServletResponse response,
                         final FilterChain filterChain) throws IOException, ServletException {

        final SlingHttpServletRequest slingRequest = (SlingHttpServletRequest) request;
        final SlingHttpServletResponse slingResponse = (SlingHttpServletResponse) response;
        String requestURI = slingRequest.getRequestURI();

        Principal userPrincipal = slingRequest.getUserPrincipal();

        try {
            if( null != userPrincipal && !StringUtils.contains(requestURI, "/apps/mediahub/content/privacypolicy.html") && !StringUtils.contains(requestURI, "/apps/granite/core/content/login.html") ){
                UserManager userManager = slingRequest.getResourceResolver().adaptTo(UserManager.class);
                Authorizable authorizable = userManager.getAuthorizable(userPrincipal);

                if(authorizable.getProperty("privacyAccepted") != null && authorizable.getProperty("privacyAccepted").length > 0){
                    boolean isPrivacyAgreed = authorizable.getProperty("privacyAccepted")[0].getBoolean();
                    if(!isPrivacyAgreed){
                        redirectUser(slingResponse);
                    }
                }else {
                    redirectUser(slingResponse);
                }
            }
        } catch (RepositoryException e) {
            logger.error("Error while accessing Repository {0}" , e);
        }

        logger.debug("requestURI {}" , requestURI);
        filterChain.doFilter(request, response);
    }

    private void redirectUser(SlingHttpServletResponse slingResponse) throws IOException {
        String newURI = "/apps/mediahub/content/privacypolicy.html";
        slingResponse.sendRedirect(newURI);
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