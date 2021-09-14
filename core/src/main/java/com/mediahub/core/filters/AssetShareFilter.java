package com.mediahub.core.filters;

import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.auth.core.spi.AuthenticationHandler;
import org.apache.sling.auth.core.spi.AuthenticationInfo;
import org.apache.sling.engine.EngineConstants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.component.propertytypes.ServiceRanking;
import org.osgi.service.component.propertytypes.ServiceVendor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;

/**
 * Servlet filter to handle the share asset request.
 */


@Component(service = {AuthenticationHandler.class},
        property = {
                AuthenticationHandler.PATH_PROPERTY + "=" + AssetShareFilter.SHARE_PAGE_PATH_VANITY,
                EngineConstants.SLING_FILTER_SCOPE + "=" + EngineConstants.FILTER_SCOPE_REQUEST,
                EngineConstants.SLING_FILTER_PATTERN + "=" + "/linkshare.*",
                EngineConstants.SLING_FILTER_EXTENSIONS + "=" + "html",
                EngineConstants.SLING_FILTER_METHODS + "=" + "GET",
                "service.ranking" + "=" + Integer.MAX_VALUE
        })
@ServiceDescription("To filter incoming share asset requests")
@ServiceRanking(Integer.MAX_VALUE)
@ServiceVendor("Adobe")
public class AssetShareFilter implements AuthenticationHandler {

    protected static final String SHARE_PAGE_PATH_VANITY = "/linkshare.html";

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference(target = "(service.pid=com.day.crx.security.token.impl.impl.TokenAuthenticationHandler)")
    private AuthenticationHandler wrappedAuthHandler;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Override
    public AuthenticationInfo extractCredentials(HttpServletRequest request,
                                                 HttpServletResponse response) {
        final Map<String, Object> authInfo = Collections
                .singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");
        String sh = request.getParameterMap().get("sh")[0];
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            Resource resource = resolver.getResource("/var/dam/share/" + sh.split("\\.")[0]);
            boolean isSecured = resource.getValueMap().get("secured", Boolean.FALSE);
            boolean isLogged = Boolean.FALSE;
            Cookie[] cookies = request.getCookies();
            if (cookies != null) {
                for (Cookie cookie : cookies) {
                    if (StringUtils.equals(cookie.getName(), "login-token")) {
                        isLogged = Boolean.TRUE;
                        break;
                    }
                }
            }
            if (isSecured && !isLogged) {
                String newURI = "/apps/granite/core/content/login.html" + "?resource=%2Flinkshare.html%3Fsh=" + sh + "%26redirected%3Dtrue";
                response.sendRedirect(newURI);
            }
        } catch (LoginException ex) {
            logger.error("Error while fetching service user {0}", ex);
        } catch (IOException e) {
            logger.error("Error while reading data from request {0}", e);
        }

        return null;
    }

    @Override
    public boolean requestCredentials(HttpServletRequest httpServletRequest,
                                      HttpServletResponse httpServletResponse) throws IOException {
        return false;
    }

    @Override
    public void dropCredentials(HttpServletRequest httpServletRequest,
                                HttpServletResponse httpServletResponse) throws IOException {
        // Methods to be implemented for the  Authentication handler interface
    }
}