package com.mediahub.core.filters;

import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.security.Principal;
import java.util.Calendar;
import javax.jcr.RepositoryException;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
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
                EngineConstants.SLING_FILTER_PATTERN + "=" + "/.*.html.*",
                EngineConstants.SLING_FILTER_METHODS + "=" + "GET"
        })
@ServiceDescription("To filter incoming asset or page requests")
@ServiceRanking(-700)
@ServiceVendor("Adobe")
public class LoginPageFilter implements Filter {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Override
    public void doFilter(final ServletRequest request, final ServletResponse response,
                         final FilterChain filterChain) throws IOException, ServletException {

        final SlingHttpServletRequest slingRequest = (SlingHttpServletRequest) request;
        final SlingHttpServletResponse slingResponse = (SlingHttpServletResponse) response;
        String requestURI = slingRequest.getRequestURI();

        Principal userPrincipal = slingRequest.getUserPrincipal();

        try {
            if (null != userPrincipal && !StringUtils.contains(requestURI, "/apps/mediahub/content/privacypolicy.html") && !StringUtils.contains(requestURI, "/apps/granite/core/content/login.html")) {
                UserManager userManager = slingRequest.getResourceResolver().adaptTo(UserManager.class);
                Authorizable authorizable = userManager.getAuthorizable(userPrincipal);
                // mediahub-administrators group
                Authorizable mediahubBasic = userManager.getAuthorizable("mediahub-basic");
                Group mediahubBasicGroup = (Group) mediahubBasic;
                Resource privacyPolicyStep1 = slingRequest.getResourceResolver().getResource("/content/dam/technique/mediahub/privacy-policy/privacy-policy-step1");
                Resource privacyPolicyStep2 = slingRequest.getResourceResolver().getResource("/content/dam/technique/mediahub/privacy-policy/privacy-policy-step2");
                if ( (!((User) authorizable).isAdmin()) &&  mediahubBasicGroup != null && mediahubBasicGroup.isMember(authorizable) && privacyPolicyStep1 != null && privacyPolicyStep2 != null ) {
                    redirectUserForPrivacyPolicy(slingResponse, authorizable, privacyPolicyStep1, privacyPolicyStep2);
                }

            }
        } catch (RepositoryException e) {
            logger.error("Error while accessing Repository {0}", e);
        }

        logger.debug("requestURI {}", requestURI);
        filterChain.doFilter(request, response);
    }

    /**
     * Redirect users based on Privacy Policy
     *
     * @param slingResponse
     * @param authorizable
     * @param privacyPolicyStep1
     * @param privacyPolicyStep2
     * @throws RepositoryException
     * @throws IOException
     */
    private void redirectUserForPrivacyPolicy(SlingHttpServletResponse slingResponse,
        Authorizable authorizable, Resource privacyPolicyStep1, Resource privacyPolicyStep2)
        throws RepositoryException, IOException {
        ValueMap contentNodeOfStep1 = privacyPolicyStep1.getChild(JcrConstants.JCR_CONTENT).getValueMap();
        ValueMap contentNodeOfStep2 = privacyPolicyStep2.getChild(JcrConstants.JCR_CONTENT).getValueMap();
        if (authorizable.getProperty(BnpConstants.PRIVACY_ACCEPTED_DATE) != null && authorizable.getProperty(BnpConstants.PRIVACY_ACCEPTED_DATE).length > 0) {
            Calendar lastModifiedStep1 = contentNodeOfStep1.get(JcrConstants.JCR_LASTMODIFIED, Calendar.class);
            Calendar lastModifiedStep2 = contentNodeOfStep2.get(JcrConstants.JCR_LASTMODIFIED, Calendar.class);
            Calendar privacyAcceptedDate = authorizable.getProperty(BnpConstants.PRIVACY_ACCEPTED_DATE)[0].getDate();
            if (lastModifiedStep1 != null && (privacyAcceptedDate.before(lastModifiedStep1) || privacyAcceptedDate.before(lastModifiedStep2)) ) {
                redirectUser(slingResponse);
            }
        } else {
            redirectUser(slingResponse);
        }
    }

    /**
     * Redirecting user to privacy policy page
     *
     * @param slingResponse
     * @throws IOException
     */
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