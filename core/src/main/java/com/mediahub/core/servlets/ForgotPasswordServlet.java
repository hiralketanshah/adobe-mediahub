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

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.UserUtils;
import org.apache.commons.lang.LocaleUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.Value;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import java.io.IOException;
import java.util.*;

/**
 * Servlet that writes token in user node and to send reset password link. It is mounted for
 * all resources of a specific Sling resource type. The
 * {@link SlingSafeMethodsServlet} shall be used for HTTP methods that are
 * idempotent. For write operations use the {@link SlingAllMethodsServlet}.
 */
@Component(service = Servlet.class,
        property = {
                "sling.servlet.methods=" + HttpConstants.METHOD_POST,
                "sling.servlet.resourceTypes=" + "granite/core/components/login",
                "sling.servlet.selectors=" + "sendrestpassowrdemail",
                "sling.servlet.extensions=" + "json"})
@ServiceDescription("To send Forgot Password Link")
public class ForgotPasswordServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(ForgotPasswordServlet.class);

    private static final long serialVersionUID = 1L;

    @Reference
    private transient Externalizer externalizer;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Reference
    private transient GenericEmailNotification genericEmailNotification;

    @SuppressWarnings("CQRules:AMSCORE-553")
    @Reference
    private transient SlingSettingsService slingSettingsService;

    @Reference
    private transient I18nProvider provider;

    @Override
    protected void doPost(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws ServletException, IOException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {

            String userName = request.getRequestParameterMap().getValue("j_username").getString();
            Authorizable userAuthorizable = resolver.adaptTo(UserManager.class).getAuthorizable(userName);

            if (null != userAuthorizable) {
                String userPath = userAuthorizable.getPath();
                Resource user = resolver.getResource(userPath);
                ModifiableValueMap modifiableValueMap = user.adaptTo(ModifiableValueMap.class);

                Value[] emails = userAuthorizable.getProperty("./profile/email");
                Value[] firstname = userAuthorizable.getProperty("./profile/givenName");
                String userToken = UUID.randomUUID().toString();
                modifiableValueMap.put("userToken", userToken);
                Calendar tokenExpiryDate = Calendar.getInstance();
                tokenExpiryDate.add(Calendar.DATE, 1);
                modifiableValueMap.put("tokenExpiryDate", tokenExpiryDate);

                Map<String, String> emailParams = new HashMap<>();
                if (null != emails && emails.length > 0) {
                    String email = emails[0].getString();
                    String[] emailRecipients = {email};

                    String language = UserUtils.getUserLanguage(user);
                    Locale locale = LocaleUtils.toLocale(language);

                    String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Forgot password link //  Mot de passe oublié", locale);
                    emailParams.put(BnpConstants.SUBJECT, subject);
                    emailParams.put("firstname", firstname[0].getString());
                    emailParams.put(BnpConstants.LINK, externalizer.authorLink(resolver, BnpConstants.CHANGE_PASSWORD_RESOURCE_PATH + userToken));
                    genericEmailNotification.sendEmail(BnpConstants.CHANGE_PASSWORD_EMAIL_TEMPLATE, emailRecipients, emailParams);
                }
                resolver.commit();
            } else {
                setErrorResponse(response, "User ID not found");
            }

        } catch (LoginException | RepositoryException e) {
            LOGGER.error("Error while fecthing system user : {0}", e);
            setErrorResponse(response, "Error which accessing repository");
        }

    }

    /**
     * @param response
     * @param message
     * @throws IOException
     */
    private void setErrorResponse(SlingHttpServletResponse response, String message) throws IOException {
        response.getWriter().print("User ID not found");
        response.setStatus(500);
    }
}
