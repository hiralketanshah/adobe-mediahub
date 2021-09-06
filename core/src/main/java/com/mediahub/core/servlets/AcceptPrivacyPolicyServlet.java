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
import java.io.IOException;
import java.security.Principal;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.apache.sling.event.jobs.JobManager;
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
@Component(
        service = Servlet.class,
        property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST, "sling.servlet.paths=" + "/bin/mediahub/privacy"})
public class AcceptPrivacyPolicyServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(AcceptPrivacyPolicyServlet.class);

    private static final long serialVersionUID = 1L;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Reference
    private transient JobManager jobManager;

    @Override
    protected void doPost(final SlingHttpServletRequest request,
                          final SlingHttpServletResponse response) throws ServletException, IOException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            Principal userPrincipal = request.getUserPrincipal();
            String userPath = resolver.adaptTo(UserManager.class).getAuthorizable(userPrincipal).getPath();
            Resource user = resolver.getResource(userPath);
            if (null != user) {
                ModifiableValueMap modifiableValueMap = user.adaptTo(ModifiableValueMap.class);
                modifiableValueMap.put(BnpConstants.PRIVACY_ACCEPTED_DATE, Calendar.getInstance());
                String welcomeEmailSent = modifiableValueMap.get(BnpConstants.WELCOME_EMAIL_SENT, Boolean.FALSE.toString());
                if(StringUtils.equals(welcomeEmailSent, Boolean.FALSE.toString())){
                    modifiableValueMap.put(BnpConstants.WELCOME_EMAIL_SENT, Boolean.TRUE.toString());
                    final Map<String, Object> properties = new HashMap<>();
                    properties.put(BnpConstants.FIRST_NAME,userPrincipal.getName());
                    Resource profile = user.getChild(BnpConstants.PROFILE);
                    if(null != profile && StringUtils.equals(profile.getValueMap().get(BnpConstants.TYPE, StringUtils.EMPTY), BnpConstants.BROADCAST_VALUE_INTERNAL) ){
                        if(profile.getValueMap().containsKey(BnpConstants.EMAIL)){
                            properties.put(BnpConstants.EMAIL,user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL, StringUtils.EMPTY));
                        } else {
                            properties.put(BnpConstants.EMAIL,user.getValueMap().get("rep:authorizableId", StringUtils.EMPTY));
                        }
                        jobManager.addJob("user/welcome/email", properties);
                    }

                }
            }
            resolver.commit();
        } catch (LoginException | RepositoryException e) {
            LOGGER.error("Error while fecthing system user : {0}", e);
        }
    }
}
