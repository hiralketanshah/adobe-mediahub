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
import java.util.Iterator;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.servlet.Servlet;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.event.jobs.JobManager;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.osgi.framework.Constants;

@SuppressWarnings("CQRules:CQBP-75")
@Component(service = Servlet.class, property = {
		Constants.SERVICE_DESCRIPTION + "=Check if the current user is an external user",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/allowPublicCollection" })
public class AllowPublicCollectionCreationServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(AllowPublicCollectionCreationServlet.class);

    private static final long serialVersionUID = 1L;

    @Reference
    private transient ResourceResolverFactory resolverFactory;

    @Reference
    private transient JobManager jobManager;

    @Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {
		try {
			ResourceResolver resolver = request.getResourceResolver();
			UserManager userManager = resolver.adaptTo(UserManager.class);
			Session session = resolver.adaptTo(Session.class);
			String userId = session.getUserID();
			Authorizable user = userManager.getAuthorizable(userId);
			
			response.getWriter().write(String.valueOf(isAllowedCreation(user)));
			
			
		} catch (IOException | RepositoryException e) {
			LOGGER.error("Error while checking if current user is external : {0}", e);
		}
	}

	private boolean isAllowedCreation(Authorizable user) throws RepositoryException {
		boolean allowPublicCollection = false;
		
		Iterator<Group> groups = user.memberOf();
		while(groups.hasNext()) {
			Group group = groups.next();
			if(null!=group.getID() && group.getID().matches("administrators|mediahub-administrators|mediahub-basic-entity-manager")) {
				allowPublicCollection = true;
				break;
			}
			
			
		}
		return allowPublicCollection;
	}
}
