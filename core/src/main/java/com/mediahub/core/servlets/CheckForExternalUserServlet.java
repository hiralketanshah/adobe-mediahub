package com.mediahub.core.servlets;

import java.io.IOException;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.servlet.Servlet;
import org.osgi.framework.Constants;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;

@Component(service = Servlet.class, property = {
		Constants.SERVICE_DESCRIPTION + "=Check if the current user is an external user",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/checkInternal" })
public class CheckForExternalUserServlet extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	protected static final Logger log = LoggerFactory.getLogger(CheckForExternalUserServlet.class);

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {

		try {

			ResourceResolver resolver = request.getResourceResolver();
			UserManager userManager = resolver.adaptTo(UserManager.class);
			Session session = resolver.adaptTo(Session.class);
			String userId = session.getUserID();
			Authorizable user = userManager.getAuthorizable(userId);
			if (null != user && isExternal(user)) {
				response.getWriter().write("true");
			} else {
				response.getWriter().write("false");
			}

		} catch (IOException | RepositoryException e) {
			log.error("Error while checking if current user is external : {0}", e);
		}

	}

	private boolean isExternal(Authorizable user) throws RepositoryException {
		Value[] propertyValue = user.getProperty(BnpConstants.USER_PROFILE + BnpConstants.PN_USER_PROFILE_TYPE);
		if (null != propertyValue) {
			return BnpConstants.EXTERNAL.equalsIgnoreCase(propertyValue[0].getString());
		}
		return false;
	}

}