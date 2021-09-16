package com.mediahub.core.servlets;

import com.mediahub.core.constants.BnpConstants;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.jcr.RepositoryException;
import javax.servlet.Servlet;
import javax.servlet.ServletException;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingSafeMethodsServlet;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@SuppressWarnings("CQRules:CQBP-75")
@Component(service=Servlet.class,
property={
        Constants.SERVICE_DESCRIPTION + "=Default Page Servlet",
        "sling.servlet.methods=" + HttpConstants.METHOD_GET,
        "sling.servlet.paths="+ "/bin/defaultpage"
})
public class DefaultPageServlet extends SlingSafeMethodsServlet {

	private static final Logger log = LoggerFactory.getLogger(DefaultPageServlet.class);

	private static final String MEDIA_LIBRARY_ASSET_PATH = "/assets.html/content/dam/medialibrary";

	private static final String PROJECTS_PATH = "/projects.html/content/projects";

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response)
			throws ServletException, IOException {

		User currentUser = request.getResourceResolver().adaptTo(User.class);
		Iterator<Group> currentUserGroups = null;
		try {
			currentUserGroups = currentUser.memberOf();
		} catch (RepositoryException e) {
			 log.error("repo error :",e);
		}

		if(currentUserGroups != null) {
				response.sendRedirect(MEDIA_LIBRARY_ASSET_PATH);
				return;
		}

		response.sendRedirect("/aem/start.html");
	}



}