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
		List<String> listGroup = new ArrayList<>();
		if(currentUserGroups != null) {
			while (currentUserGroups.hasNext()) {
				Group grp = currentUserGroups.next();
				try {
					final String groupID = grp.getID();
					listGroup.add(groupID);
			  } catch (RepositoryException e) {
			 		log.error("repo error :",e);
			  }
			}
			if(listGroup.contains(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR) || listGroup.contains(BnpConstants.MEDIAHUB_ADMINISTRATOR) || listGroup.contains(BnpConstants.MEDIAHUB_BASIC_READER) || listGroup.contains(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER) || listGroup.contains(BnpConstants.MEDIAHUB_READER_MEDIALIBRARY)) {
				response.sendRedirect(MEDIA_LIBRARY_ASSET_PATH);
				return;
			} else {
				response.sendRedirect(PROJECTS_PATH);
				return;
			}
		}

		response.sendRedirect("/aem/start.html");
	}



}