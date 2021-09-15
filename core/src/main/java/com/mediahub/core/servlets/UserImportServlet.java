package com.mediahub.core.servlets;

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
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.UserCreationService;

@SuppressWarnings("CQRules:CQBP-75")
@Component(service=Servlet.class,
property={
        Constants.SERVICE_DESCRIPTION + "=User Import Servlet",
        "sling.servlet.methods=" + HttpConstants.METHOD_GET,
        "sling.servlet.paths="+ "/bin/importUser"
})
public class UserImportServlet extends SlingSafeMethodsServlet{
	private static final long serialVersionUID = 1L;
	
	private static final String CSV_ASSET_PATH = "/content/dam/userImport.csv";
	
	@Reference
	private transient UserCreationService userCreationService;
	
	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response)
			throws ServletException, IOException {
		final Logger log = LoggerFactory.getLogger(DefaultPageServlet.class);
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
				userCreationService.readCsv(CSV_ASSET_PATH);
			}
		}
	
	}

}
