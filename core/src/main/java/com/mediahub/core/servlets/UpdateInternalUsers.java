package com.mediahub.core.servlets;

import java.io.IOException;

import javax.servlet.Servlet;
import org.osgi.framework.Constants;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;

import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.apache.sling.models.annotations.injectorspecific.OSGiService;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.UpdateInternalUsersService;

@Component(service = Servlet.class, property = { Constants.SERVICE_DESCRIPTION + "=Create/Update Internal Users",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/internalusers" })
@ServiceDescription("Create/Update Internal Users")
public class UpdateInternalUsers extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateInternalUsers.class);

	protected static final Logger log = LoggerFactory.getLogger(UpdateInternalUsers.class);

	@OSGiService
	private UpdateInternalUsersService updateInternalUsers;

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {

		try {

			String remove = request.getParameter("remove");
			if (null != remove && remove.equalsIgnoreCase("true")) {
				log.debug("Removing all users");
				updateInternalUsers.removeAllUsers();
				response.getWriter().write("Completed Removing all the users");
			} else {
				log.debug("Reading the data from the csv file");
				String responseString = "Internal Users are successfully created/updated or deleted as per the records present in the latest CSV file";
				response.getWriter().write(responseString);
				updateInternalUsers.createAndUpdateUsers(BnpConstants.CSV_FILE_PATH, BnpConstants.CSV_USER_INFO);

			}

		} catch (IOException e) {
			LOGGER.error("Error while reading CSV file for users : {0}", e);
		}
	}

}