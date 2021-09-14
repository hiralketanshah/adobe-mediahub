package com.mediahub.core.servlets;

import java.io.IOException;

import javax.servlet.Servlet;
import org.osgi.framework.Constants;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;

import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.AttributeType;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.services.UpdateInternalUsersService;

@Component(service = Servlet.class, property = { Constants.SERVICE_DESCRIPTION + "=Create/Update Internal Users",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/mediahub/internalusers" })
@ServiceDescription("Create/Update Internal Users")
@Designate(ocd = UpdateInternalUsers.SchedulerConfig.class)
public class UpdateInternalUsers extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	protected static final Logger log = LoggerFactory.getLogger(UpdateInternalUsers.class);

	@Reference
	private UpdateInternalUsersService updateInternalUsers;

	String csvUserFile;
	String csvUserInfo;
	String csvUserStatus;

	@Activate
	protected void activate(UpdateInternalUsers.SchedulerConfig config) {
		this.csvUserFile = config.csvUserInfo();
		this.csvUserInfo = config.csvAdditionalInfo();
		this.csvUserStatus = config.csvStatusInfo();
	}

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {

		try {

			String remove = request.getParameter("remove");
			if (null != remove && remove.equalsIgnoreCase("true")) {
				log.debug("Removing all users");
				updateInternalUsers.removeAllUsers();
				response.getWriter().write("Completed Removing all the users");
			} else {
				log.debug("Reading the data from the csv");
				response.getWriter()
						.write(updateInternalUsers.createAndUpdateUsers(csvUserFile, csvUserInfo, csvUserStatus));

			}

		} catch (IOException e) {
			log.error("Error while reading CSV file for users : {0}", e);
		}
	}

	@ObjectClassDefinition(name = "MediaHub Updating Internal Users Servlet", description = "MediaHub Updating Internal Users Servlet")
	public @interface SchedulerConfig {

		@AttributeDefinition(name = "CSV File Path For User Info", description = "CSV Path For User Info", type = AttributeType.STRING)
		public String csvUserInfo();

		@AttributeDefinition(name = "CSV File Path For Additional Info", description = "CSV File Path For Additional Info", type = AttributeType.STRING)
		public String csvAdditionalInfo();

		@AttributeDefinition(name = "CSV File Path For Status Info", description = "CSV File Path For Status Info", type = AttributeType.STRING)
		public String csvStatusInfo();
	}
}