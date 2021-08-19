package com.mediahub.core.servlets;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.PropertyType;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.servlet.Servlet;
import org.osgi.framework.Constants;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.adobe.granite.asset.api.Asset;
import com.mediahub.core.data.User;

@Component(service = Servlet.class, property = { Constants.SERVICE_DESCRIPTION + "=Read CSV file",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/csv" })
@ServiceDescription("Read CSV File")
public class ReadCSV extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	private static final Logger LOGGER = LoggerFactory.getLogger(AcceptPrivacyPolicyServlet.class);

	protected static final Logger log = LoggerFactory.getLogger(ReadCSV.class);
	private static final String CSV_FILE_PATH = "/content/dam/bnp/users-list.csv";
	private static final String COMMA = ",";

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {

		try {

			String remove = request.getParameter("remove");
			if (null != remove && remove.equalsIgnoreCase("true")) {
				removeUsers(request);
				response.getWriter().write("Removed users");
			} else {
				log.info("Reading the data from the csv file");
				String responseString = "Servlet hit";
				ResourceResolver resourceResolver = request.getResourceResolver();
				Resource csvResource = resourceResolver.getResource(CSV_FILE_PATH);

				InputStream inputStream = csvResource.adaptTo(Asset.class).getRendition("original").getStream();
				BufferedReader br = new BufferedReader(new InputStreamReader(inputStream));
				Map<String, User> inputUserMap = convertStreamToHashMap(br, true);
				br.close();

				UserManager userManager = resourceResolver.adaptTo(UserManager.class);
				Session session = resourceResolver.adaptTo(Session.class);
				createAndSaveUsers(inputUserMap, userManager, session);
				response.getWriter().write(responseString);
			}

		} catch (IOException e) {
			LOGGER.error("Error while reading CSV file for users : {0}", e);
		} catch (RepositoryException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}
	}

	private void removeUsers(SlingHttpServletRequest request) throws RepositoryException {
		ResourceResolver resourceResolver = request.getResourceResolver();
		Session session = resourceResolver.adaptTo(Session.class);
		Node node;
		try {
			node = session.getNode("/home/users/mediahub");
			node.remove();
			session.save();
		} catch (PathNotFoundException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}
	}

	private void createAndSaveUsers(Map<String, User> inputUserMap, UserManager userManager, Session session)
			throws RepositoryException {

		for (Map.Entry<String, User> entry : inputUserMap.entrySet()) {
			Principal principal = new Principal() {
				public String getName() {
					return entry.getKey();
				}
			};
			createAEMUser(userManager, session, principal, entry.getKey(), entry.getKey());
			session.save();
		}

	}

	private void createAEMUser(UserManager userManager, Session session, Principal principal, String userId,
			String password) {
		if (null != userManager) {
			try {
				if (null != userManager.getAuthorizable(userId)) {
					log.info("User {} is already present", userId);
					return;
				}
				org.apache.jackrabbit.api.security.user.User user = userManager.createUser(userId, password, principal,
						"/home/users/mediahub");
				user.setProperty("./profile/company",
						session.getValueFactory().createValue("Adobe", PropertyType.STRING));
				log.info("User is created successfuly");
			} catch (RepositoryException e) {
				LOGGER.error("Error while accessing repository : {0}", e);
			}
		}
	}

	private Map<String, User> convertStreamToHashMap(BufferedReader br, boolean skipLine) {
		int skip = skipLine ? 1 : 0;
		List<User> inputList;
		inputList = br.lines().skip(skip).map(mapToItem).collect(Collectors.toList());
		Map<String, User> inputUserMap = inputList.stream().collect(Collectors.toMap(User::getId, Function.identity()));
		return inputUserMap;
	}

	private Function<String, User> mapToItem = line -> {
		String[] p = line.split(COMMA);
		return new User(p[0], p[1], p[2], p[3]);
	};

}