package com.mediahub.core.servlets;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.PropertyType;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.Value;
import javax.jcr.ValueFormatException;
import javax.servlet.Servlet;
import org.osgi.framework.Constants;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.Authorizable;
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
import com.mediahub.core.data.UserInfo;

@Component(service = Servlet.class, property = { Constants.SERVICE_DESCRIPTION + "=Read CSV file",
		"sling.servlet.methods=" + HttpConstants.METHOD_GET, "sling.servlet.paths=" + "/bin/csv" })
@ServiceDescription("Read CSV File")
public class ReadCSV extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadCSV.class);

	protected static final Logger log = LoggerFactory.getLogger(ReadCSV.class);
	private static final String CSV_FILE_PATH = "/content/dam/bnp/users-list.csv";
	private static final String CSV_USER_INFO = "/content/dam/bnp/user-info.csv";
	private static final String USER_PATH = "/home/users/mediahub";
	private static final String COMMA = ",";

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {

		try {

			String remove = request.getParameter("remove");
			if (null != remove && remove.equalsIgnoreCase("true")) {
				log.info("removing all users");
				removeUsers(request);
				response.getWriter().write("Removed users");
			} else {
				log.info("Reading the data from the csv file");
				String responseString = "Servlet hit";
				ResourceResolver resourceResolver = request.getResourceResolver();
				Resource csvResource = resourceResolver.getResource(CSV_FILE_PATH);
				Resource userInfoResource = resourceResolver.getResource(CSV_USER_INFO);

				InputStream UserInputStream = csvResource.adaptTo(Asset.class).getRendition("original").getStream();
				InputStream UserInfoInputStream = userInfoResource.adaptTo(Asset.class).getRendition("original")
						.getStream();

				BufferedReader brUser = new BufferedReader(new InputStreamReader(UserInputStream));
				Map<String, User> inputUserMap = convertStreamToHashMap(brUser, true);
				brUser.close();

				BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(UserInfoInputStream));
				Map<String, UserInfo> userInfoMap = convertStreamToHashMapUserInfo(brUserInfo, true);
				brUserInfo.close();


				
				
				UserManager userManager = resourceResolver.adaptTo(UserManager.class);
				Session session = resourceResolver.adaptTo(Session.class);
				createAndSaveUsers(inputUserMap, userInfoMap, userManager, session);
				response.getWriter().write(responseString);
			}

		} catch (IOException e) {
			LOGGER.error("Error while reading CSV file for users : {0}", e);
		} catch (RepositoryException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}
	}

	private Map<String, User> combineUserMaps(Map<String, User> inputUserMap, Map<String, User> userInfoMap) {

		return null;
	}

	private void removeUsers(SlingHttpServletRequest request) throws RepositoryException {
		ResourceResolver resourceResolver = request.getResourceResolver();
		Session session = resourceResolver.adaptTo(Session.class);
		Node node;
		try {
			node = session.getNode(USER_PATH);
			node.remove();
			session.save();
		} catch (PathNotFoundException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}
	}

	private void createAndSaveUsers(Map<String, User> inputUserMap, Map<String, UserInfo> userInfoMap, UserManager userManager, Session session)
			throws RepositoryException {
		int value = 0;
		int count = 0;
		for (Map.Entry<String, User> entry : inputUserMap.entrySet()) {
			Principal principal = new Principal() {
				public String getName() {
					return entry.getKey();
				}
			};
			count++;
			if(count>=10000) {
				count=0;
				value++;
				session.save();
			}
			
			User userObject = entry.getValue();
			if(StringUtils.isNotBlank(userObject.getUoId()) && userInfoMap.containsKey(userObject.getUoId())) {
				UserInfo userInfo = userInfoMap.get(userObject.getUoId());
				 userObject.setBusiness(userInfo.getBusiness());
			}
			createAEMUser(userManager, session, principal, entry.getKey(), entry.getValue(), value);
			
		         //session.save();
		     
		}
		session.save();

	}

	private void createAEMUser(UserManager userManager, Session session, Principal principal, String userId,
			User userInfo, int value) {
		if (null != userManager) {
			try {
				if (null != userManager.getAuthorizable(userId)) {
					log.info("User {} is already present", userId);
					Authorizable user = userManager.getAuthorizable(userId);
					updateUserInfo(user,userInfo, session);
					
				}
				else {
					
					String hashed = encryptThisString(userId);
					
					org.apache.jackrabbit.api.security.user.User user = userManager.createUser(userId, userId, principal,
							USER_PATH+"/"+hashed.substring(0, 2));
					updateUserInfo(user, userInfo, session);
					/*user.setProperty("./profile/givenName",
							session.getValueFactory().createValue(userInfo.getGivenName(), PropertyType.STRING));
					user.setProperty("./profile/familyName",
							session.getValueFactory().createValue(userInfo.getFamilyName(), PropertyType.STRING));
					user.setProperty("./profile/email",
							session.getValueFactory().createValue(userInfo.getEmailId(), PropertyType.STRING));
					user.setProperty("./profile/jobTitle",
							session.getValueFactory().createValue(userInfo.getJobTitle(), PropertyType.STRING));
					user.setProperty("./profile/uoId",
							session.getValueFactory().createValue(userInfo.getUoId(), PropertyType.STRING));
					user.setProperty("./profile/business",
							session.getValueFactory().createValue(userInfo.getBusiness(), PropertyType.STRING));*/
					/*user.setProperty("./profile/company",
							session.getValueFactory().createValue("BNP Paribas", PropertyType.STRING));
					user.setProperty("./profile/type",
							session.getValueFactory().createValue("internal", PropertyType.STRING));*/
					log.info("User is created successfully with userId : {}",userId);
				}
			} catch (RepositoryException e) {
				LOGGER.error("Error while accessing repository : {0}", e);
			}
		}
	}
	
	
	public static String encryptThisString(String input)
    {
        try {
            // getInstance() method is called with algorithm SHA-1
            MessageDigest md = MessageDigest.getInstance("SHA-1");
  
            // digest() method is called
            // to calculate message digest of the input string
            // returned as array of byte
            byte[] messageDigest = md.digest(input.getBytes());
  
            // Convert byte array into signum representation
            BigInteger no = new BigInteger(1, messageDigest);
  
            // Convert message digest into hex value
            String hashtext = no.toString(16);
  
            // Add preceding 0s to make it 32 bit
            while (hashtext.length() < 32) {
                hashtext = "0" + hashtext;
            }
  
            // return the HashText
            return hashtext;
        }
  
        // For specifying wrong message digest algorithms
        catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
    }

	private void updateUserInfo(Authorizable user, User userInfo, Session session) throws ValueFormatException, UnsupportedRepositoryOperationException, RepositoryException {
		if(!checkExistance(user,"givenName", userInfo.getGivenName())) {
			user.setProperty("./profile/givenName",
					session.getValueFactory().createValue(userInfo.getGivenName(), PropertyType.STRING));
		}
		if(!checkExistance(user,"familyName", userInfo.getFamilyName())) {
			user.setProperty("./profile/familyName",
					session.getValueFactory().createValue(userInfo.getFamilyName(), PropertyType.STRING));
		}
		if(!checkExistance(user,"email", userInfo.getEmailId())) {
			user.setProperty("./profile/email",
					session.getValueFactory().createValue(userInfo.getEmailId(), PropertyType.STRING));
		}
		if(!checkExistance(user,"jobTitle", userInfo.getJobTitle())) {
			user.setProperty("./profile/jobTitle",
					session.getValueFactory().createValue(userInfo.getJobTitle(), PropertyType.STRING));
		}
		if(!checkExistance(user,"uoId", userInfo.getUoId())) {
			user.setProperty("./profile/uoId",
					session.getValueFactory().createValue(userInfo.getUoId(), PropertyType.STRING));
		}
		if(!checkExistance(user,"business", userInfo.getBusiness())) {
			user.setProperty("./profile/business",
					session.getValueFactory().createValue(userInfo.getBusiness(), PropertyType.STRING));
		}

	}
	
	private boolean checkExistance(Authorizable user, String property, String updatedPropertyValue) throws RepositoryException {
		Value[] propertyValue = user.getProperty("./profile/"+property);
		if(null==updatedPropertyValue) {
			user.removeProperty("./profile/"+property);
			return true;
		}
		
		if(null!=propertyValue) {
			String propertyString = propertyValue[0].getString();
			return updatedPropertyValue.equals(propertyString);
		}
		return false;
		
	}

	private LinkedHashMap<String, UserInfo> convertStreamToHashMapUserInfo(BufferedReader br, boolean skipLine) {
		int skip = skipLine ? 1 : 0;
		List<UserInfo> inputList;
		inputList = br.lines().skip(skip).map(mapToUserInfo).collect(Collectors.toList());
		return inputList.stream().collect(Collectors.toMap(UserInfo::getUoId, Function.identity(), (v1,v2)-> v1, LinkedHashMap :: new));		
		
	}

	private LinkedHashMap<String, User> convertStreamToHashMap(BufferedReader br, boolean skipLine) {
		int skip = skipLine ? 1 : 0;
		List<User> inputList;
		inputList = br.lines().skip(skip).map(mapToItem).collect(Collectors.toList());
		return inputList.stream().collect(Collectors.toMap(User::getId, Function.identity(), (v1,v2)-> v1, LinkedHashMap :: new));
	}

	private Function<String, User> mapToItem = line -> {
		String[] p = line.split(COMMA);
		User user = null;
		if(null!=p) {
			user = new User();
			
			user.setId(p[0]);
			user.setFamilyName(p.length>1 ? p[1] : null);
			user.setGivenName(p.length>2 ? p[2] : null);
			user.setEmailId(p.length>4 ? p[4] : null);
			user.setJobTitle(p.length>11 ? p[11] : null);
			user.setUoId(p.length>5 ? p[5] : null);
			user.setBusiness(null);
			
		}
		return user;
	};
	
	private Function<String, UserInfo> mapToUserInfo = line -> {
		String[] p = line.split(COMMA);
		UserInfo userInfo = null;
		if(null!=p) {
			userInfo = new UserInfo();
			
			userInfo.setUoId(p[0]);
			userInfo.setBusiness(p.length>=5 ? p[5] : null);
			
		}
		return userInfo;
	};

}