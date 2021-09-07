package com.mediahub.core.services.impl;

import com.adobe.granite.asset.api.Asset;
import com.adobe.granite.jmx.annotation.AnnotatedStandardMBean;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.data.User;
import com.mediahub.core.data.UserInfo;
import com.mediahub.core.services.UpdateInternalUsersService;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.util.Collections;
import java.util.HashMap;
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
import javax.jcr.Value;
import javax.management.DynamicMBean;
import javax.management.NotCompliantMBeanException;

@Component(service = { DynamicMBean.class }, immediate = true, property = {
		"jmx.objectname = com.mediahub.core.services.impl:type=Update Internal Users", "propertyPrivate = true" })

public class UpdateInternalUsersServiceImpl extends AnnotatedStandardMBean implements UpdateInternalUsersService {

	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateInternalUsersServiceImpl.class);

	@Reference
	private ResourceResolverFactory resolverFactory;

	@Reference
	private QueryBuilder builder;

	public UpdateInternalUsersServiceImpl() throws NotCompliantMBeanException {
		super(UpdateInternalUsersService.class);
	}

	final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@Override
	public String createAndUpdateUsers(String csvUserInfo, String csvAdditionalInfo) {
		StringBuilder returnValue = new StringBuilder();
		try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
			if(StringUtils.isNotBlank(csvUserInfo) && StringUtils.isNotBlank(csvAdditionalInfo)) {
				Resource csvResource = resourceResolver.getResource(csvUserInfo);
				Resource userInfoResource = resourceResolver.getResource(csvAdditionalInfo);
				Map<String, User> inputUserMap = new LinkedHashMap<>();
				Map<String, UserInfo> userInfoMap = new LinkedHashMap<>();

				if (null != csvResource && DamUtil.isAsset(csvResource)) {
					InputStream userInputStream = csvResource.adaptTo(Asset.class)
							.getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream();
					BufferedReader brUser = new BufferedReader(new InputStreamReader(userInputStream));
					inputUserMap = convertStreamToHashMap(brUser, true);
					brUser.close();
				}
				if (null != userInfoResource && DamUtil.isAsset(userInfoResource)) {
					InputStream userInfoInputStream = userInfoResource.adaptTo(Asset.class)
							.getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream();
					BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(userInfoInputStream));
					userInfoMap = convertStreamToHashMapUserInfo(brUserInfo, true);
					brUserInfo.close();
				}
				UserManager userManager = resourceResolver.adaptTo(UserManager.class);
				Session session = resourceResolver.adaptTo(Session.class);
				createAndSaveUsers(inputUserMap, userInfoMap, userManager, session);
				deletedUnwantedUsers(resourceResolver, inputUserMap);
				if (resourceResolver.hasChanges()) {
					resourceResolver.commit();
				}
				returnValue.append("Internal Users are successfully created/updated or deleted as per the records present in the latest CSV file");
			}
			else {
				returnValue.append("Kindly add CSV files for User Info and Addition Info and try again!");
			}
		} catch (LoginException e) {
			LOGGER.error("Error while Logging into the repository : {0}", e);
		} catch (IOException e) {
			LOGGER.error("Error while accessing the files : {0}", e);
		} catch (RepositoryException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}
		return returnValue.toString();
	}

	@Override
	public void removeAllUsers() {
		try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
			Session session = resourceResolver.adaptTo(Session.class);
			Node node;

			node = session.getNode(BnpConstants.USER_PATH);
			node.remove();
			session.save();

		} catch (LoginException e) {
			LOGGER.error("Error while Logging into the repository : {0}", e);
		} catch (PathNotFoundException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		} catch (RepositoryException e) {
			LOGGER.error("Error while accessing repository : {0}", e);
		}

	}

	private void deletedUnwantedUsers(ResourceResolver resourceResolver, Map<String, User> inputUserMap)
			throws PersistenceException, RepositoryException {
		boolean hasUsers = true;
		int offset = 0;
		while (hasUsers && !inputUserMap.isEmpty()) {
			Map<String, String> parameters = new HashMap<>();
			parameters.put("path", BnpConstants.USER_PATH);
			parameters.put("1_property", JcrConstants.JCR_PRIMARYTYPE);
			parameters.put("1_property.value", "rep:User");
			parameters.put("2_property", "profile/type");
			parameters.put("2_property.value", "internal");
			parameters.put("p.limit", "1000");
			parameters.put("p.offset", String.valueOf(offset));

			Query query = builder.createQuery(PredicateGroup.create(parameters),
					resourceResolver.adaptTo(Session.class));
			SearchResult result = query.getResult();

			for (Hit hit : result.getHits()) {
				Resource res = resourceResolver.getResource(hit.getPath());
				ValueMap properties = res.adaptTo(ValueMap.class);
				String userID = properties.get(BnpConstants.PN_PRINCIPAL_NAME, String.class);
				if (!inputUserMap.containsKey(userID)) {
					LOGGER.debug("Removing unwanted user : {}", userID);
					resourceResolver.delete(res);
				}

			}
			offset += 1000;
			if (offset > result.getTotalMatches()) {
				LOGGER.debug("Completed checking all the internal users!");
				hasUsers = false;
			}
			LOGGER.debug("Completed checking {} users : ", offset);
		}

	}

	private void createAndSaveUsers(Map<String, User> inputUserMap, Map<String, UserInfo> userInfoMap,
			UserManager userManager, Session session) throws RepositoryException {
		int value = 0;
		int count = 0;
		for (Map.Entry<String, User> entry : inputUserMap.entrySet()) {
			Principal principal = new Principal() {
				public String getName() {
					return entry.getKey();
				}
			};
			count++;
			if (count >= 10000) {
				count = 0;
				value++;
				session.save();
			}

			User userObject = entry.getValue();
			if (StringUtils.isNotBlank(userObject.getUoId()) && userInfoMap.containsKey(userObject.getUoId())) {
				UserInfo userInfo = userInfoMap.get(userObject.getUoId());
				userObject.setBusiness(userInfo.getBusiness());
			}
			createAEMUser(userManager, session, principal, entry.getKey(), entry.getValue(), value);

		}
		session.save();

	}

	private void createAEMUser(UserManager userManager, Session session, Principal principal, String userId,
			User userInfo, int value) {
		if (null != userManager) {
			try {
				if (null != userManager.getAuthorizable(userId)) {
					LOGGER.debug("User {} is already present", userId);
					Authorizable user = userManager.getAuthorizable(userId);
					updateUserInfo(user, userInfo, session);

				} else {
					String hashedUserId = encryptThisString(userId);
					org.apache.jackrabbit.api.security.user.User user = userManager.createUser(userId, userId,
							principal, BnpConstants.USER_PATH + "/" + hashedUserId.substring(0, 2));
					updateUserInfo(user, userInfo, session);
					LOGGER.debug("User is created successfully with userId : {}", userId);
				}
			} catch (RepositoryException e) {
				LOGGER.error("Error while accessing repository : {0}", e);
			}
		}
	}

	public static String encryptThisString(String input) {
		String hashtext = input;
		try {
			MessageDigest md = MessageDigest.getInstance("SHA-1");
			byte[] messageDigest = md.digest(input.getBytes());
			BigInteger no = new BigInteger(1, messageDigest);
			hashtext = no.toString(16);
			while (hashtext.length() < 32) {
				hashtext = "0" + hashtext;
			}
		} catch (NoSuchAlgorithmException e) {
			LOGGER.error("Error while encrypting userID : {0}", e);
		}
		return hashtext;
	}

	private void updateUserInfo(Authorizable user, User userInfo, Session session) throws RepositoryException {
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_GIVENNAME, userInfo.getGivenName())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_GIVENNAME),
					session.getValueFactory().createValue(userInfo.getGivenName(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_FAMILYNAME, userInfo.getFamilyName())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_FAMILYNAME),
					session.getValueFactory().createValue(userInfo.getFamilyName(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_EMAIL, userInfo.getEmailId())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_EMAIL),
					session.getValueFactory().createValue(userInfo.getEmailId(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_JOBTITLE, userInfo.getJobTitle())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_JOBTITLE),
					session.getValueFactory().createValue(userInfo.getJobTitle(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_CITY, userInfo.getCity())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_CITY),
					session.getValueFactory().createValue(userInfo.getCity(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_COUNTRY, userInfo.getCountry())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_COUNTRY),
					session.getValueFactory().createValue(userInfo.getCountry(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_UOID, userInfo.getUoId())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_UOID),
					session.getValueFactory().createValue(userInfo.getUoId(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_BUSINESS, userInfo.getBusiness())) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_BUSINESS),
					session.getValueFactory().createValue(userInfo.getBusiness(), PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_TYPE, BnpConstants.VAL_USER_PROFILE_TYPE)) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_TYPE),
					session.getValueFactory().createValue(BnpConstants.VAL_USER_PROFILE_TYPE, PropertyType.STRING));
		}
		if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_COMPANY, BnpConstants.VAL_USER_PROFILE_COMPANY)) {
			user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_COMPANY),
					session.getValueFactory().createValue(BnpConstants.VAL_USER_PROFILE_COMPANY, PropertyType.STRING));
		}

	}

	private boolean checkExistance(Authorizable user, String property, String updatedPropertyValue)
			throws RepositoryException {
		Value[] propertyValue = user.getProperty(BnpConstants.USER_PROFILE + property);
		if (null == updatedPropertyValue) {
			user.removeProperty(BnpConstants.USER_PROFILE + property);
			return true;
		}

		if (null != propertyValue) {
			String propertyString = propertyValue[0].getString();
			return updatedPropertyValue.equals(propertyString);
		}
		return false;

	}

	private LinkedHashMap<String, UserInfo> convertStreamToHashMapUserInfo(BufferedReader br, boolean skipLine) {
		int skip = skipLine ? 1 : 0;
		List<UserInfo> inputList;
		inputList = br.lines().skip(skip).map(mapToUserInfo).collect(Collectors.toList());
		return inputList.stream()
				.collect(Collectors.toMap(UserInfo::getUoId, Function.identity(), (v1, v2) -> v1, LinkedHashMap::new));

	}

	private LinkedHashMap<String, User> convertStreamToHashMap(BufferedReader br, boolean skipLine) {
		int skip = skipLine ? 1 : 0;
		List<User> inputList;
		inputList = br.lines().skip(skip).map(mapToItem).collect(Collectors.toList());
		return inputList.stream()
				.collect(Collectors.toMap(User::getId, Function.identity(), (v1, v2) -> v1, LinkedHashMap::new));
	}

	private Function<String, User> mapToItem = line -> {
		String[] p = line.split(BnpConstants.COMMA);
		User user = null;
		if (null != p) {
			user = new User();

			user.setId(p[0]);
			user.setFamilyName(p.length > 1 ? p[1] : null);
			user.setGivenName(p.length > 2 ? p[2] : null);
			user.setEmailId(p.length > 4 ? p[4] : null);
			user.setJobTitle(p.length > 11 ? p[11] : null);
			user.setCity(p.length > 7 ? p[7] : null);
			user.setCountry(p.length > 9 ? p[9] : null);
			user.setUoId(p.length > 5 ? p[5] : null);
			user.setBusiness(null);

		}
		return user;
	};

	private Function<String, UserInfo> mapToUserInfo = line -> {
		String[] p = line.split(BnpConstants.COMMA);
		UserInfo userInfo = null;
		if (null != p) {
			userInfo = new UserInfo();

			userInfo.setUoId(p[0]);
			userInfo.setBusiness(p.length >= 5 ? p[5] : null);

		}
		return userInfo;
	};

}