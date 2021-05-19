package com.mediahub.core.services.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.jcr.PropertyType;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.Rendition;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.models.UserData;
import com.mediahub.core.services.UserCreationService;

@Component(service = UserCreationService.class, immediate = true)
public class UserCreationServiceImpl implements UserCreationService {
	
	private final Logger log = LoggerFactory.getLogger(this.getClass());

	@Reference
	private ResourceResolverFactory resourceResolverFactory;
	
	private ResourceResolver resourceResolver;
	private Session session;
	private List<UserData> users;

	@Override
	public void createUsers(List<UserData> users) {
		try {
			session = resourceResolver.adaptTo(Session.class);
			JackrabbitSession js = (JackrabbitSession) session;
			UserManager userManager = js.getUserManager();
			ValueFactory valueFactory = session.getValueFactory();
			User user;
			int countUsers =0;
			for (UserData us : users) {
				if (userManager.getAuthorizable(us.getEmail()) == null) {
					user = userManager.createUser(us.getEmail(), StringUtils.EMPTY);
					countUsers++;
					Value firstNameValue = valueFactory.createValue(us.getFirstName(), PropertyType.STRING);
					user.setProperty("./profile/givenName", firstNameValue);

					Value lastNameValue = valueFactory.createValue(us.getLastName(), PropertyType.STRING);
					user.setProperty("./profile/familyName", lastNameValue);

					Value emailValue = valueFactory.createValue(us.getEmail(), PropertyType.STRING);
					user.setProperty("./profile/email", emailValue);

					Group addUserToGroup = (Group) (userManager.getAuthorizable(BnpConstants.BASIC_GROUP));
					if (null != addUserToGroup) {
						addUserToGroup.addMember(user);
					}
				}
			}
			log.info("Users created from CSV file is {}", countUsers);
			if (!userManager.isAutoSave()) {
				js.save();
				session.save();

			}
		} catch (RepositoryException e) {
			log.info("Error while creating user {}", e.getMessage());
		}
	}

	@Override
	public void readCsv(String filePath) {
		Resource resource = null;

		Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
				BnpConstants.WRITE_SERVICE);
		try {
			resourceResolver = resourceResolverFactory.getServiceResourceResolver(authInfo);
			resource = resourceResolver.getResource(filePath);
			Asset asset = resource.adaptTo(Asset.class);
			Rendition rend = asset.getOriginal();
			InputStream is = rend.getStream();
			BufferedReader br = new BufferedReader(new InputStreamReader(is));
			String line = "";
			String splitBy = ",";
			int count=0;
			UserData user = null;
			users = new LinkedList<>();
			while ((line = br.readLine()) != null) {
				if(count >0) {
					user = new UserData();
					String[] csvdata = line.split(splitBy);
					String id= csvdata[0];
					if(null!= id && !id.isEmpty()) {
						user.setId(id);
					}
					
					String email = csvdata[1];
					if (email != null && !email.isEmpty()) {
						user.setEmail(email);
					}
					
					String firstName = csvdata[2];
					if (firstName != null && !firstName.isEmpty()) {
						user.setFirstName(firstName);
					}

					String lastName = csvdata[3];
					if (lastName != null && !lastName.isEmpty()) {
						user.setLastName(lastName);
					}
					users.add(user);
				}
				count++;

			}
			createUsers(users);
			
			br.close();
			
		} catch (IOException | LoginException e) {
			log.info("Error while reading CSV {}", e.getMessage());
		}
	}

}
