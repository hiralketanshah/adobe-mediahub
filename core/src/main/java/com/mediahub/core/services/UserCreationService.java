package com.mediahub.core.services;

import java.util.List;

import org.apache.sling.api.resource.ResourceResolver;

import com.mediahub.core.models.UserData;


public interface UserCreationService {
	
	void createUsers(List<UserData> users, ResourceResolver resolver);
	
	void readCsv(String filePath);
}
