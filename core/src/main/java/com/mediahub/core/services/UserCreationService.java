package com.mediahub.core.services;

import java.util.List;

import com.mediahub.core.models.UserData;


public interface UserCreationService {
	
	void createUsers(List<UserData> users);
	
	void readCsv(String filePath);
}
