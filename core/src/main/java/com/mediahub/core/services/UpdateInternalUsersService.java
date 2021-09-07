package com.mediahub.core.services;

import com.adobe.granite.jmx.annotation.Name;


public interface UpdateInternalUsersService {

    String createAndUpdateUsers(@Name("CSV User Info") String csvUserInfo, @Name("CSV Additional Info") String csvAdditionalInfo);

    void removeAllUsers();

}
