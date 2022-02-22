package com.mediahub.core.services;

import com.adobe.granite.jmx.annotation.Description;
import com.adobe.granite.jmx.annotation.Name;

@Description("Updating Internal users")
public interface UpdateInternalUsersService {

    String createAndUpdateUsers(@Name("CSV User Info") String csvUserInfo, @Name("CSV Additional Info") String csvAdditionalInfo, @Name("CSV Status Info") String csvStatusInfo, @Name("Email to send results to") String emailTo);

    String removeAllUsers();

}
