package com.mediahub.core.services;

import org.apache.sling.api.resource.Resource;


public interface UpdateInternalUsersService {

    String createAndUpdateUsers();

    void removeAllUsers();

}
