package com.mediahub.core.models.impl;


import com.mediahub.core.models.UserModel;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.models.annotations.Model;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;

@Model(adaptables = SlingHttpServletRequest.class, adapters = UserModel.class)
public class UserModelImpl extends AuthorizableModelImpl implements UserModel {
  private static Logger LOG = LoggerFactory.getLogger(UserModelImpl.class);

  @Override
  public String getJobTitle() {
    if (userProperties != null) {
      try {
        return userProperties.getProperty("jobTitle");
      } catch (RepositoryException e) {
        LOG.error("Unable to obtain user's job title: ", e);
      }
    }
    return null;
  }

  @Override
  public boolean isSystemUser() {
    if (authorizable != null && !authorizable.isGroup()) {
      User user = (User)authorizable;
      return user.isSystemUser();
    }
    return false;
  }

  @Override
  public boolean isDisabled() {
    if (authorizable != null && !authorizable.isGroup()) {
      User user = (User)authorizable;
      try {
        return user.isDisabled();
      } catch (RepositoryException e) {
        LOG.error("Could not determine if the user is enabled: ", e);
      }
    }
    return false;
  }


}
