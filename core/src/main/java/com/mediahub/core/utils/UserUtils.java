package com.mediahub.core.utils;

import com.mediahub.core.constants.BnpConstants;
import javax.jcr.RepositoryException;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.sling.api.resource.Resource;

public class UserUtils {

  private UserUtils(){
    //private constructor to restrict static class
  }

  public static String getUserLanguage(Resource user) {
    if(null != user.getChild(BnpConstants.PREFERENCES)){
      Resource preferences = user.getChild(BnpConstants.PREFERENCES);
      return preferences.getValueMap().get(BnpConstants.LANGUAGE, "en");
    } else{
      return "en";
    }
  }

  public static String getUserLanguage(Authorizable userAuthorization) throws RepositoryException {
    return userAuthorization.getProperty(BnpConstants.PREFERENCES_LANGUAGE_PROPERTY) != null ? userAuthorization.getProperty(BnpConstants.PREFERENCES_LANGUAGE_PROPERTY)[0].getString() : "en";
  }
}
