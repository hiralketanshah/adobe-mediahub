package com.mediahub.core.utils;

import com.mediahub.core.constants.BnpConstants;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.jcr.RepositoryException;
import javax.jcr.Value;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
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
  
  @SuppressWarnings("squid:S2070")
  public static String encryptThisString(String input) throws NoSuchAlgorithmException {
      String hashtext = input;
      MessageDigest md = MessageDigest.getInstance("SHA-1");
      byte[] messageDigest = md.digest(input.getBytes());
      BigInteger no = new BigInteger(1, messageDigest);
      hashtext = no.toString(16);
      while (hashtext.length() < 32) {
          hashtext = "0" + hashtext;
      }
      return hashtext;
  }
  
  public static String getProjectOwnerName(String initiator, UserManager userManager) throws RepositoryException {
      User projectOwnerUser = (User) userManager.getAuthorizable(initiator);
      Value[] givenName = projectOwnerUser.getProperty("./profile/givenName");
      Value[] familyName = projectOwnerUser.getProperty("./profile/familyName");
      String fullName;
      if(null!=givenName && null!=familyName) {
          String[] fullNameString = {givenName[0].getString(),familyName[0].getString()}; 
          fullName = String.join(" ", fullNameString);
      }
      else {
          fullName = null==givenName ? (null==familyName ? initiator : familyName[0].getString()) : givenName[0].getString();
      }
      return fullName;
  }
}
