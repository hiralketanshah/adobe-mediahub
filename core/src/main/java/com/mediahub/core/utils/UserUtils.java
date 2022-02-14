package com.mediahub.core.utils;

import com.mediahub.core.constants.BnpConstants;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.security.Privilege;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UserUtils {

  private static Logger logger = LoggerFactory.getLogger(UserUtils.class);

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
      MessageDigest md = MessageDigest.getInstance("SHA-1");
      byte[] messageDigest = md.digest(input.getBytes());
      BigInteger no = new BigInteger(1, messageDigest);
      input = no.toString(16);
      while (input.length() < 32) {
          input = "0" + input;
      }
      return input;
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


  /**
   * MED-493 Create user group while creating folders
   *
   * @param resolver
   * @param uuid
   * @param resource
   */
  public static void createFolderGroups(ResourceResolver resolver, String uuid, Resource resource) {
      final UserManager userManager = resolver.adaptTo(UserManager.class);
      try {
          Group folderContributor = userManager.createGroup(uuid + "-contributor");
          Group folderEntityManager = userManager.createGroup(uuid + "-entity-manager");
          Session session = resolver.adaptTo(Session.class);
          if(userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR) != null){
              ((Group) userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR)).addMember(folderContributor);
              CreatePolicyNodeUtil.createRepPolicyNode(session, resource.getPath(), Boolean.TRUE, folderContributor.getPrincipal(), Privilege.JCR_ALL);
          }
          if(userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER) != null){
              ((Group) userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER)).addMember(folderEntityManager);
              CreatePolicyNodeUtil.createRepPolicyNode(session, resource.getPath(), Boolean.TRUE, folderEntityManager.getPrincipal(), Privilege.JCR_ALL);
          }
      } catch (RepositoryException e) {
        logger.error("Exception while creating groups for folders", e);
      }
  }

  public static String getUserType(Resource user) {
    if(null != user.getChild(BnpConstants.PROFILE)){
      Resource preferences = user.getChild(BnpConstants.PROFILE);
      return preferences.getValueMap().get(BnpConstants.TYPE, "");
    } else{
      return "";
    }
  }


}
