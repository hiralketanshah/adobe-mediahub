package com.mediahub.core.listeners;

import com.day.cq.dam.api.DamConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.RepositoryException;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChange.ChangeType;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Listener class to Comments and Assets added in content/dam/projects and notify the users.
 */
@Component(service = {
    ResourceChangeListener.class},
    immediate = true,
    property = {
        ResourceChangeListener.CHANGES + "=ADDED",
        ResourceChangeListener.CHANGES + "=CHANGED",
        ResourceChangeListener.PATHS + "=glob:/content/dam/projects/**"// For checking resources added
    }
)
@ServiceDescription("listen on changes in the resource tree")
public class ProjectDamResourceListener implements ResourceChangeListener {

  public static final String PROFILE_EMAIL = "./profile/email";
  private final Logger log = LoggerFactory.getLogger(ProjectDamResourceListener.class);

  @Reference
  GenericEmailNotification genericEmailNotification;

  @Reference
  private ResourceResolverFactory resolverFactory;

  @Override
  public void onChange(List<ResourceChange> list) {

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      for (ResourceChange resourceChanged : list) {
        String resourcePath = resourceChanged.getPath();
        if(!ChangeType.ADDED.equals(resourceChanged.getType())){
          return;
        }
        log.debug("Asset Listener :{}",  resourcePath);
        Resource addedResource = adminResolver.getResource(resourceChanged.getPath());
        String resourceType = addedResource.getResourceType();

        if(StringUtils.equals(resourceType, DamConstants.NT_DAM_ASSETCONTENT)){
          String projectPath = DamUtil.getInheritedProperty("projectPath", addedResource, "");
          Resource project = adminResolver.getResource(projectPath);
          sendNotificationEmail(adminResolver, project);
        }

        notifyAssetCreaterForComments(adminResolver, addedResource);
      }
    }catch (LoginException | RepositoryException e) {
      log.error("RepositoryException while Executing events", e);
    }

  }

  /**
   * Method to notify the user for comments
   *
   * @param adminResolver - Resolver Object to get user manager
   * @param addedResource - comments added to the image
   * @throws RepositoryException - thrown when there is a error accessing the repository
   */
  private void notifyAssetCreaterForComments(ResourceResolver adminResolver, Resource addedResource)
      throws RepositoryException {
    if(StringUtils.equals(addedResource.getValueMap().get(BnpConstants.SLING_RESOURCETYPE, StringUtils.EMPTY), "granite/comments/components/comment")){
      String createdBy = addedResource.getValueMap().get(com.day.cq.commons.jcr.JcrConstants.JCR_CREATED_BY, StringUtils.EMPTY);
      final UserManager userManager = adminResolver.adaptTo(UserManager.class);
      Authorizable user = userManager.getAuthorizable(createdBy);

      if(!user.isGroup()){
        String email = user.getProperty(PROFILE_EMAIL) != null ? user.getProperty(PROFILE_EMAIL)[0].getString() : StringUtils.EMPTY;
        if(StringUtils.isNotEmpty(email)){
          String[] emailRecipients = {email};
          Map<String, String> emailParams = new HashMap<>();
          String assetName = addedResource.getParent().getParent().getParent() != null ? addedResource.getParent().getParent().getParent().getName() : StringUtils.EMPTY;
          String userName = ((User)user).getProperty(BnpConstants.PROFILE_GIVEN_NAME)[0].getString();
          emailParams.put("firstname", userName);
          String subject = "Mediahub - Comment added in the asset : " + assetName;
          emailParams.put(BnpConstants.SUBJECT, subject);
          genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/commentsnotificationtemplate.html", emailRecipients, emailParams);
        }
      }
    }
  }

  /**
   * Method to notify users from the project
   *
   * @param adminResolver - Resolver Object to get user manager
   * @param project - Project Resource to notify the users from project
   * @throws RepositoryException
   */
  private void sendNotificationEmail(ResourceResolver adminResolver, Resource project)
      throws RepositoryException {
    if(null == project){
      return;
    }

    String manager = project.getValueMap().get(BnpConstants.ROLE_OWNER, StringUtils.EMPTY);
    final UserManager userManager= adminResolver.adaptTo(UserManager.class);
    Authorizable authorizable = userManager.getAuthorizable(manager);
    if(authorizable.isGroup()){
      Group group = (Group)authorizable;
      Iterator<Authorizable> userResources = group.getMembers();
      while(userResources.hasNext()){
        Authorizable user = userResources.next();
        String email = user.getProperty(PROFILE_EMAIL) != null ? user.getProperty(PROFILE_EMAIL)[0].getString() : StringUtils.EMPTY;
        String[] emailRecipients = {email};
        Map<String, String> emailParams = new HashMap<>();
        String title = project.getChild(JcrConstants.JCR_CONTENT) != null ? project.getChild(JcrConstants.JCR_CONTENT).getValueMap().get(com.day.cq.commons.jcr.JcrConstants.JCR_TITLE, StringUtils.EMPTY) : StringUtils.EMPTY;
        String userName = ((User)user).getProperty(BnpConstants.PROFILE_GIVEN_NAME) != null ? ((User)user).getProperty(BnpConstants.PROFILE_GIVEN_NAME)[0].getString() : StringUtils.EMPTY;
        emailParams.put("firstname", userName);
        String subject = "Mediahub - Asset Added in the Project : " + title;
        emailParams.put(BnpConstants.SUBJECT, subject);
        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/addassetnotificationtemplate.html", emailRecipients, emailParams);
      }
    }
  }
}
