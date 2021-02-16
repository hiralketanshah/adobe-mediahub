package com.mediahub.core.models.impl;

import com.adobe.granite.security.user.UserProperties;
import com.adobe.granite.security.user.UserPropertiesManager;
import com.adobe.granite.security.user.UserPropertiesService;
import com.adobe.granite.security.user.util.AuthorizableUtil;
import com.day.cq.i18n.I18n;
import com.day.cq.replication.ReplicationActionType;
import com.day.cq.replication.ReplicationStatus;
import com.mediahub.core.models.AuthorizableModel;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.SyntheticResource;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.models.annotations.Default;
import org.apache.sling.models.annotations.Model;
import org.apache.sling.models.annotations.injectorspecific.OSGiService;
import org.apache.sling.models.annotations.injectorspecific.Self;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.PostConstruct;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import javax.inject.Inject;

@Model(adaptables = SlingHttpServletRequest.class, adapters = AuthorizableModel.class)
public class AuthorizableModelImpl implements AuthorizableModel {
  private static Logger LOG = LoggerFactory.getLogger(AuthorizableModelImpl.class);

  @Self
  protected SlingHttpServletRequest request;

  @OSGiService(optional = true)
  protected UserPropertiesService upService;

  @Inject @Default(intValues = 24)
  protected int hoursLimit;

  protected UserProperties userProperties;
  protected Authorizable authorizable;
  protected ResourceResolver resolver;

  private ReplicationStatus replicationStatus;
  private ValueMap authorizableProps;
  private Resource resource;
  private I18n i18n;


  @PostConstruct
  protected void postConstruct() throws RepositoryException {
    resolver = request.getResourceResolver();
    i18n = new I18n(request);
    resource = request.getResource();
    authorizable = resource.adaptTo(Authorizable.class);

    //try to get the authorizable resource from the suffix if the resource is not an authorizable
    if (authorizable == null) {
      String suffix = request.getRequestPathInfo().getSuffix();
      if (suffix != null) {
        Resource suffixResource = resolver.getResource(suffix);
        if (suffixResource != null) {
          authorizable = suffixResource.adaptTo(Authorizable.class);
        }
      }
    }

    if (resource != null && authorizable != null) {
      authorizableProps = resource.adaptTo(ValueMap.class);

      replicationStatus = resource.adaptTo(ReplicationStatus.class);
      Session session = resolver.adaptTo(Session.class);
      UserPropertiesManager userPropManager = upService.createUserPropertiesManager(session, resolver);
      userProperties = AuthorizableUtil.getProfile(userPropManager, authorizable.getID());
    }

  }

  @Override
  public String getId() {
    try {
      return authorizable.getID();
    } catch (RepositoryException e) {
      LOG.error("Unable to obtain authorizable ID: ", e);
    }
    return null;
  }

  @Override
  public String getHomePath() {
    try {
      //handle dynamic groups case which don't have  a path
      if (resource instanceof SyntheticResource || authorizable == null) {
        return null;
      }
      return authorizable.getPath();
    } catch (RepositoryException e) {
      LOG.error("Unable to obtain authorizable home: ", e);
    }
    return null;
  }

  @Override
  public String getName() {
    //handle dynamic groups case which save the principal name on a synthetic resource
    if (resource instanceof SyntheticResource) {
      return resource.getValueMap().get("principalName", "");
    } else {
      if (authorizable != null) {
        final String nameDisplayOrder = i18n.get("{0} {1}","name display order: {0} is the given (first) name, {1} the family (last)" +
            " name","givenName middleName","familyName");
        return AuthorizableUtil.getFormattedName(resolver, authorizable, nameDisplayOrder);
      }
    }
    return null;
  }

  @Override
  public boolean isNew() {
    Calendar created = authorizableProps.get("jcr:created", Calendar.class);
    Calendar lastModified = authorizableProps.get("jcr:lastModified", Calendar.class);

    Calendar hoursLimitAgo = Calendar.getInstance();
    hoursLimitAgo.add(Calendar.HOUR_OF_DAY, -hoursLimit);

    if (created == null || (lastModified != null && lastModified.before(created))) {
      created = lastModified;
    }

    return created != null && hoursLimitAgo.before(created);
  }

  @Override
  public String getLastModified() {
    Calendar lastModified = authorizableProps.get("jcr:lastModified", Calendar.class);
    if (lastModified != null) {
      SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
      return sdf.format(lastModified.getTime());
    }
    return null;
  }

  @Override
  public String getLastModifiedBy() {
    return AuthorizableUtil.getFormattedName(resolver, authorizableProps.get("jcr:lastModifiedBy", String.class));
  }

  @Override
  public String getLastPublished() {
    Calendar lastPublished = replicationStatus.getLastPublished();
    if (lastPublished != null) {
      if (! ReplicationActionType.DEACTIVATE.equals(replicationStatus.getLastReplicationAction())) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        return sdf.format(lastPublished.getTime());
      }
    }
    return null;
  }

  @Override
  public String getLastPublishedBy() {
    return AuthorizableUtil.getFormattedName(resolver, replicationStatus.getLastPublishedBy());
  }

  @Override
  public String getPhotoPath() {
    try {
      if (userProperties != null) {
        Resource photo = userProperties.getResource(UserProperties.PHOTOS + "/primary/image");
        if (photo != null) {
          return photo.getPath();
        }
      }
    } catch (RepositoryException e) {
      LOG.error("Unable to obtain user's photo path: ", e);
    }

    return null;
  }

  @Override
  public boolean isGroup() {
    return authorizable != null && authorizable.isGroup();
  }

  @Override
  public String getType(){
    String type = StringUtils.EMPTY;
    if(resource.getChild("profile") != null){
      type = resource.getChild("profile").getValueMap().get("type", StringUtils.EMPTY);
    }
    return type;
  }

  @Override
  public String getExpiry(){
    String expiry = StringUtils.EMPTY;
    if(resource.getChild("profile") != null){
      expiry = resource.getChild("profile").getValueMap().get("expiry", StringUtils.EMPTY);
    }
    return expiry;
  }

  @Override
  public String getCountry(){
    String expiry = StringUtils.EMPTY;
    if(resource.getChild("profile") != null){
      expiry = resource.getChild("profile").getValueMap().get("country", StringUtils.EMPTY);
    }
    return expiry;
  }

  @Override
  public String getCompany(){
    String expiry = StringUtils.EMPTY;
    if(resource.getChild("profile") != null){
      expiry = resource.getChild("profile").getValueMap().get("company", StringUtils.EMPTY);
    }
    return expiry;
  }
}
