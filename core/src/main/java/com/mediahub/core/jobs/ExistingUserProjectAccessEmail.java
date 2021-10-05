package com.mediahub.core.jobs;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.QueryUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = JobConsumer.class, immediate = true,
    property = {Constants.SERVICE_DESCRIPTION + "=Job to send project access email to user",
        JobConsumer.PROPERTY_TOPICS + "=" + "user/project/access/email"})
public class ExistingUserProjectAccessEmail implements JobConsumer {

  /**
   * Logger
   */
  private static final Logger log = LoggerFactory.getLogger(ExistingUserProjectAccessEmail.class);

  @Reference
  private ResourceResolverFactory resolverFactory;

  @Reference
  private GenericEmailNotification genericEmailNotification;

  @Override
  public JobResult process(Job job) {
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);
    String path = job.getProperty(BnpConstants.PATH, String.class);
    try(ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      if(null != job.getProperty(BnpConstants.AFTER_VALUE)){
        List<String> after = job.getProperty(BnpConstants.AFTER_VALUE,new ArrayList<>());
        if(filterModifiedValues(job, after) ) {
          for(String uuid : after){
            Map<String, String> emailParams = new HashMap<>();
            emailParams.put(BnpConstants.SUBJECT, "Welcome Email");
            Resource group = resourceResolver.getResource(path);
            getProjectDetails(job, resourceResolver, emailParams, group);
            Node userNode = resourceResolver.adaptTo(Session.class).getNodeByIdentifier(uuid);
            if(null != userNode && null != userNode.getNode(BnpConstants.PROFILE)){
              Node profile = userNode.getNode(BnpConstants.PROFILE);
              ValueMap profileProperties = resourceResolver.getResource(profile.getPath()).getValueMap();

              String email = StringUtils.EMPTY;
              if(profileProperties.containsKey(BnpConstants.EMAIL)){
                email = profileProperties.get(BnpConstants.EMAIL, StringUtils.EMPTY);
              } else {
                email = resourceResolver.getResource(userNode.getPath()).getValueMap().get("rep:authorizableId", StringUtils.EMPTY);
              }
              String[] emailRecipients = {email};

              emailParams.put(BnpConstants.FIRSTNAME, profileProperties.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
              emailParams.put(BnpConstants.EXPIRY, profileProperties.get(BnpConstants.EXPIRY, StringUtils.EMPTY));

              if(StringUtils.equals(profileProperties.get(BnpConstants.TYPE, StringUtils.EMPTY), BnpConstants.BROADCAST_VALUE_INTERNAL)){
                genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentexistinginternalusers.html", emailRecipients, emailParams);
              } else {
                genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentexistingexternalusers.html", emailRecipients, emailParams);
              }
            }
          }
        }
      }
    } catch (RepositoryException | LoginException e) {
      log.error("Error while sending user notification mail", e);
      return JobResult.FAILED;
    }

    return JobResult.OK;
  }

  private void getProjectDetails(Job job, ResourceResolver resourceResolver,
      Map<String, String> emailParams, Resource group) throws RepositoryException {
    if(StringUtils.equals("rep:members", group.getName())){
      group = group.getParent();
      ValueMap properties = group.getValueMap();
      if(properties.containsKey("rep:principalName")){
        QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
        Map<String, String> map = QueryUtils
            .getPredicateMapProjectRole(properties.get(BnpConstants.REP_PRINCIPAL_NAME, StringUtils.EMPTY));
        Query query = builder.createQuery(
            PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
        SearchResult result = query.getResult();
        log.debug("Query {}", result.getQueryStatement());
        for (Hit hit : result.getHits()) {
          if(null != hit.getResource().getChild(JcrConstants.JCR_CONTENT)){
            emailParams.put("projectitle", hit.getResource().getChild(JcrConstants.JCR_CONTENT).getValueMap().get(JcrConstants.JCR_TITLE, StringUtils.EMPTY));
          }
          emailParams.put("projecturl", hit.getPath());
          emailParams.put("projectowner", job.getProperty("userID", StringUtils.EMPTY));
          break;
        }
      }
    }
  }


  private boolean filterModifiedValues(Job job, List<String> after) {
    if(null != job.getProperty(BnpConstants.BEFORE_VALUE)){
      List<String> before = job.getProperty(BnpConstants.BEFORE_VALUE, new ArrayList<>());
      if(after.size() > before.size()){
        return after.removeAll(before);
      } else {
        return false;
      }
    } else {
      return true;
    }
  }

}
