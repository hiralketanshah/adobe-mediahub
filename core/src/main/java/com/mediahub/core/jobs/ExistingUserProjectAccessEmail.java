package com.mediahub.core.jobs;

import com.adobe.acs.commons.email.EmailServiceConstants;
import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.QueryUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.*;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import java.util.*;

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

    @SuppressWarnings("CQRules:AMSCORE-553")
    @Reference
    private SlingSettingsService slingSettingsService;

    @Reference
    I18nProvider provider;

    @Override
    public JobResult process(Job job) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        String path = job.getProperty(BnpConstants.PATH, String.class);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {

            if(null == job.getProperty(BnpConstants.AFTER_VALUE)){
                log.error("Error while fetching value of members", "Since there is no after value");
                return JobResult.FAILED;
            }

            List<String> after = job.getProperty(BnpConstants.AFTER_VALUE, new ArrayList<>());
            if (filterModifiedValues(job, after)) {
                for (String uuid : after) {
                    Map<String, String> emailParams = new HashMap<>();
                    Resource group = resourceResolver.getResource(path);
                    getProjectDetails(job, resourceResolver, emailParams, group);
                    String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Invitation to join « projectitle » MediaHub project // Invitation pour rejoindre le projet MediaHub « projectitle »", Locale.ENGLISH);
                    subject = subject.replaceAll(BnpConstants.PROJECT_TITLE, emailParams.get(BnpConstants.PROJECT_TITLE));
                    emailParams.put(EmailServiceConstants.SUBJECT, subject);
                    Node userNode = resourceResolver.adaptTo(Session.class).getNodeByIdentifier(uuid);
                    if (null != userNode && null != userNode.getNode(BnpConstants.PROFILE)) {
                        Node profile = userNode.getNode(BnpConstants.PROFILE);
                        ValueMap profileProperties = resourceResolver.getResource(profile.getPath()).getValueMap();
                        String email = fetchEmail(resourceResolver, userNode, profileProperties);
                        String[] emailRecipients = {email};

                        emailParams.put(BnpConstants.FIRSTNAME, profileProperties.get(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
                        emailParams.put(BnpConstants.EXPIRY, profileProperties.get(BnpConstants.EXPIRY, StringUtils.EMPTY));

                        if(StringUtils.equals(emailParams.getOrDefault("role", StringUtils.EMPTY), "Observers")){
                            sendObserverEmail(uuid, emailParams, group, profileProperties,
                                emailRecipients);
                        } else {
                            sendEmail(emailParams, profileProperties, emailRecipients);
                        }
                    }
                }
            }

        } catch (RepositoryException | LoginException e) {
            log.error("Error while sending user notification mail", e);
            return JobResult.FAILED;
        } catch (InterruptedException e) {
            log.error("Error while making thread sleep for 5 seconds", e);
            return JobResult.FAILED;
        }

        return JobResult.OK;
    }

    private String fetchEmail(ResourceResolver resourceResolver, Node userNode,
        ValueMap profileProperties) throws RepositoryException {
        String email;
        if (profileProperties.containsKey(BnpConstants.EMAIL)) {
            email = profileProperties.get(BnpConstants.EMAIL, StringUtils.EMPTY);
        } else {
            email = resourceResolver.getResource(userNode.getPath()).getValueMap().get("rep:authorizableId", StringUtils.EMPTY);
        }
        return email;
    }

    private void sendObserverEmail(String uuid, Map<String, String> emailParams, Resource group,
        ValueMap profileProperties, String[] emailRecipients) throws InterruptedException {
        Thread.sleep(5000);
        Resource project = group.getParent().getParent();
        Boolean isObserver = true;
        Iterator<Resource> children = project.listChildren();
        while(children.hasNext()){
            Resource child = children.next();
            if(!StringUtils.equals(child.getName(), group.getParent().getName())){
                ValueMap properties =  child.getValueMap();
                if(properties.containsKey(BnpConstants.REP_MEMBERS)){
                    String[] members = properties.get(BnpConstants.REP_MEMBERS, new String[]{""});
                    if(Arrays.asList(members).contains(uuid)){
                        isObserver = false;
                        break;
                    }
                }
            }
        }

        if(isObserver){
            sendEmail(emailParams, profileProperties, emailRecipients);
        }
    }

    private void sendEmail(Map<String, String> emailParams, ValueMap profileProperties,
        String[] emailRecipients) {
        if (StringUtils
            .equals(profileProperties.get(BnpConstants.TYPE, StringUtils.EMPTY), BnpConstants.BROADCAST_VALUE_INTERNAL)) {
            genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentexistinginternalusers.html", emailRecipients, emailParams);
        } else {
            genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentexistingexternalusers.html", emailRecipients, emailParams);
        }
    }

    private void getProjectDetails(Job job, ResourceResolver resourceResolver,
                                   Map<String, String> emailParams, Resource group) throws RepositoryException {
        if (StringUtils.equals(BnpConstants.REP_MEMBERS, group.getName())) {
            group = group.getParent();
            ValueMap properties = group.getValueMap();
            if (properties.containsKey("rep:principalName")) {
                QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
                Map<String, String> map = QueryUtils
                        .getPredicateMapProjectRole(properties.get(BnpConstants.REP_PRINCIPAL_NAME, StringUtils.EMPTY));
                Query query = builder.createQuery(
                        PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
                SearchResult result = query.getResult();
                log.debug("Query {}", result.getQueryStatement());
                for (Hit hit : result.getHits()) {
                    if (null != hit.getResource().getChild(JcrConstants.JCR_CONTENT)) {
                        emailParams.put(BnpConstants.PROJECT_TITLE, hit.getResource().getChild(JcrConstants.JCR_CONTENT).getValueMap().get(JcrConstants.JCR_TITLE, StringUtils.EMPTY));
                    }
                    emailParams.put("projecturl", hit.getPath());
                    emailParams.put("projectowner", job.getProperty("userID", StringUtils.EMPTY));
                    break;
                }
            }

            if(null != group.getChild("profile")){
                 emailParams.put("role", group.getChild("profile").getValueMap().get("alternateTitle", StringUtils.EMPTY));
            }
        }
    }


    private boolean filterModifiedValues(Job job, List<String> after) {
        if (null != job.getProperty(BnpConstants.BEFORE_VALUE)) {
            List<String> before = job.getProperty(BnpConstants.BEFORE_VALUE, new ArrayList<>());
            if (after.size() > before.size()) {
                after.removeAll(before);
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    }

}
