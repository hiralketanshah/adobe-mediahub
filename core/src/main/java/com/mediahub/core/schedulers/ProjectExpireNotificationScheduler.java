package com.mediahub.core.schedulers;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectManager;
import com.day.cq.commons.Externalizer;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.QueryUtils;
import com.mediahub.core.utils.UserUtils;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.Session;
import org.apache.commons.lang.LocaleUtils;
import org.apache.http.client.utils.DateUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Scheduler class to send Project Expire Email Notification to project group users.
 * <p>
 * Scheduler runs every day 2am.
 */
@Designate(ocd = ProjectExpireNotificationScheduler.Config.class)
@Component(service = Runnable.class)
public class ProjectExpireNotificationScheduler implements Runnable {

    public static final String YYYY_MM_DD = "yyyy-MM-dd";
    private final Logger logger = LoggerFactory.getLogger(getClass());
    @Reference
    private Externalizer externalizer;

    @ObjectClassDefinition(
            name = "MediaHub Project Expire Notification Scheduler",
            description = "MediaHub Project Expiry Notification Scheduler")
    public static @interface Config {

        @AttributeDefinition(name = "Cron-job expression")
        String scheduler_expression() default "0 0 6 1/1 * ? *";

        @AttributeDefinition(
                name = "Concurrent task",
                description = "Whether or not to schedule this task concurrently")
        boolean scheduler_concurrent() default true;

        @AttributeDefinition(name = "Project Path", description = "A Path from where Project are Fetched")
        String getProjectPath() default BnpConstants.AEM_PROJECTS_PATH;
    }

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    GenericEmailNotification genericEmailNotification;

    @Reference
    private SlingSettingsService slingSettingsService;

    @Reference
    I18nProvider provider;

    private String projectPath;

    @Override
    public void run() {
        logger.debug("MediaHub Project Expiry Notification Scheduler");
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                "writeService");
        ResourceResolver resolver = null;
        try {
            resolver = resolverFactory.getServiceResourceResolver(authInfo);

            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = QueryUtils.getPredicateMapProjectDueDate(projectPath);
            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));

            SearchResult result = query.getResult();
            logger.info("Query {}", result.getQueryStatement());
            String projectOwnerProperty = null;
            for (Hit hit : result.getHits()) {
                String path = hit.getPath();
                logger.debug("Projecs Paths : {}", path);

                int index = path.lastIndexOf('/');
                String projectpath = path.substring(0, index);

                Resource adminResource = resolver.getResource(path);
                Node jcrContentNode = adminResource.adaptTo(Node.class);
                String projectactualDueDate = jcrContentNode.getProperty(BnpConstants.PROJECT_DUEDATE).getValue()
                        .getString();
                ProjectManager projectManager = resolver.adaptTo(ProjectManager.class);
                Project project = resolver.getResource(projectpath).adaptTo(Project.class);
                logger.debug("Projecs due date : {}", projectactualDueDate);
                Date actualDueDate = DateUtils.parseDate(projectactualDueDate, new String[]{
                    YYYY_MM_DD});
                Date crrentDate = DateUtils.parseDate(DateUtils.formatDate(Calendar.getInstance().getTime(),YYYY_MM_DD), new String[]{YYYY_MM_DD});
                int differenceInDays = (int) ((actualDueDate.getTime() - crrentDate.getTime()) / (1000 * 60 * 60 * 24));
                logger.debug("difference jour : {}", differenceInDays);
                if (differenceInDays == 30 || differenceInDays == 0 || differenceInDays <= -31) {
                    Resource groupOwnerResource = resolver.getResource(projectpath);
                    Node projectNode = groupOwnerResource.adaptTo(Node.class);
                    projectOwnerProperty = projectNode.getProperty(BnpConstants.ROLE_OWNER).getValue().getString();
                    UserManager userManager = resolver.adaptTo(UserManager.class);
                    Authorizable authorizable = userManager.getAuthorizable(projectOwnerProperty);
                    org.apache.jackrabbit.api.security.user.Group group = (org.apache.jackrabbit.api.security.user.Group) authorizable;
                    Iterator<Authorizable> itr = group.getDeclaredMembers();
                    Map<String, String> emailParams = new HashMap<>();
                    emailParams.put("projectitle", project.getTitle());
                    emailParams.put("projecturl", externalizer.authorLink(resolver, "/projects/details.html" + projectpath));

                    sendProjectExpiryNotification(jcrContentNode, differenceInDays, userManager,
                        itr, emailParams);
                    if (differenceInDays <= -31) {
                        logger.debug(" deletion");
                        projectManager.deleteProject(project);
                    }
                }
            }
            if (resolver.hasChanges()) {
                resolver.commit();
            }
            resolver.close();
        } catch (Exception e) {
            logger.error("Error while project expire notification  {}", e.getMessage());
        }
    }

    private void sendProjectExpiryNotification(Node jcrContentNode, int differenceInDays,
        UserManager userManager, Iterator<Authorizable> itr, Map<String, String> emailParams)
        throws javax.jcr.RepositoryException {
        while (itr.hasNext()) {
            Object obj = itr.next();
            if (obj instanceof User) {
                User user = (User) obj;
                String userID = user.getID();
                logger.debug("userID : {} ", userID);
                Authorizable userAuthorization = userManager.getAuthorizable(userID);


                if (userAuthorization.hasProperty(BnpConstants.PEOFILE_EMAIL)
                        && userAuthorization.getProperty(BnpConstants.PEOFILE_EMAIL) != null
                        && userAuthorization.getProperty(BnpConstants.PEOFILE_EMAIL).length > 0) {
                    String userName = userAuthorization.getProperty(BnpConstants.PROFILE_GIVEN_NAME)[0]
                            .getString();
                    logger.debug("User GivenName : {} ", userName);

                    String userEmailID = userAuthorization.getProperty(BnpConstants.PEOFILE_EMAIL)[0]
                            .getString();

                    logger.debug("EMailID----- : {} ", userEmailID);
                    String[] emailRecipients = {userEmailID};
                    emailParams.put("firstname", userName);
                    String language = UserUtils.getUserLanguage(userAuthorization);
                    Locale locale = LocaleUtils.toLocale(language);

                    if (differenceInDays == 30) {
                        logger.debug("Notification deletion dans 1mois");
                        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Project Expiration", locale);
                        emailParams.put(BnpConstants.SUBJECT, subject);
                        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectexpirationmailtemplate.html", emailRecipients, emailParams);

                    } else if (differenceInDays == 0) {
                        logger.debug("Notification deactivation");
                        jcrContentNode.setProperty(BnpConstants.ACTIVE, false);
                        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Project Deactivation", locale);
                        emailParams.put(BnpConstants.SUBJECT, subject);
                        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectdeactivationmailtemplate.html", emailRecipients, emailParams);

                    } else if (differenceInDays <= -31) {
                        logger.debug("Notification deletion");
                        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Project Deletion", locale);
                        emailParams.put(BnpConstants.SUBJECT, subject);
                        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectdeletionmailtemplate.html", emailRecipients, emailParams);

                    }
                }
            }
        }
    }

    @Activate
    protected void activate(final Config config) {
        projectPath = config.getProjectPath();
    }

}
