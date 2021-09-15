package com.mediahub.core.schedulers;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.UserUtils;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.JcrConstants;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
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

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Cron Job for Deactivating external user with past expiry date.
 * Currently runs every day at mid night
 */
@SuppressWarnings("CQRules:AMSCORE-553")
@Designate(ocd = UserDeactivationScheduledTask.Config.class)
@Component(service = Runnable.class)
public class UserDeactivationScheduledTask implements Runnable {

    private static final String PROFILE_EMAIL = "./profile/email";

    private static final String ROLE_OWNER = "role_owner";

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference
    private GenericEmailNotification genericEmailNotification;

    @Reference
    private SlingSettingsService slingSettingsService;

    @Reference
    I18nProvider provider;

    @ObjectClassDefinition(name = "User Deactivation Task",
            description = "User Deactivation cron-job past expiray date")
    public static @interface Config {

        @AttributeDefinition(name = "Cron-job expression")
        String scheduler_expression() default "0 6 0 * * ? *";

        @AttributeDefinition(name = "Concurrent task",
                description = "Whether or not to schedule this task concurrently")
        boolean scheduler_concurrent() default true;

        @AttributeDefinition(name = "A parameter",
                description = "Can be configured in /system/console/configMgr")
        String getUserType() default BnpConstants.EXTERNAL;
    }

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private String userType;

    @Override
    public void run() {
        logger.debug("Cron Job to deactiavte user of type ='{}'", userType);
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");

        try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            final UserManager userManager = resolver.adaptTo(UserManager.class);
            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = getPredicateMap();
            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            Iterator<Resource> userResources = result.getResources();
            while (userResources.hasNext()) {
                Resource user = userResources.next();
                Resource userProfile = user.getChild(BnpConstants.PROFILE);
                String expiryDate = userProfile != null ? userProfile.getValueMap().get(BnpConstants.EXPIRY, String.class) : StringUtils.EMPTY;
                if (StringUtils.isNotBlank(expiryDate)) {
                    deactivateExpiredUsers(userManager, user, expiryDate, builder, resolver);
                }
            }
            if (resolver.hasChanges()) {
                resolver.commit();
            }
        } catch (LoginException | PersistenceException | ParseException | RepositoryException e) {
            logger.error("Error while deactivating user {}", e.getMessage());
        }

    }

    /**
     * deactivate user having expiry date post current date
     *
     * @param userManager - user manager to get authorizable
     * @param user        - expired user
     * @param expiryDate  - expiry date in string format
     * @throws ParseException                - Thrown when there is a issue while parsing date
     * @throws javax.jcr.RepositoryException - Thrown while accessing nodes in JCR
     */
    protected void deactivateExpiredUsers(UserManager userManager, Resource user, String expiryDate, QueryBuilder builder, ResourceResolver resolver)
            throws ParseException, javax.jcr.RepositoryException {
        if (StringUtils.contains(expiryDate, "/")) {
            Authorizable authorizable = userManager.getAuthorizableByPath(user.getPath());
            SimpleDateFormat dateFormat = getSimpleDateFormat(expiryDate);

            Date actualDueDate = dateFormat.parse(expiryDate);
            Date currentDate = ProjectExpireNotificationUtil.getCurrentDate(dateFormat);
            int differenceInDays = (int) ((actualDueDate.getTime() - currentDate.getTime()) / (1000 * 60 * 60 * 24));

            if (!(authorizable instanceof User && !((User) authorizable).isDisabled())) {
                return;
            }

            if (differenceInDays == 30) {
                Iterator<Group> groupIterator = authorizable.memberOf();
                String groupName = getProjectGroupFromUser(groupIterator);
                if (StringUtils.isNotEmpty(groupName)) {
                    Set<String> managers = getMembersFromGroup(userManager, builder, resolver, groupName, ROLE_OWNER);
                    // Adding email of the user
                    String email = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL, String.class);

                    //send mail to user
                    String[] emailRecipients = {email};
                    sendWarningMail(user, emailRecipients, "/etc/mediahub/mailtemplates/userexpirationmailtemplate.html");

                    //send mail to managers
                    String[] managerEmail = managers.toArray(new String[0]);
                    sendWarningMail(user, managerEmail, "/etc/mediahub/mailtemplates/userexpirationmanagernotificationmailtemplate.html");
                }
            } else if (differenceInDays <= 0) {

                logger.info(user.getPath());
                ((User) authorizable).disable(BnpConstants.USER_HAS_EXPIRED);

                Iterator<Group> groupIterator = authorizable.memberOf();
                String groupName = getProjectGroupFromUser(groupIterator);
                if (StringUtils.isNotEmpty(groupName)) {
                    Set<String> managers = getMembersFromGroup(userManager, builder, resolver, groupName,
                            ROLE_OWNER);
                    fetchEmailFromSuperAdmin(userManager, managers);

                    // Adding email of the user
                    String email = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL, String.class);

                    //send mail to user
                    String[] emailRecipients = {email};
                    sendDeactivationgMail(user, emailRecipients, "/etc/mediahub/mailtemplates/userdeactivationmailtemplate.html");

                    //send mail to managers
                    String[] managerEmail = managers.toArray(new String[0]);
                    sendDeactivationgMail(user, managerEmail, "/etc/mediahub/mailtemplates/userdeactivationmanagernotificationmailtemplate.html");
                }
            }

        }
    }

    /**
     * Method to get date format from string value
     *
     * @param expiryDate - expiry date in string format
     * @return SimpleDateFormat object
     */
    private SimpleDateFormat getSimpleDateFormat(String expiryDate) {
        SimpleDateFormat dateFormat;
        if (expiryDate.indexOf('/') == 2) {
            dateFormat = new SimpleDateFormat(BnpConstants.DD_MM_YYYY);
        } else {
            dateFormat = new SimpleDateFormat(BnpConstants.YYYY_MM_DD);
        }
        return dateFormat;
    }

    private void fetchEmailFromSuperAdmin(UserManager userManager, Set<String> managers)
            throws RepositoryException {
        Authorizable bnpsuperAdmin = userManager.getAuthorizable("mediahub-administrators");
        if (null != bnpsuperAdmin && bnpsuperAdmin.isGroup()) {
            Iterator<Authorizable> superAdmins = ((Group) bnpsuperAdmin).getMembers();
            superAdmins.forEachRemaining(superAdmin -> {
                try {
                    if (superAdmin.getProperty(PROFILE_EMAIL) != null) {
                        managers.add(superAdmin.getProperty(PROFILE_EMAIL)[0].getString());
                    }
                } catch (RepositoryException e) {
                    logger.error("Error while trying to fecth details from users in bnp super admin", e);
                }
            });
        }
    }

    /**
     * Method to send user deactivation within 30 days notice
     *
     * @param user            - AEM user
     * @param emailRecipients - Email recipient to which mail to be sent
     * @param templatePath    - template path to create email
     */
    protected void sendWarningMail(Resource user, String[] emailRecipients, String templatePath) {
        String language = UserUtils.getUserLanguage(user);
        Locale locale = LocaleUtils.toLocale(language);
        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("User will be Deactivated in 30 days", locale);
        Map<String, String> emailParams = new HashMap<>();
        emailParams.put(BnpConstants.SUBJECT, subject);
        Resource profile = user.getChild(BnpConstants.PROFILE);
        if (profile != null) {
            emailParams.put(BnpConstants.FIRSTNAME, profile.getValueMap().get(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
        } else {
            emailParams.put(BnpConstants.FIRSTNAME, StringUtils.EMPTY);
        }
        genericEmailNotification.sendEmail(templatePath, emailRecipients, emailParams);
    }

    /**
     * Method to send user deactivation
     *
     * @param user            - AEM user
     * @param emailRecipients - Email recipient to which mail to be sent
     * @param templatePath    - template path to create email
     */
    protected void sendDeactivationgMail(Resource user, String[] emailRecipients, String templatePath) {
        String language = UserUtils.getUserLanguage(user);
        Locale locale = LocaleUtils.toLocale(language);
        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("User will be Deactivated", locale);
        Map<String, String> emailParams = new HashMap<>();
        emailParams.put(BnpConstants.SUBJECT, subject);
        Resource profile = user.getChild(BnpConstants.PROFILE);
        if (profile != null) {
            emailParams.put(BnpConstants.FIRSTNAME, profile.getValueMap().get(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
        } else {
            emailParams.put(BnpConstants.FIRSTNAME, StringUtils.EMPTY);
        }
        genericEmailNotification.sendEmail(templatePath, emailRecipients, emailParams);
    }

    /**
     * @param userManager - user manager to get authorizable
     * @param builder     - query builder to create query from map object
     * @param resolver    - Resource resolver
     * @param groupName   - name of the group
     * @param role        - role of the user in project
     * @return
     */
    protected Set<String> getMembersFromGroup(UserManager userManager, QueryBuilder builder,
                                              ResourceResolver resolver, String groupName, String role) {
        Set<String> managers = new HashSet<>();

        if (StringUtils.isBlank(groupName)) {
            return managers;
        }


        Map<String, String> predicateMapForQuery = getPredicateMapProjectSearch(BnpConstants.AEM_PROJECTS_PATH, groupName);
        Query query = builder.createQuery(
                PredicateGroup.create(predicateMapForQuery), resolver.adaptTo(Session.class));
        SearchResult result = query.getResult();
        Iterator<Resource> projectResources = result.getResources();

        projectResources.forEachRemaining(project -> {
            if (StringUtils.equals(project.getValueMap().get(BnpConstants.SLING_RESOURCETYPE, StringUtils.EMPTY), "cq/gui/components/projects/admin/card/projectcard")) {
                String roleOwnerGroup = project.getValueMap().get(role, StringUtils.EMPTY);
                if (StringUtils.isNotEmpty(roleOwnerGroup)) {
                    fetchUserMailFromGroup(userManager, managers, roleOwnerGroup);
                }
            }
        });
        return managers;
    }

    /**
     * Method to fetch and add manager email
     *
     * @param userManager    - User manager object to get autorizable
     * @param managers       - set of manager emails to notify
     * @param roleOwnerGroup - role of the user
     */
    protected void fetchUserMailFromGroup(UserManager userManager, Set<String> managers,
                                          String roleOwnerGroup) {
        try {
            Authorizable authorizableOwnersGroup = userManager.getAuthorizable(roleOwnerGroup);
            if (authorizableOwnersGroup != null && authorizableOwnersGroup.isGroup()) {
                Group ownerGroup = (Group) authorizableOwnersGroup;
                Iterator<Authorizable> projectOwners = ownerGroup.getMembers();
                while (projectOwners.hasNext()) {
                    Authorizable owner = projectOwners.next();
                    if (!owner.isGroup() && owner.getProperty(PROFILE_EMAIL) != null) {
                        managers.add(owner.getProperty(PROFILE_EMAIL)[0].getString());
                    }
                }
            }
        } catch (RepositoryException e) {
            logger.error("Error while fecthing user details from group", e);
        }
    }

    /**
     * @param groupIterator - groups the user belongs to
     * @return - group of external contributor from the user's group
     * @throws RepositoryException - Thrown while accessing nodes in JCR
     */
    protected String getProjectGroupFromUser(Iterator<Group> groupIterator)
            throws RepositoryException {
        while (groupIterator.hasNext()) {
            Group group = groupIterator.next();
            if (StringUtils.contains(group.getPrincipal().getName(), "external-contributor")) {
                return group.getPrincipal().getName();
            }
        }
        return StringUtils.EMPTY;
    }

    /**
     * @return predicate map for query to be framed
     */
    protected Map<String, String> getPredicateMap() {
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, BnpConstants.HOME_USERS);
        map.put(BnpConstants.TYPE, BnpConstants.REP_USERS);
        map.put(BnpConstants.FIRST_PROPERTY, BnpConstants.PROFILE_TYPE);
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, userType);
        map.put("p.limit", "-1");
        return map;
    }

    /**
     * @return predicate map for query to be framed
     */
    protected Map<String, String> getPredicateMapProjectSearch(String path, String externalContributer) {
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, path);
        map.put(BnpConstants.TYPE, JcrConstants.NT_UNSTRUCTURED);
        map.put(BnpConstants.FIRST_PROPERTY, "role_external-contributor");
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, externalContributer);
        map.put("p.limit", "-1");
        return map;
    }

    @Activate
    protected void activate(final Config config) {
        userType = config.getUserType();
    }

}
