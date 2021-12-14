package com.mediahub.core.schedulers;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
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
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

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
                Calendar expiryDate =  Calendar.getInstance();
                String expiry = userProfile.getValueMap().get(BnpConstants.EXPIRY, StringUtils.EMPTY);
                if(expiry.indexOf('/') == 2){
                    Date userExpiryDate = new SimpleDateFormat("dd/MM/yyyy").parse(expiry);
                    expiryDate.setTime(userExpiryDate);
                } else {
                    expiryDate = userProfile != null ? userProfile.getValueMap().get(BnpConstants.EXPIRY, Calendar.class) : null;
                }
                if (expiryDate != null) {
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
    protected void deactivateExpiredUsers(UserManager userManager, Resource user, Calendar expiryDate, QueryBuilder builder, ResourceResolver resolver)
            throws ParseException, javax.jcr.RepositoryException {
        Authorizable authorizable = userManager.getAuthorizableByPath(user.getPath());

        Duration duration = Duration.between(Instant.now(), expiryDate.toInstant());
        long differenceInDays = duration.toDays();

        if (!(authorizable instanceof User && !((User) authorizable).isDisabled())) {
            return;
        }


        String language = UserUtils.getUserLanguage(user);

        if (differenceInDays == 10) {

            Iterator<Group> groupIterator = authorizable.memberOf();
            List<String> groups = getProjectGroupsFromUser(groupIterator);
            // Adding email of the user
            String email = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL, String.class);
            String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Account deactivated within " + Math.abs(differenceInDays) + " days // Votre accès sera desactivé dans " +  Math.abs(differenceInDays) + " jours", Locale.ENGLISH);
            //send mail to user
            String[] emailRecipients = {email};
            sendMail(user, emailRecipients, "/etc/mediahub/mailtemplates/userexpirationmailtemplate.html", subject);
            if (!groups.isEmpty()) {
                for(String group : groups){
                    Set<Authorizable> managers = getMembersFromGroup(userManager, builder, resolver, group, ROLE_OWNER);
                    sendManagerNoficationMail(managers, "/etc/mediahub/mailtemplates/userexpirationmanagernotificationmailtemplate.html", subject);
                }
            }
        } else if (differenceInDays <= 0) {
            logger.debug(user.getPath());
            ((User) authorizable).disable(BnpConstants.USER_HAS_EXPIRED);
            Iterator<Group> groupIterator = authorizable.memberOf();
            List<String> groups = getProjectGroupsFromUser(groupIterator);
            // Adding email of the user
            String email = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL, String.class);
            String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("User will be Deactivated", Locale.ENGLISH);
            //send mail to user
            String[] emailRecipients = {email};
            sendMail(user, emailRecipients, "/etc/mediahub/mailtemplates/userdeactivationmailtemplate.html", subject);
            if (!groups.isEmpty()) {
                for(String group : groups){
                    Set<Authorizable> managers = getMembersFromGroup(userManager, builder, resolver, group, ROLE_OWNER);
                    fetchEmailFromSuperAdmin(userManager, managers);
                    sendManagerNoficationMail(managers, "/etc/mediahub/mailtemplates/userdeactivationmanagernotificationmailtemplate.html", subject);

                }
            }
        }

    }

    private void fetchEmailFromSuperAdmin(UserManager userManager, Set<Authorizable> managers)
        throws RepositoryException {
        Authorizable bnpsuperAdmin = userManager.getAuthorizable("mediahub-administrators");
        if (null != bnpsuperAdmin && bnpsuperAdmin.isGroup()) {
            Iterator<Authorizable> superAdmins = ((Group) bnpsuperAdmin).getMembers();
            superAdmins.forEachRemaining(superAdmin -> {
                try {
                    if (superAdmin.getProperty(PROFILE_EMAIL) != null) {
                        managers.add(superAdmin);
                    }
                } catch (RepositoryException e) {
                    logger.error("Error while trying to fecth details from users in bnp super admin", e);
                }
            });
        }
    }


    protected void sendMail(Resource user, String[] emailRecipients, String templatePath, String subject) {
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

    protected void sendManagerNoficationMail(Set<Authorizable> managers, String templatePath, String subject) {
        for(Authorizable manager : managers){
            try {
                Map<String, String> emailParams = new HashMap<>();
                emailParams.put(BnpConstants.SUBJECT, subject);
                String[] emailRecipients = new String[]{manager.getProperty(PROFILE_EMAIL)[0].getString()};
                String userName = "";
                if(manager.hasProperty("profile/givenName")){
                    userName = manager.getProperty("profile/givenName")[0].getString() + " ";
                } else if(manager.hasProperty("profile/familyName")){
                    userName = userName + manager.getProperty("profile/familyName")[0].getString();
                } else {
                    userName = manager.getProperty("rep:principalName")[0].getString();
                }
                emailParams.put(BnpConstants.FIRSTNAME, userName);
                genericEmailNotification.sendEmail(templatePath, emailRecipients, emailParams);
            } catch (RepositoryException e) {
                logger.error("Error while fecthing user details from group", e);;
            }
        }
    }

    /**
     * @param userManager - user manager to get authorizable
     * @param builder     - query builder to create query from map object
     * @param resolver    - Resource resolver
     * @param groupName   - name of the group
     * @param role        - role of the user in project
     * @return
     */
    protected Set<Authorizable> getMembersFromGroup(UserManager userManager, QueryBuilder builder,
                                              ResourceResolver resolver, String groupName, String role) {
        Set<Authorizable> managers = new HashSet<>();

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
    protected void fetchUserMailFromGroup(UserManager userManager, Set<Authorizable> managers,
                                          String roleOwnerGroup) {
        try {
            Authorizable authorizableOwnersGroup = userManager.getAuthorizable(roleOwnerGroup);
            if (authorizableOwnersGroup != null && authorizableOwnersGroup.isGroup()) {
                Group ownerGroup = (Group) authorizableOwnersGroup;
                Iterator<Authorizable> projectOwners = ownerGroup.getMembers();
                while (projectOwners.hasNext()) {
                    Authorizable owner = projectOwners.next();
                    if (!owner.isGroup() && owner.getProperty(PROFILE_EMAIL) != null) {
                        managers.add(owner);
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
    protected List<String> getProjectGroupsFromUser(Iterator<Group> groupIterator)
        throws RepositoryException {
        List<String> groups = new ArrayList<>();
        while (groupIterator.hasNext()) {
            Group group = groupIterator.next();
            if (StringUtils.contains(group.getPrincipal().getName(), "external-contributor")) {
                groups.add(group.getPrincipal().getName());
            }
        }
        return groups;
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
