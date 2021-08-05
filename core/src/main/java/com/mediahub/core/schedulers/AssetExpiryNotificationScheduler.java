package com.mediahub.core.schedulers;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.DamConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
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
@Designate(ocd = AssetExpiryNotificationScheduler.Config.class)
@Component(service = Runnable.class)
public class AssetExpiryNotificationScheduler implements Runnable {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference
    private GenericEmailNotification genericEmailNotification;

    @Reference
    private SlingSettingsService slingSettingsService;

    @ObjectClassDefinition(
            name = "MediaHub Asset Expiry Notification Scheduler",
            description = "MediaHub Asset Expiry Notification Scheduler")
    public static @interface Config {

        @AttributeDefinition(name = "Cron-job expression")
        String scheduler_expression() default "0 0 6 1/1 * ? *";

        @AttributeDefinition(
                name = "Concurrent task",
                description = "Whether or not to schedule this task concurrently")
        boolean scheduler_concurrent() default true;

        @AttributeDefinition(name = "Project Path", description = "A Path from where Project are Fetched")
        String getDamPath() default BnpConstants.DAM_PATH;
    }

    private String damPath;

    @Override
    public void run() {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");
        final ResourceResolver resolver;
        try {
            resolver = resolverFactory.getServiceResourceResolver(authInfo);
            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = getPredicateMap(damPath, LocalDateTime.now().toString());
            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            Iterator<Resource> assets = result.getResources();
            final UserManager userManager = resolver.adaptTo(UserManager.class);

            assets.forEachRemaining(expiredAsset -> {
                notifyAssetAuthor(resolver, userManager, expiredAsset);
                logger.info("Expired Asset {}", expiredAsset.getPath());
            });

            if(resolver.hasChanges()){
                resolver.commit();
            }

        } catch (LoginException e) {
            logger.error("Error while fetching resource resolver {0}", e);
        } catch (PersistenceException e) {
            logger.error("Error while persisting changes {0}", e);
        }
    }

    /**
     * Method to notify the author who created asset
     *
     * @param resolver - resource resolver object
     * @param userManager - user manager to fetch user who created asset
     * @param expiredAsset - expired asset resource
     */
    private void notifyAssetAuthor(ResourceResolver resolver, UserManager userManager,
        Resource expiredAsset) {
        ValueMap assetProperties = expiredAsset.getValueMap();
        String createdBy = assetProperties.get(JcrConstants.JCR_CREATED_BY, StringUtils.EMPTY);

        Resource contentResource = expiredAsset.getChild(JcrConstants.JCR_CONTENT);

        if(contentResource != null){
            ModifiableValueMap assetContent = contentResource.adaptTo(ModifiableValueMap.class);
            if(StringUtils.isNotBlank(createdBy) && Boolean.FALSE.equals(assetContent.get("notified", Boolean.FALSE)) ){
                try {
                    Authorizable user  = userManager.getAuthorizable(createdBy);
                    if(user != null && user.getProperty(BnpConstants.USER_PROFILE_EMAIL) != null && user.getProperty(BnpConstants.USER_PROFILE_EMAIL).length > 0){
                        String email = user.getProperty(BnpConstants.USER_PROFILE_EMAIL)[0].getString();
                        String[] emailRecipients = {email};
                        Resource userResource = resolver.getResource(user.getPath());
                        sendWarningMail(userResource, emailRecipients, "/etc/mediahub/mailtemplates/assetexpirationtemplate.html", expiredAsset);
                        assetContent.put("notified", Boolean.TRUE);
                    }
                } catch (RepositoryException e) {
                    logger.error("Error while accessing repository {0}", e);
                }
            }
        }

    }

    @Activate
    protected void activate(final Config config) {
        this.damPath = config.getDamPath();
    }

    /**
     * @return predicate map for query to be framed
     */
    protected Map<String, String> getPredicateMap(String path, String upperBoundDate) {
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, path);
        map.put(BnpConstants.TYPE, DamConstants.NT_DAM_ASSET);
        map.put("daterange.property", "./jcr:content/metadata/bnpp-usage-end-date");
        map.put("daterange.upperBound", upperBoundDate);
        map.put(BnpConstants.P_LIMIT, "-1");
        return map;
    }

    /**
     * Method to send user deactivation within 30 days notice
     *
     * @param user - AEM user
     * @param emailRecipients - Email recipient to which mail to be sent
     * @param templatePath - template path to create email
     * @param expiredAsset - Expired asset resource
     */
    protected void sendWarningMail(Resource user, String[] emailRecipients, String templatePath, Resource expiredAsset) {
        String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - Image is past expiry and will be deactivated";
        Map<String, String> emailParams = new HashMap<>();
        emailParams.put(BnpConstants.SUBJECT, subject);
        emailParams.put("assetPath", expiredAsset.getPath());
        Resource profile = user.getChild(BnpConstants.PROFILE);
        if(profile != null){
            emailParams.put(BnpConstants.FIRSTNAME,profile.getValueMap().get(BnpConstants.FIRST_NAME,
                org.apache.commons.lang3.StringUtils.EMPTY));
        } else {
            emailParams.put(BnpConstants.FIRSTNAME, org.apache.commons.lang3.StringUtils.EMPTY);
        }
        genericEmailNotification.sendEmail(templatePath, emailRecipients, emailParams);
    }

}
