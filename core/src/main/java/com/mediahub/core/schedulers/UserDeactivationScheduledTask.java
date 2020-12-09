package com.mediahub.core.schedulers;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.jcr.Session;

import org.apache.commons.lang3.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

/**
 * Cron Job for Deactivating external user with past expiry date.
 * Currently runs every day at mid night
 */
@Designate(ocd= UserDeactivationScheduledTask.Config.class)
@Component(service=Runnable.class)
public class UserDeactivationScheduledTask implements Runnable {

    @Reference
    ResourceResolverFactory resolverFactory;
    @Reference
	GenericEmailNotification genericEmailNotification;

    @ObjectClassDefinition(name="User Deactivation Task",
                           description = "User Deactivation cron-job past expiray date")
    public static @interface Config {

        @AttributeDefinition(name = "Cron-job expression")
        String scheduler_expression() default "0 1 0 1/1 * ? *";

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
        ResourceResolver resolver = null;
        try {
            resolver = resolverFactory.getServiceResourceResolver(authInfo);
            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = getPredicateMap();
            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            final UserManager userManager= resolver.adaptTo(UserManager.class);
            Iterator<Resource> userResources = result.getResources();
            while(userResources.hasNext()){
                Resource user = userResources.next();
                String expiryDate = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EXPIRY,String.class);
       
                if(StringUtils.isNotBlank(expiryDate)){
                    deactivateExpiredUsers(userManager, user, expiryDate);
                          
                }
            }
            if(resolver.hasChanges()){
                resolver.commit();
            }
            resolver.close();
        } catch (Exception e) {
            logger.info("Error while deactivating user {}", e.getMessage());
        }

    }

    /**
     * deactivate user having expiry date post current date
     *
     * @param userManager
     * @param user
     * @param expiryDate
     * @throws ParseException
     * @throws javax.jcr.RepositoryException
     */
    protected void deactivateExpiredUsers(UserManager userManager, Resource user, String expiryDate)
        throws ParseException, javax.jcr.RepositoryException {
        SimpleDateFormat sdf = new SimpleDateFormat(BnpConstants.YYYY_MM_DD);
        Date date = sdf.parse(expiryDate);
        Calendar expiry = Calendar.getInstance();
        expiry.setTime(date);
        Authorizable authorizable = userManager.getAuthorizableByPath(user.getPath());
        if(authorizable instanceof User && !((User) authorizable).isDisabled() && Calendar.getInstance().after(expiry)){
            logger.info(user.getPath());
            ((User) authorizable).disable(BnpConstants.USER_HAS_EXPIRED);
            String email = user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.EMAIL,String.class);
            String[] emailRecipients = { email };
	        String subject = "Mediahub - User Deactivated";
	        Map<String, String> emailParams = new HashMap<String, String>();
	        emailParams.put(BnpConstants.SUBJECT, subject);
	        emailParams.put("firstname",user.getChild(BnpConstants.PROFILE).getValueMap().get(BnpConstants.FIRST_NAME,String.class));
	        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/userdeactivationmailtemplate.html",emailRecipients, emailParams);
        }
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
        return map;
    }

    @Activate
    protected void activate(final Config config) {
        userType = config.getUserType();
    }

}
