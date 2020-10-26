package com.mediahub.core.schedulers;

import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.MediahubConstants;
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

/**
 * Cron Job for Deactivating external user with past expiry date.
 * Currently runs every day at mid night
 */
@Designate(ocd= UserDeactivationScheduledTask.Config.class)
@Component(service=Runnable.class)
public class UserDeactivationScheduledTask implements Runnable {

    @Reference
    private ResourceResolverFactory resolverFactory;

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
        String getUserType() default MediahubConstants.EXTERNAL;
    }

    private final Logger logger = LoggerFactory.getLogger(UserDeactivationScheduledTask.class);

    private String userType;
    
    @Override
    public void run() {
        logger.debug("Cron Job to deactiavte user of type ='{}'", userType);
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, "writeService");
        ResourceResolver resolver = null;
        try {
            resolver = resolverFactory.getServiceResourceResolver(authInfo);
            QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = new HashMap<>();
            map.put(MediahubConstants.PATH, MediahubConstants.HOME_USERS);
            map.put(MediahubConstants.TYPE, MediahubConstants.REP_USERS);
            map.put(MediahubConstants.FIRST_PROPERTY, MediahubConstants.PROFILE_TYPE);
            map.put(MediahubConstants.FIRST_PROPERTY_VALUE, userType);

            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));
            SearchResult result = query.getResult();

            final UserManager userManager= resolver.adaptTo(UserManager.class);

            Iterator<Resource> userResources = result.getResources();
            while(userResources.hasNext()){
                Resource user = userResources.next();
                String expiryDate = user.getChild(MediahubConstants.PROFILE).getValueMap().get(MediahubConstants.EXPIRY,String.class);

                if(StringUtils.isNotBlank(expiryDate)){

                    SimpleDateFormat sdf = new SimpleDateFormat(MediahubConstants.YYYY_MM_DD);
                    Date date = sdf.parse(expiryDate);

                    Calendar expiry = Calendar.getInstance();
                    expiry.setTime(date);

                    Authorizable authorizable = userManager.getAuthorizableByPath(user.getPath());
                    if(authorizable instanceof  User && !((User) authorizable).isDisabled() && Calendar.getInstance().after(expiry)){
                        logger.info(user.getPath());
                        ((User) authorizable).disable(MediahubConstants.USER_HAS_EXPIRED);
                    }
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

    @Activate
    protected void activate(final Config config) {
        userType = config.getUserType();
    }

}
