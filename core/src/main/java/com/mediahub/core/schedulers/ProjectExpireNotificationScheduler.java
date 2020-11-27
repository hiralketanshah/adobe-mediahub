package com.mediahub.core.schedulers;

import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

import javax.jcr.Node;
import javax.jcr.Session;

import org.apache.jackrabbit.api.security.user.Authorizable;
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
import com.day.cq.search.result.Hit;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.MediahubConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;

/**
 * 
 *         Scheduler class to send Project Expire Email Notification to project group users.
 *         
 *         Scheduler runs every day 2am.
 *
 */
@Designate(ocd = ProjectExpireNotificationScheduler.Config.class)
@Component(service = Runnable.class)
public class ProjectExpireNotificationScheduler implements Runnable {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @ObjectClassDefinition(
        name = "MediaHub Project Expire Notification Scheduler",
        description = "MediaHub Project Expiry Notification Scheduler")
    public static @interface Config {

        @AttributeDefinition(name = "Cron-job expression")
        String scheduler_expression() default "0 0 2 1/1 * ? *";

        @AttributeDefinition(
            name = "Concurrent task",
            description = "Whether or not to schedule this task concurrently")
        boolean scheduler_concurrent() default true;

        @AttributeDefinition(name = "Project Path", description = "A Path from where Project are Fetched")
        String getProjectPath() default MediahubConstants.AEM_PROJECTS_PATH;
    }

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    GenericEmailNotification genericEmailNotification;

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
            Map<String, String> map = ProjectExpireNotificationUtil.getPredicateMap(projectPath);
            Query query = builder.createQuery(PredicateGroup.create(map), resolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            String projectOwnerProperty = null;
            for (Hit hit : result.getHits()) {
                String path = hit.getPath();
                logger.debug("Projecs Paths : {}", path);

                int index = path.lastIndexOf("/");
                String projectpath = path.substring(0, index);
                logger.debug("Projecs Paths : {}", projectpath);
                
                Resource adminResource = resolver.getResource(path);
                Node jcrContentNode = adminResource.adaptTo(Node.class);
                String projectactualDueDate = jcrContentNode.getProperty(MediahubConstants.PROJECT_DUEDATE).getValue()
                        .getString();
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
                Date actualDueDate = dateFormat.parse(projectactualDueDate);
                Date crrentDate = ProjectExpireNotificationUtil.getCurrentDate(dateFormat);
                int differenceInDays = (int) ((actualDueDate.getTime() - crrentDate.getTime()) / (1000 * 60 * 60 * 24));
                Resource groupOwnerResource = resolver.getResource(projectpath);
                Node projectNode = groupOwnerResource.adaptTo(Node.class);
                projectOwnerProperty = projectNode.getProperty(MediahubConstants.ROLE_OWNER).getValue().getString();

                UserManager userManager = resolver.adaptTo(UserManager.class);
                Authorizable authorizable;
                authorizable = userManager.getAuthorizable(projectOwnerProperty);
                org.apache.jackrabbit.api.security.user.Group group = (org.apache.jackrabbit.api.security.user.Group) authorizable;
                Iterator<Authorizable> itr = group.getDeclaredMembers();

                if (differenceInDays < 0 && differenceInDays >= -29) {
                    ProjectExpireNotificationUtil.sendProjectDeletedEmailNotification(itr, 
                            userManager, resolver, projectpath, genericEmailNotification);
                } else {

                    ProjectExpireNotificationUtil.sendProjectEmailNotification(itr, differenceInDays, resolver,
                            jcrContentNode, projectpath, userManager, genericEmailNotification);
                }
            }
            if (resolver.hasChanges()) {
                resolver.commit();
            }
            resolver.close();
        } catch (Exception e) {
            logger.info("Error while deactivating user {}", e.getMessage());
        }
    }

    @Activate
    protected void activate(final Config config) {
        projectPath = config.getProjectPath();
    }

}
