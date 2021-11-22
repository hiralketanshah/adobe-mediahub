package com.mediahub.core.utils;

import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ValueMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import java.util.HashMap;
import java.util.Map;

import static com.mediahub.core.constants.BnpConstants.*;

public class ProjectPermissionsUtil {

    private static final Logger logger = LoggerFactory.getLogger(ProjectPermissionsUtil.class);

    private static Map<String, String> mapping = new HashMap<>();

    static {
        mapping.put(PROJECT_READER_GROUP, ROLE_OBSERVER);
        mapping.put(PROJECT_EXTERNAL_CONTRIBUTOR_GROUP, ROLE_EXTERNALCONTRIBUTEUR);
        mapping.put(PROJECT_INTERNAL_CONTRIBUTOR_GROUP, ROLE_EDITOR);
        mapping.put(PROJECT_PUBLISHER_GROUP, ROLE_PROJECTPUBLISHER);
        mapping.put(PROJECT_MANAGER_GROUP, ROLE_OWNER);
    }

    public static boolean isAuthorizedForProject(ResourceResolver resourceResolver, String projectDamPath, String[] groups, String userId) {
        try {
            if (groups != null && groups.length > 0) {
                if (!StringUtils.isEmpty(projectDamPath) && projectDamPath.startsWith("/content/dam/projects")) {
                    Resource projectDam = resourceResolver.getResource(projectDamPath);
                    boolean isProject = false;
                    while (!projectDam.getPath().equals("/content/dam/projects") && !isProject) {
                        if (projectDam.getValueMap().containsKey("projectPath")) {
                            isProject = true;
                        } else {
                            projectDam = projectDam.getParent();
                        }
                    }
                    String projectPath = projectDam.getValueMap().get("projectPath", new String[]{""})[0];
                    Resource project = resourceResolver.getResource(projectPath);
                    ValueMap map = project.getValueMap();
                    for (String group : groups) {
                        String projectRole = mapping.get(group);
                        if (map.containsKey(projectRole)) {
                            String projectGroup = map.get(projectRole, String.class);
                            UserManager userManager = resourceResolver.adaptTo(UserManager.class);
                            if (userManager.getAuthorizable(projectGroup) != null && userManager.getAuthorizable(projectGroup).isGroup() && ((Group) userManager.getAuthorizable(projectGroup)).isMember(userManager.getAuthorizable(userId))) {
                                return true;
                            }
                        }

                    }
                }
            }
        } catch (RepositoryException e) {
            logger.error("Unable to get user groups for project");
        }
        return false;
    }
}
