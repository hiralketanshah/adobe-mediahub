package com.mediahub.core.utils;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.principal.PrincipalManager;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ValueMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import java.security.Principal;
import java.util.HashMap;
import java.util.Iterator;
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
            if (groups != null && groups.length > 0 && !StringUtils.isEmpty(projectDamPath)) {
                String projectPath = null;
                if (projectDamPath.startsWith("/content/dam/projects")) {
                    Resource projectDam = resourceResolver.getResource(projectDamPath);
                    boolean isProject = false;
                    while (!projectDam.getPath().equals("/content/dam/projects") && !isProject) {
                        if (projectDam.getValueMap().containsKey("projectPath")) {
                            isProject = true;
                        } else {
                            projectDam = projectDam.getParent();
                        }
                    }
                    projectPath = projectDam.getValueMap().get("projectPath", new String[]{""})[0];
                } else if (projectDamPath.startsWith("/content/projects")) {
                    projectPath = projectDamPath;
                }
                if (!StringUtils.isEmpty(projectPath)) {
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

    public static void assignMembersToMedialibrary(ResourceResolver resourceResolver, String projectPath) {
        try {
            Session session = resourceResolver.adaptTo(Session.class);
            JackrabbitSession js = (JackrabbitSession) session;
            PrincipalManager principalMgr = js.getPrincipalManager();

            Resource projectResource = resourceResolver.getResource(projectPath);
            ValueMap projectValueMap = projectResource.getValueMap();
            Principal groupOwnerPrincipal = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_OWNER, String.class));
            Principal groupOwnerProjectPublisher = principalMgr.getPrincipal(projectValueMap.get(BnpConstants.ROLE_PROJECTPUBLISHER, String.class));

            UserManager userManager = resourceResolver.adaptTo(UserManager.class);
            String damPath = projectResource.getChild(JcrConstants.JCR_CONTENT).getValueMap().get("project.path", String.class);
            String[] damPathFolders = damPath.substring(1).split("/");
            StringBuilder pathBuilder = new StringBuilder(MEDIALIBRARY_PATH);
            if (damPathFolders.length > 3) {
                for (int i = 3; i < damPathFolders.length && i <= 7; i++) {
                    pathBuilder.append("/").append(damPathFolders[i]);
                }
            }
            Resource damPathSelectedFolderMetadata = resourceResolver.getResource(pathBuilder.toString()).getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
            if (damPathSelectedFolderMetadata != null) {
                String damPathSelectedFolderUUID = damPathSelectedFolderMetadata.getValueMap().get("uuid", String.class);
                if (!StringUtils.isEmpty(damPathSelectedFolderUUID)) {
                    Group uuidGroup = (Group) userManager.getAuthorizable(damPathSelectedFolderUUID + "-contributor");
                    if (uuidGroup != null) {
                        //Add managers to contributor
                        Iterator<Authorizable> managerMembers = ((Group) userManager.getAuthorizable(groupOwnerPrincipal)).getDeclaredMembers();
                        while (managerMembers.hasNext()) {
                            Authorizable uuidGroupMember = managerMembers.next();
                            uuidGroup.addMember(uuidGroupMember);
                        }
                        //Add publishers to contributor
                        Iterator<Authorizable> publishersMembers = ((Group) userManager.getAuthorizable(groupOwnerProjectPublisher)).getDeclaredMembers();
                        while (publishersMembers.hasNext()) {
                            Authorizable uuidGroupMember = publishersMembers.next();
                            uuidGroup.addMember(uuidGroupMember);
                        }
                        if (!userManager.isAutoSave()) {
                            resourceResolver.commit();
                        }
                    }
                }
            }
        } catch (Exception e) {
            logger.error("Unable to assign project members to the medialibrary");
        }
    }


    public static Map<String, String> getPredicateMapProjectSearch(String role, String group) {
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, BnpConstants.AEM_PROJECTS_PATH);
        map.put(BnpConstants.TYPE, org.apache.jackrabbit.JcrConstants.NT_UNSTRUCTURED);
        map.put(BnpConstants.FIRST_PROPERTY, role);
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, group);
        map.put("p.limit", "-1");
        return map;
    }

}
