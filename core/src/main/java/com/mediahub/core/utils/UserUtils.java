package com.mediahub.core.utils;

import com.google.common.collect.ImmutableMap;
import com.mediahub.core.constants.BnpConstants;
import java.util.Iterator;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import javax.jcr.security.Privilege;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.StringJoiner;

public class UserUtils {

    private static Logger logger = LoggerFactory.getLogger(UserUtils.class);

    private UserUtils() {
        //private constructor to restrict static class
    }

    public static String getUserLanguage(Resource user) {
        if (null != user.getChild(BnpConstants.PREFERENCES)) {
            Resource preferences = user.getChild(BnpConstants.PREFERENCES);
            return preferences.getValueMap().get(BnpConstants.LANGUAGE, "en");
        } else {
            return "en";
        }
    }

    public static String getUserLanguage(Authorizable userAuthorization) throws RepositoryException {
        return userAuthorization.getProperty(BnpConstants.PREFERENCES_LANGUAGE_PROPERTY) != null ? userAuthorization.getProperty(BnpConstants.PREFERENCES_LANGUAGE_PROPERTY)[0].getString() : "en";
    }

    @SuppressWarnings("squid:S2070")
    public static String encryptThisString(String input) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance("SHA-1");
        byte[] messageDigest = md.digest(input.getBytes());
        BigInteger no = new BigInteger(1, messageDigest);
        input = no.toString(16);
        while (input.length() < 32) {
            input = "0" + input;
        }
        return input;
    }

    public static String getProjectOwnerName(String initiator, UserManager userManager) throws RepositoryException {
        User projectOwnerUser = (User) userManager.getAuthorizable(initiator);
        Value[] givenName = projectOwnerUser.getProperty("./profile/givenName");
        Value[] familyName = projectOwnerUser.getProperty("./profile/familyName");
        String fullName;
        if (null != givenName && null != familyName) {
            String[] fullNameString = {givenName[0].getString(), familyName[0].getString()};
            fullName = String.join(" ", fullNameString);
        } else {
            fullName = null == givenName ? (null == familyName ? initiator : familyName[0].getString()) : givenName[0].getString();
        }
        return fullName;
    }


    /**
     * MED-493 Create user group while creating folders
     *
     * @param resolver
     * @param uuid
     * @param resource
     */
    public static void createFolderGroups(ResourceResolver resolver, String uuid, Resource resource) {
        final UserManager userManager = resolver.adaptTo(UserManager.class);
        try {

            Group folderContributor = userManager.createGroup(uuid + "-contributor");
            Group folderEntityManager = userManager.createGroup(uuid + "-entity-manager");
            Session session = resolver.adaptTo(Session.class);
            if (userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR) != null) {
                ((Group) userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_CONTRIBUTOR)).addMember(folderContributor);
                CreatePolicyNodeUtil.createRepPolicyNode(session, resource.getPath(), Boolean.TRUE, folderContributor.getPrincipal(), Privilege.JCR_ALL);
            }
            if (userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER) != null) {
                ((Group) userManager.getAuthorizable(BnpConstants.MEDIAHUB_BASIC_ENTITY_MANAGER)).addMember(folderEntityManager);
                CreatePolicyNodeUtil.createRepPolicyNode(session, resource.getPath(), Boolean.TRUE, folderEntityManager.getPrincipal(), Privilege.JCR_ALL);
            }
        } catch (RepositoryException e) {
            logger.error("Exception while creating groups for folders", e);
        }
    }

    public static String deriveProjectGroupNameFromPath(String path) {
        String[] folderSegments = path.substring(1).split("/");
        StringJoiner joiner = new StringJoiner("-");
        if (folderSegments.length > 2) {
            for (int i = 2; i < folderSegments.length && i <= 6; i++) {
                joiner.add(folderSegments[i]);
            }
        }
        return joiner.toString() + "-entity-project";
    }

    public static void createProjectFolderGroups(ResourceResolver resolver, Resource resource) {
        final UserManager userManager = resolver.adaptTo(UserManager.class);
        try {
            Group folderContributor = userManager.createGroup(deriveProjectGroupNameFromPath(resource.getPath()));
            Session session = resolver.adaptTo(Session.class);
            if (userManager.getAuthorizable(BnpConstants.MEDIAHUB_PROJECT_ADMINISTRATOR) != null) {
                ((Group) userManager.getAuthorizable(BnpConstants.MEDIAHUB_PROJECT_ADMINISTRATOR)).addMember(folderContributor);
                CreatePolicyNodeUtil.createRepPolicyNode(session, resource.getPath(), Boolean.TRUE, folderContributor.getPrincipal(), Privilege.JCR_ALL);
                ValueFactory vf = session.getValueFactory();
                String damFolderPath = resource.getParent().getPath().replace(BnpConstants.AEM_PROJECTS_PATH, BnpConstants.MEDIALIBRARY_PROJECTS_PATH);
                Resource damFolderResource = resolver.getResource(damFolderPath);
                CreatePolicyNodeUtil.createRepPolicyNode(session, damFolderResource.getPath(), Boolean.TRUE, folderContributor.getPrincipal(), Privilege.JCR_ALL);
                while (damFolderResource.getParent() != null && !StringUtils.equals(damFolderResource.getParent().getPath(), BnpConstants.MEDIALIBRARY_PROJECTS_PATH)) {
                    damFolderResource = damFolderResource.getParent();
                    CreatePolicyNodeUtil.createRepPolicyNode(session, damFolderResource.getPath(), folderContributor.getPrincipal(), ImmutableMap.of("rep:glob", vf.createValue("")), Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
                }
            }
        } catch (RepositoryException e) {
            logger.error("Exception while creating groups for folders", e);
        }
    }

    public static String getUserType(Resource user) {
        if (user != null && user.getChild(BnpConstants.PROFILE) != null) {
            Resource preferences = user.getChild(BnpConstants.PROFILE);
            return preferences.getValueMap().get(BnpConstants.TYPE, "");
        } else {
            return "";
        }
    }

    public static boolean isTechnicalAdmin(Authorizable auth) throws RepositoryException {
        boolean isTechnicalAdmin = Boolean.FALSE;
        Iterator<Group> projectGroups = auth.memberOf();
        while (projectGroups.hasNext()) {
            Group group = projectGroups.next();
            if (StringUtils.equals(auth.getID(), "admin") || StringUtils.equals(group.getID(), "administrators") || StringUtils.equals(group.getID(), "mediahub-administrators")) {
                isTechnicalAdmin = Boolean.TRUE;
            }

        }
        return isTechnicalAdmin;
    }





}
