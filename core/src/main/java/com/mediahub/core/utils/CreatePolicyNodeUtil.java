package com.mediahub.core.utils;

import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.Privilege;
import java.security.Principal;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class CreatePolicyNodeUtil {
    private final static Logger logger = LoggerFactory.getLogger(CreatePolicyNodeUtil.class);

    public static void createRepPolicyNodes(Session adminSession, String parentFolderPath, List<Principal> principalNameList) {
        for (Principal principalName : principalNameList) {
            createRepPolicyNode(adminSession, parentFolderPath, true, principalName, Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
        }
    }

    public static void createRepPolicyNodes(Session adminSession, String parentFolderPath, List<Principal> principalNameList, Map<String, Value> restrictions) {
        for (Principal principalName : principalNameList) {
            createRepPolicyNode(adminSession, parentFolderPath, principalName, true, restrictions, Privilege.JCR_READ, Privilege.JCR_READ_ACCESS_CONTROL);
        }
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, boolean allow, Principal principalName, String... privilegeNames) {
        createRepPolicyNode(adminSession, parentFolderPath, principalName, allow, null, privilegeNames);
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, Principal principalName, String... privilegeNames) {
        createRepPolicyNode(adminSession, parentFolderPath, principalName, true, null, privilegeNames);
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, Principal principalName, Map<String, Value> restrictions, String... privilegeNames) {
        createRepPolicyNode(adminSession, parentFolderPath, principalName, true, restrictions, privilegeNames);
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, Principal principalName, boolean allow, Map<String, Value> restrictions, String... privilegeNames) {
        try {
            AccessControlManager accessControlManager = adminSession.getAccessControlManager();
            JackrabbitAccessControlList acl = AccessControlUtils.getAccessControlList(adminSession, parentFolderPath);
            // create privilege
            List<Privilege> privilegesList = Arrays.asList(privilegeNames).stream().map(p -> {
                try {
                    return accessControlManager.privilegeFromName(p);
                } catch (RepositoryException e) {
                    logger.error("Error when getting privileges", e);
                }
                return null;
            }).filter(p -> p != null).collect(Collectors.toList());
            Privilege[] privileges = privilegesList.toArray(new Privilege[privilegesList.size()]);
            if (restrictions == null) {
                acl.addEntry(principalName, privileges, allow);// true for allow entry, false for deny
            } else {
                acl.addEntry(principalName, privileges, allow, restrictions);
            }
            accessControlManager.setPolicy(parentFolderPath, acl);
            adminSession.save();

        } catch (Exception e) {
            logger.error("An error occurred", e);
        }

    }

}
