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
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

public class CreatePolicyNodeUtil {
    private final static Logger logger = LoggerFactory.getLogger(CreatePolicyNodeUtil.class);

    private CreatePolicyNodeUtil() {
        // private Constructor
    }

    public static void createRepPolicyNodes(Session adminSession, String parentFolderPath, List<Principal> principalNameList) {
        for (Principal principalName : principalNameList) {
            createRepPolicyNode(adminSession, parentFolderPath, principalName, Privilege.JCR_READ);
        }
    }

    public static void createRepPolicyNodes(Session adminSession, String parentFolderPath, List<Principal> principalNameList, Map<String, Value> restrictions) {
        for (Principal principalName : principalNameList) {
            createRepPolicyNode(adminSession, parentFolderPath, principalName, Privilege.JCR_READ, restrictions);
        }
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, Principal principalName, String privilege) {
        createRepPolicyNode(adminSession, parentFolderPath, principalName, privilege, null);
    }

    public static void createRepPolicyNode(Session adminSession, String parentFolderPath, Principal principalName, String privilege, Map<String, Value> restrictions) {
        try {
            AccessControlManager accessControlManager = adminSession.getAccessControlManager();
            JackrabbitAccessControlList acl = AccessControlUtils.getAccessControlList(adminSession, parentFolderPath);
            // create privilege
            Privilege[] privileges = new Privilege[]{accessControlManager.privilegeFromName(privilege)};
            if (restrictions == null) {
                acl.addEntry(principalName, privileges, true);// true for allow entry, false for deny
            } else {
                acl.addEntry(principalName, privileges, true, restrictions);
            }
            accessControlManager.setPolicy(parentFolderPath, acl);
            adminSession.save();

        } catch (NoSuchElementException nse) {
            logger.error("An NoSuchElementException occured : {}", nse.getMessage());
        } catch (RepositoryException e) {
            logger.error("An RepositoryException occured : {}", e.getMessage());
        }

    }

}
