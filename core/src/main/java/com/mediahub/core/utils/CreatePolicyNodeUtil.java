package com.mediahub.core.utils;

import java.security.Principal;
import java.util.List;
import java.util.NoSuchElementException;

import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.security.AccessControlManager;
import javax.jcr.security.Privilege;

import org.apache.jackrabbit.api.security.JackrabbitAccessControlList;
import org.apache.jackrabbit.commons.jackrabbit.authorization.AccessControlUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CreatePolicyNodeUtil {
    private final static Logger logger = LoggerFactory.getLogger(CreatePolicyNodeUtil.class);

    private CreatePolicyNodeUtil() {
        // private Constructor
    }

    public static void creatrepPolicyeNodes(Session adminSession, String parentFolderPath,
            List<Principal> principalNameList) {
        try {

            AccessControlManager accessControlManager = adminSession.getAccessControlManager();
            JackrabbitAccessControlList acl = AccessControlUtils.getAccessControlList(adminSession, parentFolderPath);
            // create privilege
            for (Principal principalName : principalNameList) {
                Privilege[] privileges = new Privilege[] { accessControlManager.privilegeFromName(Privilege.JCR_READ) };

                acl.addEntry(principalName, privileges, true);// true for allow entry, false for deny
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
