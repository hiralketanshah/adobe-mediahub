package com.mediahub.core.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.jcr.Node;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.ResourceResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectManager;
import com.mediahub.core.constants.MediahubConstants;
import com.mediahub.core.services.GenericEmailNotification;

public class ProjectExpireNotificationUtil {

    private static Logger logger = LoggerFactory.getLogger(ProjectExpireNotificationUtil.class);

    private ProjectExpireNotificationUtil() {
        // private Constructor
    }

    public static Map<String, String> getPredicateMap(String projectPath) {
        SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(MediahubConstants.YYYY_MM_DD_T_HH_MM_SS_SSSZ);
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, 1);
        Date onePlusMonth = cal.getTime();

        Calendar cal1 = Calendar.getInstance();
        cal1.add(Calendar.MONTH, -1);
        Date oneMinusMonth = cal1.getTime();

        Map<String, String> map = new HashMap<>();
        map.put(MediahubConstants.PATH, projectPath);
        map.put(MediahubConstants.FIRST_PROPERTY, MediahubConstants.SLING_RESOURCETYPE);
        map.put(MediahubConstants.FIRST_PROPERTY_OPERATION, MediahubConstants.LIKE);
        map.put(MediahubConstants.FIRST_PROPERTY_VALUE, MediahubConstants.PROJECT_RESOURCE);
        map.put(MediahubConstants.SECOND_DATERANGE_PROPERTY, MediahubConstants.PROJECT_DUEDATE);
        map.put(MediahubConstants.SECOND_DATERANGE_LOWEROPERATION, MediahubConstants.GREATERTHAN_EQUALS);
        map.put(MediahubConstants.SECOND_DATERANGE_LOWERBOUND, DATE_FORMAT.format(oneMinusMonth)); // example:
                                                                                                   // 2020-11-26T19:29:13.454+05:30
        map.put(MediahubConstants.SECOND_DATERANGE_UPPEROPERATION, MediahubConstants.LESSTHAN_EQUALS);
        map.put(MediahubConstants.SECOND_DATERANGE_UPPERBOUND, DATE_FORMAT.format(onePlusMonth)); // example :
                                                                                                  // 2020-12-26T19:29:13.454+05:30

        return map;
    }

    public static Date getCurrentDate(SimpleDateFormat dateFormat) {
        Date crrentDate = null;
        Date date = Calendar.getInstance().getTime();
        String strDate = dateFormat.format(date);
        try {
            crrentDate = dateFormat.parse(strDate);
        } catch (ParseException e) {

            logger.debug("Eception occured while Parsing the Date : {} ", e.getMessage());
        }
        return crrentDate;
    }

    public static void sendProjectDeletedEmailNotification(Iterator<Authorizable> itr, UserManager userManager,
            ResourceResolver resolver, String projectpath, GenericEmailNotification genericEmailNotification) {
        try {
            List<String> listOfUserMailID = new ArrayList<>();
            ProjectManager projectManager = resolver.adaptTo(ProjectManager.class);
            Project project = resolver.getResource(projectpath).adaptTo(Project.class);
            String projectName = project.getTitle();
            logger.debug("Project : {}", projectName);

            while (itr.hasNext()) {
                Object obj = itr.next();
                if (obj instanceof User) {
                    User user = (User) obj;
                    String userID = user.getID();
                    logger.debug("userID : {} ", userID);
                    Authorizable userAuthorization = userManager.getAuthorizable(userID);
                    if (userAuthorization.hasProperty(MediahubConstants.PEOFILE_EMAIL)
                            && userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL) != null
                            && userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL).length > 0) {
                        String useremailID = userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL)[0]
                                .getString();
                        listOfUserMailID.add(useremailID);
                        logger.debug("EMailID----- : {} ", useremailID);

                    }
                }
            }

            String[] emailIDs = new String[listOfUserMailID.size()];
            emailIDs = listOfUserMailID.toArray(emailIDs);

            String[] emailRecipients = emailIDs;
            String subject = MediahubConstants.MEDIAHUB_EXPIRE_OROJECT;
            String bodyforUsers = "Hello User,\n\nThe " + projectName + " space has been deleted, \n\nMediahub Team";
            genericEmailNotification.sendEmail(emailRecipients, bodyforUsers, projectName, subject);
            logger.debug("Succesful Sent Mail");

            projectManager.deleteProject(project);

        } catch (Exception e) {
            logger.info("Error while deactivating user {}", e.getMessage());
        }

    }

    public static void sendProjectEmailNotification(Iterator<Authorizable> itr, int differenceInDays,
            ResourceResolver resolver, Node jcrContentNode, String projectpath, UserManager userManager,
            GenericEmailNotification genericEmailNotification) {
        try {
            Project project = resolver.getResource(projectpath).adaptTo(Project.class);
            String projectName = project.getTitle();
            logger.debug("Project : {}", projectName);

            while (itr.hasNext()) {
                Object obj = itr.next();
                if (obj instanceof User) {
                    User user = (User) obj;
                    String userID = user.getID();
                    logger.debug("userID : {} ", userID);
                    Authorizable userAuthorization = userManager.getAuthorizable(userID);
                    if (userAuthorization.hasProperty(MediahubConstants.PEOFILE_EMAIL)
                            && userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL) != null
                            && userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL).length > 0) {
                        String userName = userAuthorization.getProperty(MediahubConstants.PROFILE_GIVEN_NAME)[0]
                                .getString();
                        logger.debug("User GivenName : {} ", userName);

                        String userEmailID = userAuthorization.getProperty(MediahubConstants.PEOFILE_EMAIL)[0]
                                .getString();

                        logger.debug("EMailID----- : {} ", userEmailID);

                        if (differenceInDays <= 30 && differenceInDays > 0) {
                            String[] emailRecipients = { userEmailID };
                            String subject = MediahubConstants.MEDIAHUB_EXPIRE_OROJECT;
                            String bodyforUsers = "Hello " + userName + ",\n\nThe " + projectName
                                    + " project space will expire in \"1\" month.\n\nOn this date, access to all contributors will be deactivated and you will have 1 month to save the content of your space.\n\nMediahub Team";

                            genericEmailNotification.sendEmail(emailRecipients, bodyforUsers, projectName, subject);
                            logger.debug("Succesful Sent Mail ");
                        } else {

                            jcrContentNode.setProperty(MediahubConstants.ACTIVE, false);

                            String[] emailRecipients = { userEmailID };
                            String subject = MediahubConstants.MEDIAHUB_EXPIRE_OROJECT;
                            String bodyforUsers = "Hello " + userName + ",\n\nThe " + projectName
                                    + " space has been deactivated, you have \"1\" month left to recover your content. On this date, the space will be deleted.\n\nMedia published in the medialibrary are not affected.\n\nIf necessary, you can extend the use of your space in the parameters of your project space \"URL ACCESS MEDIAHUB TO EXTEND PROJECT SPACE\"\n\nMediahub Team";
                            genericEmailNotification.sendEmail(emailRecipients, bodyforUsers, projectName, subject);
                            logger.debug("Succesful Sent Mail");
                        }

                    }
                }
            }

        } catch (Exception e) {
            logger.info("Error while deactivating user {}", e.getMessage());
        }
    }

}
