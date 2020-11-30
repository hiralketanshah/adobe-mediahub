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
        cal.add(Calendar.DAY_OF_MONTH, 1);
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
       // map.put(MediahubConstants.SECOND_DATERANGE_LOWEROPERATION, MediahubConstants.GREATERTHAN_EQUALS);
      //  map.put(MediahubConstants.SECOND_DATERANGE_LOWERBOUND, DATE_FORMAT.format(oneMinusMonth)); // example:
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

}
