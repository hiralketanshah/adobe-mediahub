package com.mediahub.core.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.MediahubConstants;

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

        Map<String, String> map = new HashMap<>();
        map.put(MediahubConstants.PATH, projectPath);
        map.put(MediahubConstants.FIRST_PROPERTY, MediahubConstants.SLING_RESOURCETYPE);
        map.put(MediahubConstants.FIRST_PROPERTY_OPERATION, MediahubConstants.LIKE);
        map.put(MediahubConstants.FIRST_PROPERTY_VALUE, MediahubConstants.PROJECT_RESOURCE);
        map.put(MediahubConstants.SECOND_DATERANGE_PROPERTY, MediahubConstants.PROJECT_DUEDATE);
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
