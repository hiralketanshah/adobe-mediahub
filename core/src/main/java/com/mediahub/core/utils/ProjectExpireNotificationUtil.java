package com.mediahub.core.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.constants.BnpConstants;

public class ProjectExpireNotificationUtil {

    private static Logger logger = LoggerFactory.getLogger(ProjectExpireNotificationUtil.class);

    private ProjectExpireNotificationUtil() {
        // private Constructor
    }

    public static Map<String, String> getPredicateMap(String projectPath) {
        SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(BnpConstants.YYYY_MM_DD_T_HH_MM_SS_SSSZ);
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, 1);
        cal.add(Calendar.DAY_OF_MONTH, 1);
        Date onePlusMonth = cal.getTime();
        Calendar cal1 = Calendar.getInstance();
        cal1.add(Calendar.MONTH, -1);

        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, projectPath);
        map.put(BnpConstants.FIRST_PROPERTY, BnpConstants.SLING_RESOURCETYPE);
        map.put(BnpConstants.FIRST_PROPERTY_OPERATION, BnpConstants.LIKE);
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, BnpConstants.PROJECT_RESOURCE);
        map.put(BnpConstants.SECOND_DATERANGE_PROPERTY, BnpConstants.PROJECT_DUEDATE);
                                                                                                     // 2020-11-26T19:29:13.454+05:30
        map.put(BnpConstants.SECOND_DATERANGE_UPPEROPERATION, BnpConstants.LESSTHAN_EQUALS);
        map.put(BnpConstants.SECOND_DATERANGE_UPPERBOUND, DATE_FORMAT.format(onePlusMonth)); // example :
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
