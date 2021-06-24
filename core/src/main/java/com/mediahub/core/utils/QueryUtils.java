package com.mediahub.core.utils;

import com.mediahub.core.constants.BnpConstants;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class QueryUtils {

  private static Logger logger = LoggerFactory.getLogger(QueryUtils.class);

  private QueryUtils() {
    // private Constructor
  }

  public static Map<String, String> getPredicateMapProjectDueDate(String projectPath) {
      SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(BnpConstants.YYYY_MM_DD_T_HH_MM_SS_SSSZ);

      Calendar cal = Calendar.getInstance();
      cal.add(Calendar.MONTH, 1);
      cal.add(Calendar.DAY_OF_MONTH, 1);

      Map<String, String> map = new HashMap<>();
      map.put(BnpConstants.PATH, projectPath);
      map.put(BnpConstants.FIRST_PROPERTY, BnpConstants.SLING_RESOURCETYPE);
      map.put(BnpConstants.FIRST_PROPERTY_OPERATION, BnpConstants.LIKE);
      map.put(BnpConstants.FIRST_PROPERTY_VALUE, BnpConstants.PROJECT_RESOURCE);
      map.put(BnpConstants.SECOND_DATERANGE_PROPERTY, BnpConstants.PROJECT_DUEDATE);  // 2020-11-26T19:29:13.454+05:30
      map.put(BnpConstants.SECOND_DATERANGE_UPPEROPERATION, BnpConstants.LESSTHAN_EQUALS);
      map.put(BnpConstants.SECOND_DATERANGE_UPPERBOUND, DATE_FORMAT.format(cal.getTime())); // example : 2020-12-26T19:29:13.454+05:30
      map.put(BnpConstants.P_LIMIT, "-1");

      return map;
  }

}
