package com.mediahub.core.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.settings.SlingSettingsService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProjectExpireNotificationUtil {

    private static Logger logger = LoggerFactory.getLogger(ProjectExpireNotificationUtil.class);

    private ProjectExpireNotificationUtil() {
        // private Constructor
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

    public static String getRunmodeText(SlingSettingsService slingSettingsService){
        if(slingSettingsService.getRunModes().contains("stage")){
            return "Mediahub Stage";
        } else if(slingSettingsService.getRunModes().contains("dev")){
            return "Mediahub Dev";
        } else{
            return "Mediahub";
        }
    }

}
