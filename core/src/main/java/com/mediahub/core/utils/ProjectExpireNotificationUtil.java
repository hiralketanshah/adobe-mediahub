package com.mediahub.core.utils;

import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Session;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.settings.SlingSettingsService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProjectExpireNotificationUtil {

    private static Logger logger = LoggerFactory.getLogger(ProjectExpireNotificationUtil.class);

    private ProjectExpireNotificationUtil() {
        // private Constructor
    }

    /**
     * @param dateFormat - Simple Date format Object
     * @return Current Date Object
     */
    public static Date getCurrentDate(SimpleDateFormat dateFormat) {
        Date crrentDate = null;
        Date date = Calendar.getInstance().getTime();
        String strDate = dateFormat.format(date);
        try {
            crrentDate = dateFormat.parse(strDate);
        } catch (ParseException e) {
            logger.error("Eception occured while Parsing the Date : {} ", e.getMessage());
        }
        return crrentDate;
    }

    /**
     * @param dateFormat  - Simple Date format Object
     * @return date string
     */
    public static String getCurrentDateString(SimpleDateFormat dateFormat) {
        Date crrentDate = null;
        Date date = Calendar.getInstance().getTime();
        String strDate = dateFormat.format(date);
        return strDate;
    }

    @SuppressWarnings("CQRules:AMSCORE-553")
    public static String getRunmodeText(SlingSettingsService slingSettingsService){
        if(slingSettingsService.getRunModes().contains("stage")){
            return "Mediahub Stage";
        } else if(slingSettingsService.getRunModes().contains("dev")){
            return "Mediahub Dev";
        } else{
            return "Mediahub";
        }
    }

    /**
     * Method to get date format from string value
     *
     * @param expiryDate - expiry date in string format
     * @return SimpleDateFormat object
     */
    public static SimpleDateFormat getSimpleDateFormat(String expiryDate) {
        SimpleDateFormat dateFormat;
        if (expiryDate.indexOf('/') == 2) {
            dateFormat = new SimpleDateFormat(BnpConstants.DD_MM_YYYY);
        } else {
            dateFormat = new SimpleDateFormat(BnpConstants.YYYY_MM_DD);
        }
        return dateFormat;
    }

    public static List<String> getRequiredMetadataFields(ResourceResolver resourceResolver, String schemaPath) {
        List<String> missedMetaData = new ArrayList<>();
        Map<String, String> map = QueryUtils.getPredicateMapRequiredSchemaFields(schemaPath);
        QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
        Query query = builder.createQuery(PredicateGroup.create(map), resourceResolver.adaptTo(
            Session.class));
        SearchResult result = query.getResult();
        Iterator<Resource> requiredFields = result.getResources();
        while (requiredFields.hasNext()) {
            Resource field = requiredFields.next();
            String metaField = StringUtils
                .replace(field.getValueMap().get("cq-msm-lockable", StringUtils.EMPTY), "./metadata/", "");
            if (StringUtils.isNotBlank(metaField)) {
                missedMetaData.add(metaField);
            }
        }
        return missedMetaData;
    }

}
