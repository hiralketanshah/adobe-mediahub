package com.mediahub.core.utils;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.JobManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class SlingJobUtils {

    private static Logger logger = LoggerFactory.getLogger(SlingJobUtils.class);

    public static String S7_ACTIVATE_VALUE = "Activate";
    public static String S7_DEACTIVATE_VALUE = "Deactivate";
    private static long JOB_TIMEOUT = 30 * 60 * 1000; //Timeout is set to 30min

    public static boolean startS7ActivationJob(Resource asset, ResourceResolver resourceResolver, JobManager jobManager, String action) {
        try {
            final Map<String, Object> props = new HashMap<>();
            props.put("action", action);
            props.put("path", asset.getPath());
            props.put("user", "");
            Job job = jobManager.addJob("dam/scene7/asset/activation", props);
            long time = System.currentTimeMillis();
            while (job.getFinishedDate() == null && System.currentTimeMillis() < time + JOB_TIMEOUT) {
                Thread.sleep(1000);
                job = jobManager.getJobById(job.getId());
            }
            if (job.getFinishedDate() != null) {
                if (job.getJobState() == Job.JobState.SUCCEEDED) {
                    Resource metadata = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                    String status = metadata.getValueMap().get(BnpConstants.S7_FILE_STATUS_PROPERTY, String.class);
                    if (BnpConstants.S7_FILE_STATUS_COMPLETE.equals(status) || BnpConstants.S7_FILE_STATUS_INCOMPLETE.equals(status)) {
                        while (System.currentTimeMillis() < time + JOB_TIMEOUT) {
                            if (S7_ACTIVATE_VALUE.equals(action)) {
                                if (BnpConstants.S7_FILE_STATUS_COMPLETE.equals(status)) {
                                    return true;
                                }
                            } else if (S7_DEACTIVATE_VALUE.equals(action)) {
                                if (BnpConstants.S7_FILE_STATUS_INCOMPLETE.equals(status)) {
                                    return true;
                                }
                            }
                            status = resourceResolver.getResource(metadata.getPath()).getValueMap().get(BnpConstants.S7_FILE_STATUS_PROPERTY, String.class);
                        }
                    }
                    return true;
                }
            }
            return false;
        } catch (Exception e) {
            logger.error("Error with S7 activation job", e);
            return false;
        }
    }

}
