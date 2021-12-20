package com.mediahub.core.utils;

import org.apache.commons.lang3.StringUtils;
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

    public static final String S7_ACTIVATE_VALUE = "Activate";
    public static final String S7_DEACTIVATE_VALUE = "Deactivate";
    private static long JOB_TIMEOUT = 15 * 60 * (long) 1000; //Timeout is set to 15min

    public static boolean startS7ActivationJob(Resource asset, JobManager jobManager, String action) {
        try {
            final Map<String, Object> props = new HashMap<>();
            props.put("action", action);
            props.put("path", asset.getPath());
            props.put("user", "");
            Job job = jobManager.addJob("dam/scene7/asset/activation", props);
            final String jobId = job.getId();
            final long time = System.currentTimeMillis();
            if (!StringUtils.isEmpty(jobId)) {
                while ((job == null || job.getFinishedDate() == null) && System.currentTimeMillis() < time + JOB_TIMEOUT) {
                    Thread.sleep(1000);
                    job = jobManager.getJobById(jobId);
                }
                if (job.getFinishedDate() != null) {
                    if (job.getJobState() == Job.JobState.SUCCEEDED) {
                        return true;
                    } else {
                        logger.error("Error with DM activation job '{}'", job.getResultMessage());
                    }
                }
            }
            return false;
        } catch (Exception e) {
            logger.error("Exception when activating asset in DM", e);
            return false;
        }
    }

    public static void startScene7ActivationJobWithoutStatus(Resource asset, ResourceResolver resourceResolver, JobManager jobManager, String action) {
        final Map<String, Object> props = new HashMap<>();
        props.put("action", action);
        props.put("path", asset.getPath());
        props.put("user", "");
        Job job = jobManager.addJob("dam/scene7/asset/activation", props);
    }

}
