package com.mediahub.core.jobs;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = JobConsumer.class, immediate = true,
    property = {Constants.SERVICE_DESCRIPTION + "=Job to send welcome email to user",
        JobConsumer.PROPERTY_TOPICS + "=" + "user/welcome/email"})
/**
 * This job will send welcome email to user.
 *
 */
public class UserWelcomeEmail implements JobConsumer {

  @Reference
  GenericEmailNotification genericEmailNotification;

  @Override
  public JobResult process(Job job) {
    if(StringUtils.isNotBlank(job.getProperty(BnpConstants.EMAIL, StringUtils.EMPTY))){
      String[] emailRecipients = { job.getProperty(BnpConstants.EMAIL).toString() };
      Map<String, String> emailParams = new HashMap<>();
      emailParams.put(BnpConstants.SUBJECT, "Mediahub - Welcome Email");
      emailParams.put("firstname", job.getProperty(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
      genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/welcome.html",emailRecipients, emailParams);
      return JobResult.OK;
    } else {
      return JobResult.OK;
    }
  }


}
