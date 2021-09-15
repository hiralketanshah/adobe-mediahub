package com.mediahub.core.jobs;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.adobe.acs.commons.models.injectors.impl.I18nInjector;
import com.day.cq.i18n.I18n;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

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

  @SuppressWarnings("CQRules:AMSCORE-553")
  @Reference
  private SlingSettingsService slingSettingsService;

  @Reference
  I18nProvider provider;

  @Override
  public JobResult process(Job job) {

    if(StringUtils.isNotBlank(job.getProperty(BnpConstants.EMAIL, StringUtils.EMPTY))){
      String[] emailRecipients = { job.getProperty(BnpConstants.EMAIL).toString() };
      Locale locale = LocaleUtils.toLocale(job.getProperty(BnpConstants.LANGUAGE, "en"));
      Map<String, String> emailParams = new HashMap<>();
      emailParams.put(BnpConstants.SUBJECT, ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Welcome Email", locale) );
      emailParams.put("firstname", job.getProperty(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
      genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/welcome.html",emailRecipients, emailParams);
      return JobResult.OK;
    } else {
      return JobResult.OK;
    }
  }


}
