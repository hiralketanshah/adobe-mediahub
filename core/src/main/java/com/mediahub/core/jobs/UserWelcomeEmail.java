package com.mediahub.core.jobs;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.adobe.acs.commons.models.injectors.impl.I18nInjector;
import com.day.cq.commons.Externalizer;
import com.day.cq.i18n.I18n;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
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

  private static final Logger LOGGER = LoggerFactory.getLogger(UserWelcomeEmail.class);

  @Reference
  GenericEmailNotification genericEmailNotification;

  @SuppressWarnings("CQRules:AMSCORE-553")
  @Reference
  private SlingSettingsService slingSettingsService;

  @Reference
  I18nProvider provider;

  @Reference
  ResourceResolverFactory resolverFactory;

  @Reference
  Externalizer externalizer;

  @Override
  public JobResult process(Job job) {

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);

    try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      if(StringUtils.isNotBlank(job.getProperty(BnpConstants.EMAIL, StringUtils.EMPTY))){
        String[] emailRecipients = { job.getProperty(BnpConstants.EMAIL).toString() };
        Map<String, String> emailParams = new HashMap<>();
        emailParams.put(BnpConstants.SUBJECT, ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate("Welcome to MediaHub // Bienvenue sur MediaHub", Locale.ENGLISH) );
        emailParams.put("firstname", job.getProperty(BnpConstants.FIRST_NAME, StringUtils.EMPTY));
        String userToken = job.getProperty(BnpConstants.USER_TOKEN, StringUtils.EMPTY);
        emailParams.put(BnpConstants.LINK, externalizer.authorLink(resolver, BnpConstants.CHANGE_PASSWORD_RESOURCE_PATH + userToken));
        genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/welcome.html",emailRecipients, emailParams);
      }
    } catch (LoginException e) {
      LOGGER.error("Error while fetching system user", e);
      return JobResult.FAILED;
    }

    return JobResult.OK;
  }


}
