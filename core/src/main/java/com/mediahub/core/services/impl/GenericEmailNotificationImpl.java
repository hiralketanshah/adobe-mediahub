package com.mediahub.core.services.impl;

import java.util.List;
import java.util.Map;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.adobe.acs.commons.email.EmailService;
import com.adobe.acs.commons.email.EmailServiceConstants;
import com.mediahub.core.services.GenericEmailNotification;

/**
 * Generic class for sending email notification. The method can be used as a
 * part of a custom workflow process for sending notification
 * 
 * @author Shipra Arora
 */
@Component(service = GenericEmailNotification.class)
public class GenericEmailNotificationImpl implements GenericEmailNotification {

	@Reference
	EmailService emailService;

	private final Logger logger = LoggerFactory.getLogger(getClass());

	public void sendEmail(String templatePath,String[] emailRecipients, Map<String, String> emailParams) {

		List<String> failureList = null;
		emailParams.put(EmailServiceConstants.SENDER_EMAIL_ADDRESS,"noreply@service.mediahub.bnpparibas");
		emailParams.put(EmailServiceConstants.SENDER_NAME, "Mediahub");

		failureList = emailService.sendEmail(templatePath, emailParams, emailRecipients);

		if (failureList.isEmpty()) {
			logger.debug("Mail sent successfully: workflow notification");
		} else {
			logger.debug("Failure in sending mail: workflow notification");
		}

	}

	
	
}
