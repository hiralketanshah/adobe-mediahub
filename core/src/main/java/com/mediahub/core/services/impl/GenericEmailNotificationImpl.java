package com.mediahub.core.services.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.adobe.acs.commons.email.EmailService;
import com.adobe.acs.commons.email.EmailServiceConstants;
import com.mediahub.core.constants.BnpConstants;
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

	public void sendEmail(String[] emailRecipients, String body, String title, String subject) {

		String[] recipients = emailRecipients ;
		Map<String, String> emailParams = new HashMap<String, String>();
		emailParams.put("body", body);
		emailParams.put(BnpConstants.SUBJECT, subject);
		emailParams.put("title", title);

		emailParams.put(EmailServiceConstants.SENDER_EMAIL_ADDRESS, BnpConstants.SENDER_EMAIL_ADDRESS);

		List<String> failureList = null;
		failureList = emailService.sendEmail(BnpConstants.GENERIC_TEMPLATE_PATH, emailParams, recipients);

		if (failureList.isEmpty()) {
			logger.debug("Mail sent successfully: workflow notification");
		} else {
			logger.debug("Failure in sending mail: workflow notification");
		}

	}

	
	
}
