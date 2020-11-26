package com.mediahub.core.services;

import java.util.Map;

/**
 * Generic email utility service interface for sending notification from workflows.
 * @author Shipra Arora
 */
public interface GenericEmailNotification {
	
	public void sendEmail(String[] emailRecipients, String body, String title, String subject);
}
