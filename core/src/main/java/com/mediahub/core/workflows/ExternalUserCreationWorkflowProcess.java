package com.mediahub.core.workflows;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.jcr.PropertyType;
import javax.jcr.Session;
import javax.jcr.Value;
import javax.jcr.ValueFactory;

import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.adobe.acs.commons.email.EmailService;
import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectMember;
import com.adobe.cq.projects.api.ProjectMemberRole;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;

/**
 * @author Shipra Arora
 *
 *         Process step to create external user and assign role as External_contributor
 */
@Component(service = WorkflowProcess.class, property = { "process.label=External User Creation" })
public class ExternalUserCreationWorkflowProcess implements WorkflowProcess {

	@Reference
	ResourceResolverFactory resourceResolverFactory;
	@Reference 
	EmailService emailService;	
	@Reference
	GenericEmailNotification genericEmailNotification;

	private final Logger logger = LoggerFactory.getLogger(getClass());

	/**
	 * Custom workflow process step to identify project from the payload
	 * 
	 * @see com.adobe.granite.workflow.exec.WorkflowProcess#execute(com.adobe.granite.workflow.exec.WorkItem,
	 *      com.adobe.granite.workflow.WorkflowSession,
	 *      com.adobe.granite.workflow.metadata.MetaDataMap)
	 */

	@Override
	public void execute(WorkItem item, WorkflowSession wfsession, MetaDataMap args) throws WorkflowException {

		logger.info("ExternalUserCreationWorkflowProcess :: exceute method start");

		ResourceResolver resourceResolver = null;

		try {
			
			final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
	                BnpConstants.WRITE_SERVICE);
			resourceResolver = resourceResolverFactory.getServiceResourceResolver(authInfo);
			String payloadPath = item.getWorkflowData().getPayload().toString();
			String projectName =payloadPath.substring(payloadPath.lastIndexOf("/")+1, payloadPath.length());
			logger.debug("ExternalUserCreationWorkflowProcess :: payloadPath" + payloadPath);
			Session adminSession = resourceResolver.adaptTo(Session.class);
			JackrabbitSession js = (JackrabbitSession) adminSession;
			
			String userName = item.getWorkflow().getMetaDataMap().get("firstName").toString();
			String lastName = item.getWorkflow().getMetaDataMap().get("lastName").toString();
			String email = item.getWorkflow().getMetaDataMap().get("email").toString();
			String expiryDate = item.getWorkflow().getMetaDataMap().get("expiryDate").toString();
			String projectPath = item.getWorkflow().getMetaDataMap().get("project").toString();
			Boolean isUserAlreadyExists = false;
			
			
			if (payloadPath != null) {
				
				
				org.apache.jackrabbit.api.security.user.UserManager userManager = js.getUserManager();
				org.apache.jackrabbit.api.security.user.User user = null;
				ValueFactory valueFactory = adminSession.getValueFactory();
				
		        if (userManager.getAuthorizable(userName) == null) {
		            user = userManager.createUser(userName, "passwordAdmin123#");

		            
		            Value firstNameValue = valueFactory.createValue(userName, PropertyType.STRING);
		            user.setProperty("./profile/givenName", firstNameValue);

		            Value lastNameValue = valueFactory.createValue(lastName, PropertyType.STRING);
		            user.setProperty("./profile/familyName", lastNameValue);

		            Value emailValue = valueFactory.createValue(email, PropertyType.STRING);
		            user.setProperty("./profile/email", emailValue);
		            
		            
		            Value expiryDateValue = valueFactory.createValue(expiryDate, PropertyType.STRING);
		            user.setProperty("./profile/expiryDate", expiryDateValue);
		            
				       
				    
		            
		            


		        } else {
		        	
		        	isUserAlreadyExists = true;
		            logger.info("---> User already exist..");
		            if(userManager.getAuthorizable(userName).getProperty("./profile/expiryDate")!=null)
		            {
		            String userExpiryDate = userManager.getAuthorizable(userName).getProperty("./profile/expiryDate")[0].toString().substring(0, 10);
		            String newExpiryDate = expiryDate.substring(0,10);
		            Date start = new SimpleDateFormat("yyyy-MM-dd").parse(userExpiryDate);
		            Date end = new SimpleDateFormat("yyyy-MM-dd").parse(newExpiryDate);
		            
		            if (start.compareTo(end) < 0)
		            {
		            	Value expiryDateValue = valueFactory.createValue(expiryDate, PropertyType.STRING);
		            	userManager.getAuthorizable(userName).setProperty("./profile/expiryDate", expiryDateValue);
		            }
		            
		            }
		            else
		            	
		            {
		            	Value expiryDateValue = valueFactory.createValue(expiryDate, PropertyType.STRING);
		            	userManager.getAuthorizable(userName).setProperty("./profile/expiryDate", expiryDateValue);
		            }
		            logger.info("---> NewExpiry Date");
		            
		            
		        }
		        
			       Project project = resourceResolver.getResource(projectPath).adaptTo(Project.class);
			       Set<ProjectMember> projectMembers = project.getMembers();
			       List<String> usersList = new ArrayList<String>();
			       List<String> rolesList = new ArrayList<String>();
			       
			       for (ProjectMember memberObj : projectMembers) {
						usersList.add(memberObj.getId());
						Set<ProjectMemberRole> projectRoles = memberObj.getRoles();
						for (ProjectMemberRole roleObj : projectRoles) {
							rolesList.add(roleObj.getId());
						}
					}
			       
			      usersList.add(userName);
			      rolesList.add("external-contributor");
			      project.updateMembers(usersList, rolesList); 
			      
			      
			     //notification with the expiry date modification if the user already exists and project link...username and pwd
			       String[] emailRecipients = { email };
			       String subject = "Mediahub - Assignment project " + projectName;
			       String bodyforNewUser = "This is your credentials to access the new project assigned :  " + project.getTitle() + "\\n" + "Login:" + userName + "\\n" + "Password :" + "password";
			       String bodyforExistingUser = "The new Project has been assigned to you : "+ project.getTitle() +" with the expiry Date as: "
			       		+ userManager.getAuthorizable(userName).getProperty("./profile/expiryDate")[0].toString();
			       if(isUserAlreadyExists)
				       {

						 genericEmailNotification.sendEmail(emailRecipients, bodyforExistingUser, project.getTitle(), subject);
						 
				       }
			       else {
			    	   genericEmailNotification.sendEmail(emailRecipients, bodyforNewUser, project.getTitle(), subject);
			       }
				       
			       if (! userManager.isAutoSave()) {
		            	  js.save();
		            	}
			       
			       
			       
		    }
			
			
			}
		catch (Exception e) {
			logger.error("Exception in ExternalUserCreationWorkflowProcess", e);
		} finally {
			if (resourceResolver != null) {
				resourceResolver.close();
			}
			
            }
			

		logger.info("ExternalUserCreationWorkflowProcess :: exceute method end");

	
}
}
