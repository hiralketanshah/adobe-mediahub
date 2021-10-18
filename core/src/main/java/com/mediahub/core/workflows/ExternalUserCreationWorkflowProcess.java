package com.mediahub.core.workflows;

import com.adobe.acs.commons.email.EmailService;
import com.adobe.acs.commons.i18n.I18nProvider;
import com.adobe.cq.projects.api.Project;
import com.adobe.cq.projects.api.ProjectMember;
import com.adobe.cq.projects.api.ProjectMemberRole;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.UserUtils;
import org.apache.commons.lang.LocaleUtils;
import org.apache.jackrabbit.api.JackrabbitSession;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.*;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.*;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @author Shipra Arora
 * <p>
 * Process step to create external user and assign role as External_contributor
 */
@Component(service = WorkflowProcess.class, property = {"process.label=External User Creation"})
public class ExternalUserCreationWorkflowProcess implements WorkflowProcess {

    @Reference
    ResourceResolverFactory resourceResolverFactory;

    @Reference
    EmailService emailService;

    @Reference
    GenericEmailNotification genericEmailNotification;

    @Reference
    private Externalizer externalizer;

    @SuppressWarnings("CQRules:AMSCORE-553")
    @Reference
    private SlingSettingsService slingSettingsService;

    @Reference
    I18nProvider provider;

    private final Logger logger = LoggerFactory.getLogger(getClass());

    /**
     * Custom workflow process step to identify project from the payload
     *
     * @see com.adobe.granite.workflow.exec.WorkflowProcess#execute(com.adobe.granite.workflow.exec.WorkItem,
     * com.adobe.granite.workflow.WorkflowSession,
     * com.adobe.granite.workflow.metadata.MetaDataMap)
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
            String projectName = payloadPath.substring(payloadPath.lastIndexOf("/") + 1, payloadPath.length());
            logger.debug("ExternalUserCreationWorkflowProcess :: payloadPath" + payloadPath);


            String firstName = item.getWorkflow().getMetaDataMap().get("firstName").toString();
            String lastName = item.getWorkflow().getMetaDataMap().get("lastName").toString();
            String email = item.getWorkflow().getMetaDataMap().get("email").toString();
            String expiryDate = item.getWorkflow().getMetaDataMap().get("expiryDate").toString();
            String projectPath = item.getWorkflow().getMetaDataMap().get("project").toString();
            String company = item.getWorkflow().getMetaDataMap().get("company").toString();
            String city = item.getWorkflow().getMetaDataMap().get("city").toString();
            String country = item.getWorkflow().getMetaDataMap().get("country").toString();
            Boolean isUserAlreadyExists = false;
            String password = "";

            Session adminSession = resourceResolver.adaptTo(Session.class);
            JackrabbitSession js = (JackrabbitSession) adminSession;

            String userToken = UUID.randomUUID().toString();

            if (payloadPath != null) {
                UserManager userManager = js.getUserManager();
                User user = null;
                ValueFactory valueFactory = adminSession.getValueFactory();

                if (userManager.getAuthorizable(email) == null) {
                    password = org.apache.commons.lang.RandomStringUtils.random(14, BnpConstants.P_CHARACTER);
                    while (password.matches(BnpConstants.P_CONSTRAINT) == false) {
                        password = org.apache.commons.lang.RandomStringUtils.random(14, BnpConstants.P_CHARACTER);

                    }
                    Principal principal = new Principal() {
                        public String getName() {
                            return email;
                        }
                    };
                    String hashedUserId = UserUtils.encryptThisString(email);
                    user = userManager.createUser(email, password, principal, BnpConstants.EXTERNAL_USER_PATH + "/" + hashedUserId.substring(0, 2) + "/"
                            + hashedUserId.substring(2, 4));
                    setUserTokenDetails(resourceResolver, user, userToken);
                    Value firstNameValue = valueFactory.createValue(firstName, PropertyType.STRING);
                    user.setProperty("./profile/givenName", firstNameValue);

                    Value lastNameValue = valueFactory.createValue(lastName, PropertyType.STRING);
                    user.setProperty("./profile/familyName", lastNameValue);

                    Value emailValue = valueFactory.createValue(email, PropertyType.STRING);
                    user.setProperty("./profile/email", emailValue);

                    Value expiryDateValue = valueFactory.createValue(expiryDate, PropertyType.STRING);
                    user.setProperty("./profile/expiry", expiryDateValue);

                    Value typeValue = valueFactory.createValue("external", PropertyType.STRING);
                    user.setProperty("./profile/type", typeValue);

                    Value companyValue = valueFactory.createValue(company, PropertyType.STRING);
                    user.setProperty("./profile/company", companyValue);

                    Value cityValue = valueFactory.createValue(city, PropertyType.STRING);
                    user.setProperty("./profile/city", cityValue);

                    Value countryValue = valueFactory.createValue(country, PropertyType.STRING);
                    user.setProperty("./profile/country", countryValue);
                    // Add User to Group

                    Group addUserToGroup = (Group) (userManager.getAuthorizable(BnpConstants.BASIC_GROUP));
                    if (null != addUserToGroup) {
                        addUserToGroup.addMember(user);
                    }

                } else {
                    isUserAlreadyExists = true;
                    logger.debug("---> User already exist..");
                    user = setExpiryDateExistingUser(email, expiryDate, userManager, valueFactory);

                }

                Project project = resourceResolver.getResource(projectPath).adaptTo(Project.class);
                Set<ProjectMember> projectMembers = project.getMembers();
                List<String> usersList = new ArrayList<>();
                List<String> rolesList = new ArrayList<>();

                for (ProjectMember memberObj : projectMembers) {
                    logger.debug("---> memberObj.getId()" + memberObj.getId());
                    usersList.add(memberObj.getId());
                    Set<ProjectMemberRole> projectRoles = memberObj.getRoles();
                    for (ProjectMemberRole roleObj : projectRoles) {
                        logger.debug("---> roleObj.getId()" + roleObj.getId());

                        rolesList.add(roleObj.getId());
                    }
                }

                usersList.add(email);
                rolesList.add("external-contributor");
                project.updateMembers(usersList, rolesList);


                //notification with the expiry date modification if the user already exists and project link...username and pwd
                String[] emailRecipients = {email};

                String language = UserUtils.getUserLanguage(user);
                Locale locale = LocaleUtils.toLocale(language);
                String subject = ProjectExpireNotificationUtil.getRunmodeText(slingSettingsService) + " - " + provider.translate(" Invitation to join", locale) + " « " + projectName + " » MediaHub project";
                Map<String, String> emailParams = new HashMap<>();
                emailParams.put(BnpConstants.SUBJECT, subject);
                emailParams.put("firstname", user.getProperty(BnpConstants.EXT_USER_PROPERTY_GIVENNAME)[0].toString());
                emailParams.put("projectitle", project.getTitle());
                emailParams.put("login", email);
                emailParams.put("resetlink", externalizer.authorLink(resourceResolver, BnpConstants.CHANGE_PASSWORD_RESOURCE_PATH + userToken));
                emailParams.put("password", password);
                emailParams.put("expiry", user.getProperty(BnpConstants.EXT_USER_PROPERTY_EXPIRY)[0].toString().substring(0, 10));
                emailParams.put("projecturl", externalizer.authorLink(resourceResolver, "/projects/details.html" + payloadPath.replace("/dam", "")));
                emailParams.put("projectowner", item.getWorkflow().getInitiator());

                if (isUserAlreadyExists) {
                    genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentmailtemplate.html", emailRecipients, emailParams);
                } else {
                    genericEmailNotification.sendEmail("/etc/mediahub/mailtemplates/projectassignmentcreamailtemplate.html", emailRecipients, emailParams);
                }

                if (!userManager.isAutoSave()) {
                    js.save();
                }


            }

        } catch (LoginException | RepositoryException | ParseException | NoSuchAlgorithmException e) {
            logger.error("Exception in ExternalUserCreationWorkflowProcess", e);
        } finally {
            if (resourceResolver != null) {
                resourceResolver.close();
            }
        }


        logger.info("ExternalUserCreationWorkflowProcess :: exceute method end");

    }

    /**
     * @param resourceResolver
     * @param user
     * @throws javax.jcr.RepositoryException
     */
    private void setUserTokenDetails(ResourceResolver resourceResolver, User user, String token)
            throws javax.jcr.RepositoryException {
        Resource userResource = resourceResolver.getResource(user.getPath());
        ModifiableValueMap modifiableValueMap = userResource.adaptTo(ModifiableValueMap.class);

        modifiableValueMap.put("userToken", token);
        Calendar tokenExpiryDate = Calendar.getInstance();
        tokenExpiryDate.add(Calendar.DATE, 1);
        modifiableValueMap.put("tokenExpiryDate", tokenExpiryDate);
    }

    /**
     * Set Expiry date for existing user
     *
     * @param email
     * @param expiryDate
     * @param userManager
     * @param valueFactory
     * @return
     * @throws javax.jcr.RepositoryException
     * @throws ParseException
     */
    protected User setExpiryDateExistingUser(String email, String expiryDate, UserManager userManager,
                                             ValueFactory valueFactory) throws javax.jcr.RepositoryException, ParseException {
        User user;
        user = (User) userManager.getAuthorizable(email);
        if (user.getProperty("./profile/expiry") != null) {
            String userExpiryDate = user.getProperty("./profile/expiry")[0].toString().substring(0, 10);
            String newExpiryDate = expiryDate.substring(0, 10);
            Date start = new SimpleDateFormat("yyyy/MM/dd").parse(userExpiryDate);
            Date end = new SimpleDateFormat("yyyy/MM/dd").parse(newExpiryDate);

            if (start.compareTo(end) < 0) {
                Value expiryDateValue = valueFactory.createValue(expiryDate, PropertyType.STRING);
                user.setProperty("./profile/expiry", expiryDateValue);
            }

        }
        return user;
    }
}
