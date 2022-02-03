package com.mediahub.core.services.impl;

import com.adobe.granite.asset.api.Asset;
import com.adobe.granite.jmx.annotation.AnnotatedStandardMBean;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.data.User;
import com.mediahub.core.data.UserInfo;
import com.mediahub.core.data.UserStatus;
import com.mediahub.core.services.UpdateInternalUsersService;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.*;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.*;
import javax.management.DynamicMBean;
import javax.management.NotCompliantMBeanException;
import java.io.*;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Principal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;

@SuppressWarnings("findsecbugs:PATH_TRAVERSAL_IN")
@Component(service = {DynamicMBean.class, UpdateInternalUsersService.class}, property = {
        "jmx.objectname = com.mediahub.core.services.impl:type=Update Internal Users"})
public class UpdateInternalUsersServiceImpl extends AnnotatedStandardMBean implements UpdateInternalUsersService {

    private static final Logger LOGGER = LoggerFactory.getLogger(UpdateInternalUsersServiceImpl.class);

    @Reference
    private ResourceResolverFactory resolverFactory;

    public UpdateInternalUsersServiceImpl() throws NotCompliantMBeanException {
        super(UpdateInternalUsersService.class);
    }

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
            BnpConstants.WRITE_SERVICE);

    @Override
    public String createAndUpdateUsers(String csvUserInfo, String csvAdditionalInfo, String csvStatusInfo) {
        StringBuilder returnValue = new StringBuilder();
        StringBuilder summary = new StringBuilder();
        LOGGER.info("Start time : {}", new SimpleDateFormat("yyyy.MM.dd HH:mm:ss").format(new Date()));
        UpdateUserResult updateUserResult = null;
        long countOfDeletedUser = 0L;
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            if (StringUtils.isNotBlank(csvUserInfo) && null != csvAdditionalInfo && null != csvStatusInfo) {

                Resource csvResource = resourceResolver.getResource(csvUserInfo);
                Resource userInfoResource = resourceResolver.getResource(csvAdditionalInfo);
                Resource userStatusResource = resourceResolver.getResource(csvStatusInfo);

                File csvUserInfoFile = new File(csvUserInfo);
                File csvAdditionalInfoFile = new File(csvAdditionalInfo);
                File csvStatusInfoFile = new File(csvStatusInfo);
                Map<String, User> inputUserMap = new LinkedHashMap<>();
                Map<String, UserInfo> userInfoMap = new LinkedHashMap<>();
                Map<String, UserStatus> userStatusMap = new LinkedHashMap<>();
                if ((null != csvResource && DamUtil.isAsset(csvResource)) || (csvUserInfoFile.isFile())) {
                    InputStream userInputStream = ((csvUserInfo.startsWith(BnpConstants.DAM_PATH)
                            && null != csvResource)
                            ? csvResource.adaptTo(Asset.class)
                            .getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream()
                            : getFileInputStream(csvUserInfoFile));
                    BufferedReader brUser = new BufferedReader(new InputStreamReader(userInputStream));
                    inputUserMap = convertStreamToHashMap(brUser, false);
                    brUser.close();
                }
                if ((null != userInfoResource && DamUtil.isAsset(userInfoResource))
                        || (csvAdditionalInfoFile.isFile())) {
                    InputStream userInfoInputStream = ((csvAdditionalInfo.startsWith(BnpConstants.DAM_PATH)
                            && null != userInfoResource)
                            ? userInfoResource.adaptTo(Asset.class)
                            .getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream()
                            : getFileInputStream(csvAdditionalInfoFile));
                    BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(userInfoInputStream));
                    userInfoMap = convertStreamToHashMapUserInfo(brUserInfo, false);
                    brUserInfo.close();
                }
                if ((null != userStatusResource && DamUtil.isAsset(userStatusResource))
                        || (csvStatusInfoFile.isFile())) {
                    InputStream userStatusInputStream = ((csvStatusInfo.startsWith(BnpConstants.DAM_PATH)
                            && null != userStatusResource)
                            ? userStatusResource.adaptTo(Asset.class)
                            .getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream()
                            : getFileInputStream(csvStatusInfoFile));
                    BufferedReader brUserInfo = new BufferedReader(new InputStreamReader(userStatusInputStream));
                    userStatusMap = convertStreamToHashMapUserStatus(brUserInfo, false);
                    brUserInfo.close();
                }
                UserManager userManager = resourceResolver.adaptTo(UserManager.class);
                Session session = resourceResolver.adaptTo(Session.class);
                updateUserResult = createAndSaveUsers(inputUserMap, userInfoMap, userStatusMap, userManager, session);
                countOfDeletedUser = deletedUnwantedUsers(resourceResolver, inputUserMap);
                if (resourceResolver.hasChanges()) {
                    resourceResolver.commit();
                }
                returnValue.append(
                        "Internal Users are successfully created/updated or deleted as per the records present in the latest CSV file");
            } else {
                returnValue.append("Kindly add CSV file path for User Info and try again!");
            }
        } catch (LoginException e) {
            LOGGER.error("Error while Logging into the repository : {0}", e);
        } catch (IOException e) {
            LOGGER.error("Error while accessing the files : {0}", e);
        } catch (RepositoryException e) {
            LOGGER.error("Error while accessing repository : {0}", e);
        }

        LOGGER.info("End time : {}", new SimpleDateFormat("yyyy.MM.dd HH:mm:ss").format(new Date()));
        summary.append('\n');
        summary.append("Count of users created : " + updateUserResult.getCountOfCreatedUser());
        summary.append('\n');
        summary.append("Count of users updated : " + updateUserResult.getCountOfUpdatedUser());
        summary.append('\n');
        summary.append("Count of users deleted : " + countOfDeletedUser);
        LOGGER.info(summary.toString());

        return returnValue.toString();
    }

    @Override
    public String removeAllUsers() {
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            Session session = resourceResolver.adaptTo(Session.class);
            Node node;

            node = session.getNode(BnpConstants.INTERNAL_USER_PATH);
            node.remove();
            session.save();

        } catch (LoginException e) {
            LOGGER.error("Error while Logging into the repository", e);
        } catch (RepositoryException e) {
            LOGGER.error("Error while accessing repository", e);
        }
        return "Successfully removed all the internal users!";
    }

    public long deletedUnwantedUsers(ResourceResolver resourceResolver, Map<String, User> inputUserMap) throws RepositoryException {
        boolean hasUsers = true;
        final AtomicLong offset = new AtomicLong(0L);
        long countOfDeletedUser = 0l;
        while (hasUsers && !inputUserMap.isEmpty()) {
            Query query = new Query() {
                @Override
                public <T> void build(QueryBuilder<T> builder) {
                    T condition = null;

                    T internalCondition = null;
                    try {
                        internalCondition = builder.eq("profile/@type", resourceResolver.adaptTo(Session.class).getValueFactory().createValue("internal"));
                    } catch (RepositoryException e) {
                        LOGGER.error("Error while accessing repository", e);
                    }
                    condition = (condition == null) ? internalCondition : builder.and(internalCondition, condition);

                    if (condition != null) {
                        builder.setCondition(condition);
                    }
                    builder.setLimit(offset.getAndAdd(10000L), 10000L);
                }
            };

            UserManager um = resourceResolver.adaptTo(UserManager.class);
            Iterator<Authorizable> authorizablesIt = um.findAuthorizables(query);

            if (authorizablesIt.hasNext()) {
                while (authorizablesIt.hasNext()) {
                    Authorizable auth = authorizablesIt.next();
                    String name = auth.getID();
                    if (!inputUserMap.containsKey(name)) {
                        LOGGER.debug("Removing unwanted user : {}", name);
                        auth.remove();
                        countOfDeletedUser++;
                    }
                }
            } else {
                hasUsers = false;
            }
            LOGGER.debug("Completed checking {} users : ", offset);
        }
        return countOfDeletedUser;
    }

    public UpdateUserResult createAndSaveUsers(Map<String, User> inputUserMap, Map<String, UserInfo> userInfoMap, Map<String, UserStatus> userStatusMap, UserManager userManager, Session session) throws RepositoryException {
        long count = 0L;
        long batchCount = 1L;
        long createdUsers = 0L;
        long updatedUsers = 0L;
        Group mediahubBasicGroup = (Group) (userManager.getAuthorizable(BnpConstants.MEDIAHUB_READER_MEDIALIBRARY));
        for (Map.Entry<String, User> entry : inputUserMap.entrySet()) {
            Principal principal = new Principal() {
                public String getName() {
                    return entry.getKey();
                }
            };
            count++;
            if (count >= 10000) {
                count = 0;
                session.save();
                LOGGER.info("{} records processed", 1000L * batchCount++);
            }

            User userObject = entry.getValue();
            if (StringUtils.isNotBlank(userObject.getUoId()) && userInfoMap.containsKey(userObject.getUoId())) {
                UserInfo userInfo = userInfoMap.get(userObject.getUoId());
                userObject.setBusiness(userInfo.getBusiness());
                userObject.setOrganizationUnit(userInfo.getOrganizationUnit());
            }
            if (StringUtils.isNotBlank(userObject.getStatusId())
                    && userStatusMap.containsKey(userObject.getStatusId())) {
                UserStatus userStatus = userStatusMap.get(userObject.getStatusId());
                userObject.setStatus(userStatus.getStatus());
            }

            if (userManager.getAuthorizable(entry.getKey()) != null) {
                if (updateAEMUser(userManager, session, entry.getKey(), entry.getValue())) {
                    updatedUsers++;
                }
            } else {
                if (createAEMUser(userManager, session, principal, entry.getKey(), entry.getValue(), mediahubBasicGroup)) {
                    createdUsers++;
                }
            }
        }
        session.save();
        return new UpdateUserResult(createdUsers, updatedUsers);
    }

    private boolean createAEMUser(UserManager userManager, Session session, Principal principal, String userId, User userInfo, Group mediahubBasicGroup) {
        try {
            String hashedUserId = encryptThisString(userId);
            String password = org.apache.commons.lang.RandomStringUtils.random(14, BnpConstants.P_CHARACTER);
            while (password.matches(BnpConstants.P_CONSTRAINT) == false) {
                password = org.apache.commons.lang.RandomStringUtils.random(14, BnpConstants.P_CHARACTER);
            }
            org.apache.jackrabbit.api.security.user.User user = userManager.createUser(userId, password,
                    principal, BnpConstants.INTERNAL_USER_PATH + "/" + hashedUserId.substring(0, 2) + "/"
                            + hashedUserId.substring(2, 4));
            updateUserInfo(user, userInfo, session);
            mediahubBasicGroup.addMember(user);
            LOGGER.debug("User is created successfully with userId : {}", userId);
            return true;
        } catch (RepositoryException e) {
            LOGGER.error("Error while accessing repository : {0}", e);
        }
        return false;
    }

    private boolean updateAEMUser(UserManager userManager, Session session, String userId, User userInfo) {
        try {
            LOGGER.debug("User {} is already present", userId);
            Authorizable user = userManager.getAuthorizable(userId);
            return updateUserInfo(user, userInfo, session);
        } catch (RepositoryException e) {
            LOGGER.error("Error while accessing repository : {0}", e);
        }
        return false;
    }

    @SuppressWarnings("squid:S2070")
    public static String encryptThisString(String input) {
        String hashtext = input;
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] messageDigest = md.digest(input.getBytes());
            BigInteger no = new BigInteger(1, messageDigest);
            hashtext = no.toString(16);
            while (hashtext.length() < 32) {
                hashtext = "0" + hashtext;
            }
        } catch (NoSuchAlgorithmException e) {
            LOGGER.error("Error while encrypting userID : {0}", e);
        }
        return hashtext;
    }

    private boolean updateUserInfo(Authorizable user, User userInfo, Session session) throws RepositoryException {
        boolean updated = false;
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_GIVENNAME, userInfo.getGivenName())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_GIVENNAME),
                    session.getValueFactory().createValue(userInfo.getGivenName(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_FAMILYNAME, userInfo.getFamilyName())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_FAMILYNAME),
                    session.getValueFactory().createValue(userInfo.getFamilyName(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_EMAIL, userInfo.getEmailId())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_EMAIL),
                    session.getValueFactory().createValue(userInfo.getEmailId(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_JOBTITLE, userInfo.getJobTitle())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_JOBTITLE),
                    session.getValueFactory().createValue(userInfo.getJobTitle(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_CITY, userInfo.getCity())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_CITY),
                    session.getValueFactory().createValue(userInfo.getCity(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_COUNTRY, userInfo.getCountry())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_COUNTRY),
                    session.getValueFactory().createValue(userInfo.getCountry(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_UOID, userInfo.getUoId())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_UOID),
                    session.getValueFactory().createValue(userInfo.getUoId(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_BUSINESS, userInfo.getBusiness())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_BUSINESS),
                    session.getValueFactory().createValue(userInfo.getBusiness(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_TYPE, BnpConstants.VAL_USER_PROFILE_TYPE)) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_TYPE),
                    session.getValueFactory().createValue(BnpConstants.VAL_USER_PROFILE_TYPE, PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_COMPANY, BnpConstants.VAL_USER_PROFILE_COMPANY)) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_COMPANY),
                    session.getValueFactory().createValue(BnpConstants.VAL_USER_PROFILE_COMPANY, PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_ORGANIZATION_UNIT, userInfo.getOrganizationUnit())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_ORGANIZATION_UNIT),
                    session.getValueFactory().createValue(userInfo.getOrganizationUnit(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_STATUS, userInfo.getStatus())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_STATUS),
                    session.getValueFactory().createValue(userInfo.getStatus(), PropertyType.STRING));
            updated = true;
        }
        if (!checkExistance(user, BnpConstants.PN_USER_PROFILE_STATUS_ID, userInfo.getStatusId())) {
            user.setProperty(BnpConstants.USER_PROFILE.concat(BnpConstants.PN_USER_PROFILE_STATUS_ID),
                    session.getValueFactory().createValue(userInfo.getStatusId(), PropertyType.STRING));
            updated = true;
        }
        return updated;
    }

    private boolean checkExistance(Authorizable user, String property, String updatedPropertyValue)
            throws RepositoryException {
        Value[] propertyValue = user.getProperty(BnpConstants.USER_PROFILE + property);
        if (null == updatedPropertyValue) {
            user.removeProperty(BnpConstants.USER_PROFILE + property);
            return true;
        }
        if (null != propertyValue) {
            String propertyString = propertyValue[0].getString();
            return updatedPropertyValue.equals(propertyString);
        }
        return false;
    }

    private InputStream getFileInputStream(File file) {
        InputStream fis = null;
        try {
            fis = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            LOGGER.error("Exception while reading FILE DROP location : {}", e.getMessage());
        }
        return fis;
    }

    public Map<String, UserInfo> convertStreamToHashMapUserInfo(BufferedReader br, boolean skipLine) {
        int skip = skipLine ? 1 : 0;
        List<UserInfo> inputList;
        inputList = br.lines().skip(skip).map(mapToUserInfo).collect(Collectors.toList());
        return inputList.stream()
                .collect(Collectors.toMap(UserInfo::getUoId, Function.identity(), (v1, v2) -> v1, LinkedHashMap::new));

    }

    public Map<String, User> convertStreamToHashMap(BufferedReader br, boolean skipLine) {
        int skip = skipLine ? 1 : 0;
        List<User> inputList;
        inputList = br.lines().skip(skip).map(mapToItem).collect(Collectors.toList());
        inputList.remove(inputList.size() - 1);
        return inputList.stream()
                .collect(Collectors.toMap(User::getId, Function.identity(), (v1, v2) -> v1, LinkedHashMap::new));
    }

    public Map<String, UserStatus> convertStreamToHashMapUserStatus(BufferedReader br, boolean skipLine) {
        int skip = skipLine ? 1 : 0;
        List<UserStatus> inputList;
        inputList = br.lines().skip(skip).map(mapToUserStatus).collect(Collectors.toList());
        return inputList.stream()
                .collect(Collectors.toMap(UserStatus::getId, Function.identity(), (v1, v2) -> v1, LinkedHashMap::new));
    }

    private Function<String, User> mapToItem = line -> {
        String[] p = line.split(BnpConstants.SEMI_COLON);
        User user = null;
        if (null != p) {
            user = new User();
            user.setId(p[0]);
            user.setFamilyName(p.length > 1 ? p[1] : null);
            user.setGivenName(p.length > 2 ? p[2] : null);
            user.setEmailId(p.length > 3 ? p[3] : null);
            user.setJobTitle(p.length > 12 ? p[12] : null);
            user.setCity(p.length > 7 ? p[7] : null);
            user.setCountry(p.length > 8 ? p[8] : null);
            user.setStatusId(p.length > 11 ? p[11] : null);
            user.setUoId(p.length > 4 ? p[4] : null);
            user.setBusiness(null);
            user.setStatus(null);
        }
        return user;
    };

    private Function<String, UserInfo> mapToUserInfo = line -> {
        String[] p = line.split(BnpConstants.SEMI_COLON);
        UserInfo userInfo = null;
        if (null != p) {
            userInfo = new UserInfo();
            userInfo.setUoId(p[0]);
            userInfo.setBusiness(p.length >= 5 ? p[5] : null);
            userInfo.setOrganizationUnit(p.length > 2 ? p[2] : null);
        }
        return userInfo;
    };

    private Function<String, UserStatus> mapToUserStatus = line -> {
        String[] p = line.split(BnpConstants.SEMI_COLON);
        UserStatus userStatus = null;
        if (null != p) {
            userStatus = new UserStatus();
            userStatus.setId(p[0]);
            userStatus.setStatus(p.length > 2 ? p[2] : null);
        }
        return userStatus;
    };

    private class UpdateUserResult {
        private long countOfCreatedUser;
        private long countOfUpdatedUser;

        public UpdateUserResult(long countOfCreatedUser, long countOfUpdatedUser) {
            this.countOfCreatedUser = countOfCreatedUser;
            this.countOfUpdatedUser = countOfUpdatedUser;
        }

        public long getCountOfCreatedUser() {
            return countOfCreatedUser;
        }

        public long getCountOfUpdatedUser() {
            return countOfUpdatedUser;
        }
    }
}