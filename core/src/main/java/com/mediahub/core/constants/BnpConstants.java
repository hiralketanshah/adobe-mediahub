package com.mediahub.core.constants;

public class BnpConstants {

    private BnpConstants() {

    }

    public static final String METADATA = "metadata";
    public static final String WRITE_SERVICE = "writeService";
    public static final String USER_ID = "userid";
    public static final String SLING_FOLDER = "sling:Folder";
    public static final String SLING_ORDERED_FOLDER = "sling:OrderedFolder";
    public static final String DAM_ASSET = com.day.cq.dam.api.DamConstants.NT_DAM_ASSET;
    public static final String TOPIC_RESOURCE_ADDED = "org/apache/sling/api/resource/Resource/ADDED";
    public static final String TOPIC_RESOURCE_CHANGED = "org/apache/sling/api/resource/Resource/CHANGED";
    public static final String ITEMS = "items";
    public static final String TEMP = "temp";
    public static final String TABS = "tabs";
    public static final String CONF_FOLDERMETADATASCHEMA = "/conf/global/settings/dam/adminui-extension/foldermetadataschema";
    public static final String FOLDER_WIZARD_PATH = "/apps/dam/gui/content/assets/v2/foldersharewizard/jcr:content/content/items/form/items/wizard/items/settingStep/items/fixedColumns/items/fixedColumn4/items/tabs/items";
    public static final String APPS_DAM = "/apps/dam";
    public static final String EVENT_TOPIC = "event/topic";
    public static final String FOLDER_METADATA_SCHEMA = "folderMetadataSchema";
    public static final String METADATA_SCHEMA = "metadataSchema";
    public static final String REQUIRED_CASCADING = "requiredCascading";
    public static final String ALWAYS = "always";
    public static final String BASIC_GROUP = "MEDIAHUB-BASIC";
    public static final String GENERIC_TEMPLATE_PATH = "/etc/mediahub/mailtemplates/genericemailtemplate.html";
    public static final String SUBJECT = "subject";
    public static final String P_CONSTRAINT = "(?=.*\\d.*)(?=.*[a-z].*)(?=.*[A-Z].*)(?=.*[@#%\\*\\-+=~\\[\\]{}<>\\?].*).{8,16}";
    public static final String P_CHARACTER = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789?=.*[@#%*-+=~[]{}<>?";
    public static final String PATH = "path";
    public static final String TYPE = "type";
    public static final String FIRST_PROPERTY = "1_property";
    public static final String FIRST_PROPERTY_VALUE = "1_property.value";
    public static final String YYYY_MM_DD = "yyyy/MM/dd";
    public static final String DD_MM_YYYY = "dd/MM/yyyy";
    public static final String HOME_USERS = "/home/users";
    public static final String REP_USERS = "rep:User";
    public static final String PROFILE_TYPE = "profile/@type";
    public static final String EXTERNAL = "external";
    public static final String EXPIRY = "expiry";
    public static final String PROFILE = "profile";
    public static final String USER_HAS_EXPIRED = "User has Expired";
    public static final String NT_NODE_TYPE = com.day.cq.commons.jcr.JcrConstants.NT_UNSTRUCTURED;
    public static final String AEM_PROJECTS_PATH = "/content/projects";
    public static final String MEDIALIBRARY_PATH = "/content/dam/medialibrary";
    public static final String MEDIALIBRARY_PROJECTS_PATH = "/content/dam/projects";
    public static final String REP_POLICY = "rep:policy";
    public static final String REP_ACL = "rep:ACL";
    public static final String JCR_MIXINTYPES = com.day.cq.commons.jcr.JcrConstants.JCR_MIXINTYPES;
    public static final String REP_ACCESSCONTROLLABLE = "rep:AccessControllable";
    public static final String ROLE_EDITOR = "role_editor";
    public static final String ROLE_OBSERVER = "role_observer";
    public static final String ROLE_OWNER = "role_owner";
    public static final String ROLE_PROJECTPUBLISHER = "role_project-publisher";
    public static final String ROLE_EXTERNALCONTRIBUTEUR = "role_external-contributor";
    public static final String SLING_RESOURCETYPE = "sling:resourceType";
    public static final String PROJECT_RESOURCE = "cq/gui/components/projects/admin/card/projectcontent";
    public final static String PROJECT_READER_GROUP = "mediahub-basic-project-reader";
    public final static String PROJECT_MANAGER_GROUP = "mediahub-basic-project-manager";
    public final static String PROJECT_INTERNAL_CONTRIBUTOR_GROUP = "mediahub-basic-project-internal-contributor";
    public final static String PROJECT_EXTERNAL_CONTRIBUTOR_GROUP = "mediahub-basic-project-external-contributor";
    public final static String PROJECT_PUBLISHER_GROUP = "mediahub-basic-project-publisher";
    public static final String JCR_TITLE = "jcr:title";
    public static final String PROJECT_DUEDATE = "project.dueDate";
    public static final String PEOFILE_EMAIL = "profile/email";
    public static final String PROFILE_GIVEN_NAME = "profile/givenName";
    public static final String LIKE = "like";
    public static final String FIRST_PROPERTY_OPERATION = "1_property.operation";
    public static final String SECOND_DATERANGE_PROPERTY = "2_daterange.property";
    public static final String SECOND_DATERANGE_LOWEROPERATION = "2_daterange.lowerOperation";
    public static final String SECOND_DATERANGE_LOWERBOUND = "2_daterange.lowerBound";
    public static final String SECOND_DATERANGE_UPPEROPERATION = "2_daterange.upperOperation";
    public static final String SECOND_DATERANGE_UPPERBOUND = "2_daterange.upperBound";
    public static final String GREATERTHAN_EQUALS = ">=";
    public static final String LESSTHAN_EQUALS = "<=";
    public static final String YYYY_MM_DD_T_HH_MM_SS_SSSZ = "yyyy-MM-dd'T'HH:mm:ss.SSSXXX";
    public static final String ACTIVE = "active";
    public static final String MEDIAHUB_EXPIRE_OROJECT = "Mediahub : Expiring project ";
    public static final String EMAIL = "email";
    public static final String FIRST_NAME = "givenName";
    public static final String REP_PRINCIPAL_NAME = "rep:principalName";
    public static final String MEDIAHUB_BASIC_CONTRIBUTOR = "mediahub-basic-contributor";
    public static final String MEDIAHUB_ADMINISTRATOR = "mediahub-administrator";
    public static final String MEDIAHUB_BASIC_READER = "mediahub-basic-reader";
    public static final String MEDIAHUB_BASIC_ENTITY_MANAGER = "mediahub-basic-entity-manager";
    public static final String MEDIAHUB_READER_MEDIALIBRARY = "mediahub-reader-medialibrary";
    public static final String USER_DEACTIVATION_SERVICE = "userDeactivationService";
    public static final String BNPP_BROADCAST_STATUS = "bnpp-broadcast-status";
    public static final String FIRSTNAME = "firstname";


    /**
     * Assets URLs
     */
    public static final String BNPP_EXTERNAL_FILE_URL = "bnpp-external-file-master-url";
    public static final String BNPP_EXTERNAL_BROADCAST_URL = "bnpp-external-broadcast-master-url";
    public static final String BNPP_EXTERNAL_FILE_URL_HD = "bnpp-external-file-master-url-hd";
    public static final String BNPP_EXTERNAL_FILE_URL_MD = "bnpp-external-file-master-url-md";

    public static final String BNPP_INTERNAL_FILE_URL = "bnpp-internal-file-url";
    public static final String BNPP_INTERNAL_BROADCAST_URL = "bnpp-internal-broadcast-url";

    /**
     * Assets Tracking URLs
     */
    public static final String BNPP_TRACKING_EXTERNAL_FILE_URL = "bnpp-external-file-url";
    public static final String BNPP_TRACKING_EXTERNAL_BROADCAST_URL = "bnpp-external-broadcast-url";
    public static final String BNPP_TRACKING_EXTERNAL_FILE_URL_HD = "bnpp-external-file-url-hd";
    public static final String BNPP_TRACKING_EXTERNAL_FILE_URL_MD = "bnpp-external-file-url-md";

    public static final String S7_DOMAIN_PROPERTY = "dam:scene7Domain";
    public static final String S7_FILE_STATUS_PROPERTY = "dam:scene7FileStatus";
    public static final String S7_FILE_STATUS_NOT_SUPPORTED = "NotSupported";
    public static final String S7_FILE_STATUS_COMPLETE = "PublishComplete";
    public static final String S7_FILE_STATUS_INCOMPLETE = "PublishIncomplete";
    public static final String S7_TYPE = "dam:scene7Type";
    public static final String S7_FILE = "dam:scene7File";

    public static final String BROADCAST_VALUE_EXTERNAL = "external";
    public static final String BROADCAST_VALUE_INTERNAL = "internal";
    public static final String BROADCAST_VALUE_NOT_BROADCAST = "not-broadcast";

    public static final String DAM_PATH = "/content/dam";
    public static final String USER_PROFILE_EMAIL = "./profile/email";
    public static final String P_LIMIT = "p.limit";

    public static final String PRIVACY_ACCEPTED_DATE = "privacyAcceptedDate";
    public static final String AFTER_VALUE = "afterValue";
    public static final String BEFORE_VALUE = "beforeValue";
}
