package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.replication.ReplicationActionType;
import com.day.cq.replication.Replicator;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import com.mediahub.core.utils.AssetUtils;
import com.mediahub.core.utils.ReplicationUtils;
import com.mediahub.core.utils.SlingJobUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.*;
import org.apache.sling.event.jobs.JobManager;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static com.mediahub.core.constants.BnpConstants.S7_FILE_STATUS_NOT_SUPPORTED;
import static com.mediahub.core.constants.BnpConstants.S7_FILE_STATUS_PROPERTY;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Deactivation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Dynamic Media Deactivation"})
public class UnpublishDynamicMediaProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(UnpublishDynamicMediaProcess.class);

    @Reference
    Scene7Service scene7Service;

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    Scene7DeactivationService scene7DeactivationService;

    @Reference
    JobManager jobManager;

    @Reference
    private Replicator replicator;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {

        ResourceResolver resourceResolver = null;
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource damResource = resourceResolver.getResource(payloadPath);
            if (null != damResource) {
                S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
                if (s7Config == null) {
                    throw new WorkflowException("No Scene 7 Clould Configuration for the Asset");
                }
                ValueMap metadata = damResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();


                if (!StringUtils.isEmpty(metadata.get(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, String.class)) || (!StringUtils.isEmpty(metadata.get(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, String.class))) ) {

                    boolean jobSuccess = SlingJobUtils.startS7ActivationJob(damResource, resourceResolver, jobManager, SlingJobUtils.S7_DEACTIVATE_VALUE);
                    if (jobSuccess) {
                        ModifiableValueMap properties = damResource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).adaptTo(ModifiableValueMap.class);
                        workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL, properties.get(BnpConstants.BNPP_EXTERNAL_FILE_URL, StringUtils.EMPTY));
                        workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL_HD, properties.get(BnpConstants.BNPP_EXTERNAL_FILE_URL_HD, StringUtils.EMPTY));
                        workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL_MD, properties.get(BnpConstants.BNPP_EXTERNAL_FILE_URL_MD, StringUtils.EMPTY));
                        workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL_SUPER_HD, properties.get(BnpConstants.BNPP_EXTERNAL_FILE_URL_SUPER_HD, StringUtils.EMPTY));
                        properties.remove(BnpConstants.BNPP_EXTERNAL_FILE_URL);
                        properties.remove(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL);
                        properties.remove(BnpConstants.BNPP_EXTERNAL_FILE_URL_HD);
                        properties.remove(BnpConstants.BNPP_EXTERNAL_FILE_URL_MD);
                        properties.remove(BnpConstants.BNPP_EXTERNAL_FILE_URL_SUPER_HD);
                        properties.remove(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL);
                        properties.remove(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL);
                        properties.remove(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_HD);
                        properties.remove(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_SUPER_HD);
                        properties.remove(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_MD);
                        properties.remove(BnpConstants.BNPP_BROADCAST_STATUS);
                        resourceResolver.commit();

                        if (S7_FILE_STATUS_NOT_SUPPORTED.equals(properties.get(S7_FILE_STATUS_PROPERTY, String.class)) && StringUtils.contains(payloadPath, "/content/dam/medialibrary") && DamUtil.isAsset(damResource)) {
                            ReplicationUtils.replicateContent(payloadPath, resourceResolver, replicator, ReplicationActionType.DEACTIVATE);
                        }
                    }
                }
            }

        } catch (LoginException | PersistenceException e) {
            throw new WorkflowException("Error while deactivating asset from S7", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }

    }

}
