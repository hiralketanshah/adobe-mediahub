package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.constants.Scene7AssetType;
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.day.cq.replication.Replicator;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import com.mediahub.core.utils.ReplicationUtils;
import com.mediahub.core.utils.SlingJobUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.JobManager;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Save Scene7 Metadata"})
public class SaveScene7MetadataProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(SaveScene7MetadataProcess.class);

    public static final String IS_CONTENT = "is/content/";
    public static final String S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET = "s7viewers/html5/VideoViewer.html?asset=";
    private static long JOB_TIMEOUT = 15 * 60 * (long) 1000;

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    Scene7Service scene7Service;

    @Reference
    private Externalizer externalizer;

    @Reference
    Scene7DeactivationService scene7DeactivationService;

    @Reference
    JobManager jobManager;

    @Reference
    private Replicator replicator;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap) throws WorkflowException {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource movedAsset = resourceResolver.getResource(payloadPath);
            if (null != movedAsset) {
                Resource metadata = movedAsset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                if (metadata != null) {
                    //Hack to wait for the move to be done
                    Thread.sleep(30000);
                    isInRunningWorkflow(movedAsset);
                    boolean jobSuccess = SlingJobUtils.startS7ActivationJob(movedAsset, jobManager, SlingJobUtils.S7_ACTIVATE_VALUE);
                    if (jobSuccess) {
                        String file = null;
                        ModifiableValueMap modifiableValueMap = resourceResolver.getResource(metadata.getPath()).adaptTo(ModifiableValueMap.class);
                        String domain = modifiableValueMap.get(BnpConstants.S7_DOMAIN_PROPERTY, String.class);
                        String s7Status = modifiableValueMap.get(BnpConstants.S7_FILE_STATUS_PROPERTY, String.class);
                        if (BnpConstants.S7_FILE_STATUS_NOT_SUPPORTED.equals(s7Status) || BnpConstants.S7_FILE_STATUS_COMPLETE.equals(s7Status)) {
                            if (DamUtil.isVideo(DamUtil.resolveToAsset(movedAsset))) {
                                if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(BnpConstants.S7_TYPE, StringUtils.EMPTY), Scene7AssetType.VIDEO.getValue())) {
                                    file = IS_CONTENT + modifiableValueMap.get(BnpConstants.S7_FILE, StringUtils.EMPTY);
                                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET + modifiableValueMap.get(BnpConstants.S7_FILE, StringUtils.EMPTY));
                                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + file);
                                } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(BnpConstants.S7_TYPE, StringUtils.EMPTY), Scene7AssetType.MASTER_VIDEO.getValue())) {
                                    file = IS_CONTENT + modifiableValueMap.get(BnpConstants.S7_FILE, StringUtils.EMPTY);
                                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET + modifiableValueMap.get("dam:scene7FileAvs", StringUtils.EMPTY));
                                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + file);
                                }
                                setMediumHighDefinitionAssetUrls(movedAsset, resourceResolver, modifiableValueMap, domain);
                                modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, externalizer.externalLink(resourceResolver, BnpConstants.EXTERNAL, "/") + "mh/external/player/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                            } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(BnpConstants.S7_TYPE, StringUtils.EMPTY), Scene7AssetType.IMAGE.getValue())) {
                                file = "is/image/" + modifiableValueMap.get(BnpConstants.S7_FILE, StringUtils.EMPTY);
                                modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + file);
                                modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + file);
                            } else if (!BnpConstants.S7_FILE_STATUS_NOT_SUPPORTED.equals(modifiableValueMap.get(BnpConstants.S7_FILE_STATUS_PROPERTY, String.class))) {
                                file = IS_CONTENT + modifiableValueMap.get(BnpConstants.S7_FILE, StringUtils.EMPTY);
                                modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + file);
                                modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + file);
                            }
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL, externalizer.externalLink(resourceResolver, BnpConstants.EXTERNAL, "/") + "mh/external/master/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_LAST_PUBLISHED, Calendar.getInstance());
                            if (file != null) {
                                workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + file);
                            }
                            if (StringUtils.contains(payloadPath, BnpConstants.MEDIALIBRARY_PATH) && DamUtil.isAsset(movedAsset)) {
                                ReplicationUtils.replicateParentMetadata(resourceResolver, movedAsset, replicator);
                            }
                            resourceResolver.commit();
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new WorkflowException("Error while activating asset in S7", e);
        }

    }

    /**
     * Check if the asset is in running workflow
     *
     * @param movedAsset
     */
    private void isInRunningWorkflow(Resource movedAsset) {
        Resource currentAsset = movedAsset;
        long time = System.currentTimeMillis();
        while (DamUtil.isInRunningWorkflow(currentAsset) && System.currentTimeMillis() < time + JOB_TIMEOUT) {
            try {
                log.debug("Wait for the asset {} to be processed", movedAsset.getPath());
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                log.error("Error while waiting for asset to update", e);
            }
        }
        time = System.currentTimeMillis();
        String status = BnpConstants.S7_FILE_STATUS_UPLOAD;
        while (BnpConstants.S7_FILE_STATUS_UPLOAD.equals(status) && System.currentTimeMillis() < time + JOB_TIMEOUT) {
            try {
                log.debug("Wait for the asset {} to be processed in DM", movedAsset.getPath());
                Thread.sleep(1000);
                Resource metadata = movedAsset.getResourceResolver().getResource(movedAsset.getPath()).getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                status = metadata.getValueMap().get(BnpConstants.S7_FILE_STATUS_PROPERTY, String.class);
            } catch (InterruptedException e) {
                log.error("Error while waiting for asset to update in DM", e);
            }
        }
    }

    /**
     * Setting medium and high definition urls
     *
     * @param resourceResolver   - Resolver object
     * @param modifiableValueMap - value map containing properties
     * @param domain
     */
    private void setMediumHighDefinitionAssetUrls(Resource originalAsset, ResourceResolver resourceResolver, ModifiableValueMap modifiableValueMap, String domain) {
        S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
        List<Scene7Asset> scene7Assets = scene7Service.getAssets(new String[]{modifiableValueMap.get("dam:scene7ID", StringUtils.EMPTY)}, null, null, s7Config);
        //scene7Assets = getUpdatedScene7Assets(modifiableValueMap, s7Config, scene7Assets);
        if (scene7Assets != null && !scene7Assets.isEmpty()) {
            Scene7Asset associatedAsset = scene7Service.getAssociatedAssets(scene7Assets.get(0), s7Config);
            if (null != associatedAsset) {
                List<Scene7Asset> subAssets = associatedAsset.getSubAssets();
                for (Scene7Asset asset : subAssets) {
                    if (asset != null && asset.getHeight() != null) {
                        if (asset.getHeight() == 540L) {
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL_MD, domain + IS_CONTENT + asset.getRootFolder() + asset.getName());
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_MD, externalizer.externalLink(resourceResolver, BnpConstants.EXTERNAL, "/") + "mh/external/md/" + originalAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                        }
                        if (asset.getHeight() == 720L) {
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL_HD, domain + IS_CONTENT + asset.getRootFolder() + asset.getName());
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_HD, externalizer.externalLink(resourceResolver, BnpConstants.EXTERNAL, "/") + "mh/external/hd/" + originalAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                        }
                        if (asset.getHeight() == 1080L) {
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL_SUPER_HD, domain + IS_CONTENT + asset.getRootFolder() + asset.getName());
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_SUPER_HD, externalizer.externalLink(resourceResolver, BnpConstants.EXTERNAL, "/") + "mh/external/superhd/" + originalAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                        }
                    }
                }
            }
        }
    }

}
