package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.constants.Scene7AssetType;
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import com.mediahub.core.utils.AssetUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.*;
import org.apache.sling.event.jobs.JobManager;
import org.eclipse.jetty.util.URIUtil;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Save Scene7 Metadata"})
public class SaveScene7MetadataProcess implements WorkflowProcess {

    public static final String DAM_SCENE_7_TYPE = "dam:scene7Type";
    public static final String DAM_SCENE_7_FILE = "dam:scene7File";
    public static final String IS_CONTENT = "is/content/";
    public static final String S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET = "s7viewers/html5/VideoViewer.html?asset=";

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

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {

        ResourceResolver resourceResolver = null;


        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource movedAsset = resourceResolver.getResource(payloadPath);
            if (null != movedAsset) {
                Resource metadata = movedAsset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                ModifiableValueMap modifiableValueMap = metadata.adaptTo(ModifiableValueMap.class);
                String domain = modifiableValueMap.get("dam:scene7Domain", "https://s7g10.scene7.com/");

                AssetUtils.slingJobForActivateOrDeactiveAsset(resourceResolver, movedAsset, scene7DeactivationService,
                    jobManager, scene7Service, "Activate");

                String file;
                if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.VIDEO.getValue())) {
                    file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET
                            + URIUtil.encodePath(modifiableValueMap.get(
                            DAM_SCENE_7_FILE, StringUtils.EMPTY)));
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                    setMediumHighDefinitionAssetUrls(movedAsset, resourceResolver, modifiableValueMap, domain);
                } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.MASTER_VIDEO.getValue())) {
                    file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + S_7_VIEWERS_HTML_5_VIDEO_VIEWER_HTML_ASSET + URIUtil.encodePath(modifiableValueMap.get("dam:scene7FileAvs", StringUtils.EMPTY)));
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                    setMediumHighDefinitionAssetUrls(movedAsset, resourceResolver, modifiableValueMap, domain);
                } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.IMAGE.getValue())) {
                    file = "is/image/" + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + URIUtil.encodePath(file));
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                } else {
                    file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_BROADCAST_URL, domain + URIUtil.encodePath(file));
                    modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                }

                modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_BROADCAST_URL, externalizer.externalLink(resourceResolver, "external", "/") + "mh/external/player/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL, externalizer.externalLink(resourceResolver, "external", "/") + "mh/external/master/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));

                workItem.getWorkflow().getWorkflowData().getMetaDataMap().put(BnpConstants.BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                resourceResolver.commit();
            }
        } catch (LoginException | PersistenceException e) {
            throw new WorkflowException("Login exception", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
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
        if (scene7Assets != null && !scene7Assets.isEmpty()) {
            Scene7Asset associatedAsset = scene7Service.getAssociatedAssets(scene7Assets.get(0), s7Config);

            if (null != associatedAsset) {
                List<Scene7Asset> subAssets = associatedAsset.getSubAssets();
                for (Scene7Asset asset : subAssets) {
                    if (asset != null && asset.getHeight() != null) {
                        if (asset.getHeight() == 388L) {
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL_MD, domain + IS_CONTENT + asset.getFolder() + asset.getFileName());
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_MD, externalizer.externalLink(resourceResolver, "external", "/") + "mh/external/md/" + originalAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                        }
                        if (asset.getHeight() == 720L) {
                            modifiableValueMap.put(BnpConstants.BNPP_EXTERNAL_FILE_URL_HD, domain + IS_CONTENT + asset.getFolder() + asset.getFileName());
                            modifiableValueMap.put(BnpConstants.BNPP_TRACKING_EXTERNAL_FILE_URL_HD, externalizer.externalLink(resourceResolver, "external", "/") + "mh/external/hd/" + originalAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                        }
                    }
                }
            }
        }
    }
}
