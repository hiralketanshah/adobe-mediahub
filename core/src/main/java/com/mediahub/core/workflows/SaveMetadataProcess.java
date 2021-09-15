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
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.eclipse.jetty.util.URIUtil;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Save Asset Metadata"})
public class SaveMetadataProcess implements WorkflowProcess {

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    Externalizer externalizer;

    @Reference
    Scene7Service scene7Service;

    @Reference
    Scene7DeactivationService scene7DeactivationService;

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
                setVideoAssetMetadata(resourceResolver, movedAsset, modifiableValueMap);
                String broadcastUrl = "/player.jsp?content=" + URIUtil.encodePath(payloadPath);
                modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, externalizer.externalLink(resourceResolver, "internal", broadcastUrl));
                modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_FILE_URL, externalizer.externalLink(resourceResolver, "internal", "/") + "mh/internal/master/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                resourceResolver.commit();
            }

        } catch (LoginException | PersistenceException e) {

            throw new WorkflowException("Error while adding attributes for internal asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }

    }

    /**
     * @param resourceResolver - Resolver object
     * @param movedAsset
     * @param modifiableValueMap - value map containing properties
     */
    private void setVideoAssetMetadata(ResourceResolver resourceResolver, Resource movedAsset,
        ModifiableValueMap modifiableValueMap) {
        if(DamUtil.isAsset(movedAsset) && DamUtil.isVideo(DamUtil.resolveToAsset(movedAsset))){
            S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
            List<Scene7Asset> scene7Assets = scene7Service.getAssets(new String[]{modifiableValueMap.get("dam:scene7ID", StringUtils.EMPTY)}, null, null, s7Config);
            if (scene7Assets != null && !scene7Assets.isEmpty()) {
                setMediumHighDefinitionAssetUrls(scene7Assets, resourceResolver, modifiableValueMap, s7Config);
            }

        }
    }

    /**
     * Setting medium and high definition urls
     *
     * @param resourceResolver   - Resolver object
     * @param modifiableValueMap - value map containing properties
     */
    private void setMediumHighDefinitionAssetUrls(List<Scene7Asset> scene7Assets, ResourceResolver resourceResolver, ModifiableValueMap modifiableValueMap, S7Config s7Config) {

        if (scene7Assets == null || scene7Assets.isEmpty()) {
            return;
        }

        Scene7Asset associatedAsset = scene7Service.getAssociatedAssets(scene7Assets.get(0), s7Config);
        if(null == associatedAsset){
            return;
        }

        List<Scene7Asset> subAssets = associatedAsset.getSubAssets();
        for (Scene7Asset asset : subAssets) {
            if (asset != null && asset.getHeight() != null) {
                if (asset.getHeight() == 540L) {
                    modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_MD, externalizer.externalLink(resourceResolver, Externalizer.PUBLISH, "/" + BnpConstants.IS_CONTENT + asset.getFolder() + asset.getFileName()));
                }
                if (asset.getHeight() == 720L) {
                    modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_HD, externalizer.externalLink(resourceResolver, Externalizer.PUBLISH, "/" + BnpConstants.IS_CONTENT + asset.getFolder() + asset.getFileName()));
                }
            }
        }

    }
}
