package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.scene7.api.constants.Scene7AssetType;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
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
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Save Scene7 Metadata"})
public class SaveScene7MetadataProcess implements WorkflowProcess {

    public static final String DAM_SCENE_7_TYPE = "dam:scene7Type";
    public static final String DAM_SCENE_7_FILE = "dam:scene7File";
    public static final String BNPP_EXTERNAL_BROADCAST_URL = "bnpp-external-broadcast-url";
    public static final String BNPP_EXTERNAL_FILE_URL = "bnpp-external-file-url";
    public static final String IS_CONTENT = "is/content/";

    @Reference
    ResourceResolverFactory resolverFactory;

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

                if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.VIDEO.toString())) {
                    String file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BNPP_EXTERNAL_BROADCAST_URL, domain + "s7viewers/html5/VideoViewer.html?asset=" + URIUtil.encodePath(modifiableValueMap.get(
                        DAM_SCENE_7_FILE, StringUtils.EMPTY)));
                    modifiableValueMap.put(BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.MASTER_VIDEO.toString())) {
                    String file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BNPP_EXTERNAL_BROADCAST_URL, domain + "s7viewers/html5/VideoViewer.html?asset=" + URIUtil.encodePath(modifiableValueMap.get("dam:scene7FileAvs", StringUtils.EMPTY)));
                    modifiableValueMap.put(BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                } else if (StringUtils.equalsIgnoreCase(modifiableValueMap.get(DAM_SCENE_7_TYPE, StringUtils.EMPTY), Scene7AssetType.PDF.toString())){
                    String file = IS_CONTENT + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BNPP_EXTERNAL_BROADCAST_URL, domain + URIUtil.encodePath(file));
                    modifiableValueMap.put(BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                } else {
                    String file = "is/image/" + modifiableValueMap.get(DAM_SCENE_7_FILE, StringUtils.EMPTY);
                    modifiableValueMap.put(BNPP_EXTERNAL_BROADCAST_URL, domain + URIUtil.encodePath(file));
                    modifiableValueMap.put(BNPP_EXTERNAL_FILE_URL, domain + URIUtil.encodePath(file));
                }

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
}
