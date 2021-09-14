package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.resource.*;
import org.eclipse.jetty.util.URIUtil;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

import java.util.Collections;
import java.util.Map;

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
                String broadcastUrl = "/player.jsp?content=" + URIUtil.encodePath(payloadPath);
                modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_BROADCAST_URL, externalizer.externalLink(resourceResolver, "internal", broadcastUrl));
                modifiableValueMap.put(BnpConstants.BNPP_INTERNAL_FILE_URL, externalizer.externalLink(resourceResolver, "internal", "/") + "mh/internal/master/" + movedAsset.getValueMap().get(JcrConstants.JCR_UUID, String.class));
                resourceResolver.commit();
            }

        } catch (Exception e) {
            throw new WorkflowException("Error while adding attributes for internal asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }

    }
}
