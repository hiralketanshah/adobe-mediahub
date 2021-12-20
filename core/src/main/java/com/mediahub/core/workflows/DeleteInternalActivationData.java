package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

import java.util.Collections;
import java.util.Map;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for Deleting Internal Activation details
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Internal Activation Details Metadata"})
public class DeleteInternalActivationData implements WorkflowProcess {

    @Reference
    ResourceResolverFactory resolverFactory;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)){
            String payloadPath = workItem.getWorkflowData().getPayload().toString();

            Resource movedAsset = resourceResolver.getResource(payloadPath);

            if (null != movedAsset) {
                Resource metadata = movedAsset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                ModifiableValueMap modifiableValueMap = metadata.adaptTo(ModifiableValueMap.class);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_BROADCAST_URL);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_URL);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_URL_MD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_URL_HD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_URL_SUPER_HD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_MD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_HD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_SUPER_HD);
                modifiableValueMap.remove(BnpConstants.BNPP_INTERNAL_FILE_MASTER_URL_PLAYER);
                modifiableValueMap.remove(BnpConstants.BNPP_BROADCAST_STATUS);
                resourceResolver.commit();
            }

        } catch (Exception e) {
            throw new WorkflowException("Error while removing attributes for internal asset", e);
        }

    }
}
