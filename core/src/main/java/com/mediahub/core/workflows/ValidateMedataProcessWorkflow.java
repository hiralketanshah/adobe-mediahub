package com.mediahub.core.workflows;


import com.adobe.granite.asset.api.AssetManager;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;

import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : VALIDATE METADATA"})
public class ValidateMedataProcessWorkflow implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(ValidateMedataProcessWorkflow.class);


    @Reference
    private ResourceResolverFactory resolverFactory;

    /**
     * The method called by the AEM Workflow Engine to perform Workflow work.
     *
     * @param workItem        the work item representing the resource moving through the Workflow
     * @param workflowSession the workflow session
     * @param args            arguments for this Workflow Process defined on the Workflow Model (PROCESS_ARGS, argSingle, argMulti)
     * @throws WorkflowException when the Workflow Process step cannot complete. This will cause the WF to retry.
     */
    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap args) throws WorkflowException {

        if (workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {

            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);

            ResourceResolver resourceResolver = null;
            try {
                resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
             //   if (resourceResolver == null)
             //       throw new WorkflowException("Resource resolver retourne null, Vérifier les permissions du system User");

                String payloadPath = workItem.getWorkflowData().getPayload().toString();
                log.info("payloadPath :" + payloadPath);
                Resource assetResource = resourceResolver.getResource(payloadPath);
                Asset asset = DamUtil.resolveToAsset(assetResource);
                AssetManager assetManager = resourceResolver.adaptTo(AssetManager.class);
                
                // Get the metadata schema (for media or asset)
                // Validate all mandatory fields are filled. If not return an error with the list of fields
             
                
                
     
         
            } catch (LoginException e) {
                throw new WorkflowException("Login exception", e);
            }finally {
                if (resourceResolver != null && resourceResolver.isLive()) {
                    resourceResolver.close();
                }
            }
        } else {
            throw new WorkflowException("Impossible de recupérer le PayLoad");
        }
    }
}