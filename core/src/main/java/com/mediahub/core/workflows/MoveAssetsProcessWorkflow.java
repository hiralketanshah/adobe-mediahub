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

import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=VyV Médiathèque : Déplacer asset avant publication"})
public class MoveAssetsProcessWorkflow implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(MoveAssetsProcessWorkflow.class);

    public static final String MOVE_TARGET_FOLDER = "moveTargetFolder";

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

            Map<String, Object> params = new HashMap<>();
            params.put(ResourceResolverFactory.SUBSERVICE, VyvConstants.WORKFLOW_USER);

            ResourceResolver resourceResolver = null;
            try {
                resourceResolver = resolverFactory.getServiceResourceResolver(params);
             //   if (resourceResolver == null)
             //       throw new WorkflowException("Resource resolver retourne null, Vérifier les permissions du system User");

                String payloadPath = workItem.getWorkflowData().getPayload().toString();
                log.info("payloadPath :" + payloadPath);
                Resource assetResource = resourceResolver.getResource(payloadPath);
                Asset asset = DamUtil.resolveToAsset(assetResource);
                AssetManager assetManager = resourceResolver.adaptTo(AssetManager.class);

                String targetDirectory = assetResource.getChild(JcrConstants.JCR_CONTENT).adaptTo(ValueMap.class).get(MOVE_TARGET_FOLDER, String.class);
                // Déplacer l'asset.
                String newPayloadPath = targetDirectory + "/" + asset.getName();
                assetManager.moveAsset(asset.getPath(), newPayloadPath);
                // Modifier le payloadPath avec sa nouvelle valeur pour les step suivante
                WorkflowUtils.updateWorkflowPayload(workItem, workflowSession, newPayloadPath);

                resourceResolver.commit();
            } catch (LoginException e) {
                throw new WorkflowException("Login exception", e);
            } catch (PersistenceException e) {
                throw new WorkflowException("Persistence exception", e);
            } finally {
                if (resourceResolver != null && resourceResolver.isLive()) {
                    resourceResolver.close();
                }
            }
        } else {
            throw new WorkflowException("Impossible de recupérer le PayLoad");
        }
    }
}