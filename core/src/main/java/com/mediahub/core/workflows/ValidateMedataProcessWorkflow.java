package com.mediahub.core.workflows;


import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Session;
import java.util.*;


@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : VALIDATE METADATA"})
public class ValidateMedataProcessWorkflow implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(ValidateMedataProcessWorkflow.class);


    @Reference
    ResourceResolverFactory resolverFactory;

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

        if (!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {
            throw new WorkflowException("Unable to get the payload");
        }
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            List<String> missedAssetMetaData = new ArrayList<>();
            List<String> missedFolderMetaData = new ArrayList<>();
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            log.debug("payloadPath : {}", payloadPath);
            Resource resource = resourceResolver.getResource(payloadPath);

            if (resource == null || resource.getChild(JcrConstants.JCR_CONTENT) == null) {
                throw new WorkflowException("The asset Does not has any jcr:content Node. Hence no use in validation");
            }

            if ((StringUtils.equals(resource.getResourceType(), BnpConstants.DAM_ASSET))) {
                Resource folderContentResource = resource.getParent().getChild(JcrConstants.JCR_CONTENT);
                Resource metadataResource = resource.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                String matadataSchemaPath = folderContentResource.getValueMap().get(BnpConstants.METADATA_SCHEMA, StringUtils.EMPTY);
                missedAssetMetaData = checkMissingMetadata(resourceResolver, missedAssetMetaData, resource, metadataResource, matadataSchemaPath);


                //Validation Folder Metadata
                if (folderContentResource.getChild(BnpConstants.METADATA) != null) {
                    Resource folderMetadataResource = folderContentResource.getChild(BnpConstants.METADATA);
                    String foldermetadataSchemaPath = folderContentResource.getValueMap().get(BnpConstants.FOLDER_METADATA_SCHEMA, StringUtils.EMPTY);
                    missedFolderMetaData = checkMissingMetadata(resourceResolver, missedFolderMetaData, resource.getParent(), folderMetadataResource, foldermetadataSchemaPath);
                    isAssetProcessed(workItem, folderMetadataResource);
                }
            }


            // Validate all mandatory fields are filled. If not return an error with the list of fields
            if (!missedAssetMetaData.isEmpty() || !missedFolderMetaData.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                if (!missedAssetMetaData.isEmpty()) {
                    sb.append("Missing Asset Metadata Fields : ");
                    sb.append(String.join(", ", missedAssetMetaData));
                }

                if (!missedFolderMetaData.isEmpty()) {
                    sb.append(" Missing Folder Metadata Fields : ");
                    sb.append(String.join(", ", missedFolderMetaData));
                }
                log.debug("Missing Fields : {}", sb);
                workItem.getNode().setTitle(sb.toString());
                workItem.getNode().setDescription(sb.toString());
                throw new WorkflowException(String.format("Missing Metadata Fields : %s", sb.toString()));

            }
        } catch (LoginException e) {
            throw new WorkflowException("Error while validating asset metadata", e);
        }

    }

    /**
     * @param workItem
     * @param metadataResource
     * @throws WorkflowException
     */
    protected void isAssetProcessed(WorkItem workItem, Resource metadataResource)
            throws WorkflowException {
        if (metadataResource != null) {
            if (!StringUtils.equals(metadataResource.getValueMap().get("bnpp-status", StringUtils.EMPTY), "validated")) {
                workItem.getNode().setTitle("Asset not Validated");
                workItem.getNode().setDescription("property bnpp-status is not validated");
                throw new WorkflowException("Asset not Validated");
            }
        }
    }

    /**
     * @param resourceResolver
     * @param missedMetaData   - list of missed metadata
     * @param assetResource    - resource of the asset
     * @param metadataResource - authored metadata resource
     * @param schemaPath       - schema path
     * @return list of missed metadata properties
     */
    protected List<String> checkMissingMetadata(ResourceResolver resourceResolver,
                                                List<String> missedMetaData, Resource assetResource, Resource metadataResource,
                                                String schemaPath) {
        if (metadataResource != null && StringUtils.isNotBlank(schemaPath)) {
            Map<String, Object> metadata = assetResource.getChild(JcrConstants.JCR_CONTENT)
                    .getChild(BnpConstants.METADATA).getValueMap();
            missedMetaData = setMissedMetadataFields(resourceResolver, schemaPath, metadata);
        }
        return missedMetaData;
    }

    /**
     * Fetch the schema and use query builder to find the missed fields and then add it to the list
     *
     * @param resourceResolver - resolver for adapting and creating query
     * @param schemaPath       - schema path like folder or asset schema
     * @param metadata         - authored metadata from asset
     * @return - list of missed properties in metadata
     */
    protected List<String> setMissedMetadataFields(ResourceResolver resourceResolver, String schemaPath, Map<String, Object> metadata) {

        List<String> missedMetaData = new ArrayList<>();
        Map<String, String> map = new HashMap<>();
        map.put(BnpConstants.PATH, schemaPath);
        map.put(BnpConstants.TYPE, JcrConstants.NT_UNSTRUCTURED);
        map.put(BnpConstants.FIRST_PROPERTY, BnpConstants.REQUIRED_CASCADING);
        map.put(BnpConstants.FIRST_PROPERTY_VALUE, BnpConstants.ALWAYS);
        QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
        Query query = builder.createQuery(PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
        SearchResult result = query.getResult();
        Iterator<Resource> requiredFields = result.getResources();
        while (requiredFields.hasNext()) {
            Resource field = requiredFields.next();
            String metaField = StringUtils
                    .replace(field.getValueMap().get("cq-msm-lockable", StringUtils.EMPTY), "./metadata/", "");
            if (StringUtils.isNotBlank(metaField) && !metadata.keySet().contains(metaField)) {
                missedMetaData.add(metaField);
            }
        }
        return missedMetaData;
    }
}