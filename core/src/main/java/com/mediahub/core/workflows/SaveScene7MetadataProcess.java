package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.commons.jcr.JcrConstants;
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
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * @author Abuthahir Ibrahim
 *
 * Process step for Inter or External Activation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Save Scene7 Metadata"})
public class SaveScene7MetadataProcess implements WorkflowProcess{

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
      if(null != movedAsset){
        Resource metadata = movedAsset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
        ModifiableValueMap modifiableValueMap = metadata.adaptTo(ModifiableValueMap.class);
        String domain = modifiableValueMap.get("dam:scene7Domain", "https://s7g10.scene7.com/");
        String folder =  "is/content/" + modifiableValueMap.get("dam:scene7Folder", StringUtils.EMPTY);
        String broadcastUrl = "/player.jsp?content=";
        modifiableValueMap.put("bnpp-external-broadcast-url", externalizer.publishLink(resourceResolver, broadcastUrl) + domain + folder + movedAsset.getName());
        modifiableValueMap.put("bnpp-external-file-url", domain + folder + movedAsset.getName());
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
