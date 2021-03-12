package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.S7ConfigResolver;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.mediahub.core.constants.BnpConstants;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;

/**
 * @author Abuthahir Ibrahim
 *
 * Process step for Deactivation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Dynamic Media Deactivation"})
public class UnpublishDynamicMediaProcess implements WorkflowProcess{

  @Reference
  S7ConfigResolver s7ConfigResolver;

  @Reference
  Scene7Service scene7Service;

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

      Resource damResource = resourceResolver.getResource(payloadPath);

      if(null != damResource){
        S7Config s7Config = s7ConfigResolver.getS7ConfigForResource(damResource);

        if(s7Config == null){
          throw new WorkflowException("No Scene 7 Clould Configuration for the Asset");
        }
        String status = scene7Service.deleteAsset(payloadPath, s7Config);
        if(StringUtils.equals(status, "failure")){
          throw new WorkflowException("The Asset Could not be deleted in Dynamic Media");
        }
      }

    } catch (LoginException e) {
      throw new WorkflowException("Login exception", e);
    } catch (Exception e) {
      throw new WorkflowException("Exception while deleting asset in scene 7", e);
    } finally {
      if (resourceResolver != null && resourceResolver.isLive()) {
        resourceResolver.close();
      }
    }

  }
}
