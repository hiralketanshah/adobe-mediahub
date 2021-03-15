package com.mediahub.core.workflows;

import static org.apache.tika.parser.ner.NamedEntityParser.LOG;

import com.adobe.forms.foundation.service.util.AssetUtils;
import com.adobe.granite.asset.api.AssetManager;
import com.adobe.granite.asset.api.AssetMetadata;
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
import com.mediahub.core.services.Scene7DeactivationService;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.osgi.services.HttpClientBuilderFactory;
import org.apache.http.util.EntityUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.json.JSONObject;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Abuthahir Ibrahim
 *
 * Process step for Deactivation of Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Dynamic Media Deactivation"})
public class UnpublishDynamicMediaProcess implements WorkflowProcess{

  private static final Logger log = LoggerFactory.getLogger(UnpublishDynamicMediaProcess.class);

  @Reference
  Scene7Service scene7Service;

  @Reference
  ResourceResolverFactory resolverFactory;

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
      Resource damResource = resourceResolver.getResource(payloadPath);
      if(null != damResource){
        S7Config s7Config = resourceResolver.getResource(scene7DeactivationService.getCloudConfigurationPath()).adaptTo(S7Config.class);
        if(s7Config == null){
          throw new WorkflowException("No Scene 7 Clould Configuration for the Asset");
        }
        String status = scene7Service.deleteAsset(damResource.getChild("jcr:content").getChild("metadata").getValueMap().get("dam:scene7ID", StringUtils.EMPTY), s7Config);
        if(StringUtils.equals(status, "failure")){
          throw new WorkflowException("The Asset Could not be deleted in Dynamic Media");
        }
      }

    } catch (LoginException e) {
      throw new WorkflowException("Login exception", e);
    } catch (Exception e) {
      log.error("Exception while deleting asset in scene 7", e);
      throw new WorkflowException("Exception while deleting asset in scene 7", e);
    } finally {
      if (resourceResolver != null && resourceResolver.isLive()) {
        resourceResolver.close();
      }
    }

  }
}
