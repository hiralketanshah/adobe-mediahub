package com.mediahub.core.workflows;

import static org.apache.tika.parser.ner.NamedEntityParser.LOG;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.Scene7DeactivationService;
import java.io.IOException;
import java.util.Collections;
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
  ResourceResolverFactory resolverFactory;

  @Reference
  Externalizer externalizer;

  @Reference
  HttpClientBuilderFactory httpClientBuilderFactory;

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

      if(null == damResource){
        return;
      }

      if(LOG.isInfoEnabled()){
        log.info("end point url {} ", externalizer.authorLink(resourceResolver,payloadPath));
      }

      String action = "deactivate";
      if(metaDataMap.containsKey("PROCESS_ARGS")){
        action = metaDataMap.get("PROCESS_ARGS", String.class);
      }

      HttpPost post = scene7DeactivationService.createGetRequestForMigration(externalizer.authorLink(resourceResolver, payloadPath), payloadPath, action);
      HttpClientBuilder builder = httpClientBuilderFactory.newBuilder();
      RequestConfig requestConfig = RequestConfig.custom()
          .setConnectTimeout(scene7DeactivationService.getConnectionTimeOut())
          .setSocketTimeout(scene7DeactivationService.getSocketTimeOut())
          .build();
      builder.setDefaultRequestConfig(requestConfig);
      String responseString = StringUtils.EMPTY;
      try(CloseableHttpClient httpClient = builder.build()){
        HttpResponse response =  httpClient.execute(post);
        if(response.getStatusLine().getStatusCode() == HttpServletResponse.SC_OK) {
          responseString = EntityUtils.toString(response.getEntity());

          JSONObject responseJson = new JSONObject(responseString);
          String status = responseJson.getString(payloadPath);
          if(StringUtils.equals("deactivate",status)){
            log.info("The Asset {} has been successfully Unpublised from scene 7", payloadPath);
            ModifiableValueMap modifiableValueMap = damResource.adaptTo(ModifiableValueMap.class);
            modifiableValueMap.put("bnpp-external-broadcast-url", null);
            modifiableValueMap.remove("bnpp-external-file-url", null);
            resourceResolver.commit();
          } else {
            throw new WorkflowException("Not able to deactivate the Asset from Dynamic Media. The status of asset is : " +  status);
          }

        }else {
          throw new IOException("Call to url failed" + EntityUtils.toString(response.getEntity()));
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
