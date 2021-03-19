package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.servlet.Servlet;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = Servlet.class,
    property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST,
        "sling.servlet.resourceTypes=" + "cq/Page", "sling.servlet.selectors=" + "metadata-migration",
        "sling.servlet.extensions=" + "json"})
@ServiceDescription("Metadata Migration Servlet")
public class MetadataMigrationServlet extends SlingAllMethodsServlet {

  private static final Logger LOGGER = LoggerFactory.getLogger(MetadataMigrationServlet.class);

  private static final long serialVersionUID = 1L;

  @Override
  protected void doPost(final SlingHttpServletRequest request,
      final SlingHttpServletResponse response) throws IOException {
    LOGGER.debug("Executing Migration Servlet...");
    Map<String, List<String>> assets = getAssetData(request);
    List<Object> propertyNames = new ArrayList<>();

    for(String assetPath : assets.keySet()){
      if(StringUtils.equals(assetPath, "assetPath")){
        extractExcelHeaders(assets, propertyNames, assetPath);
      } else {
        Resource asset = request.getResourceResolver().getResource(assetPath);
        if(null != asset && asset.getChild(JcrConstants.JCR_CONTENT) != null && asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null){
          asset = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
        }
        ModifiableValueMap ModifiableValueMap = null;
        if(null != asset){
          ModifiableValueMap = asset.adaptTo(ModifiableValueMap.class);
        }
        if(ModifiableValueMap != null){
          setResourceMetadata(request, assets, propertyNames, assetPath, ModifiableValueMap);
        }
      }
    }
    assets.keySet();
  }

  /**
   * @param request
   * @param assets
   * @param propertyNames
   * @param assetPath
   * @param modifiableValueMap
   * @throws PersistenceException
   */
  private void setResourceMetadata(SlingHttpServletRequest request,
      Map<String, List<String>> assets, List<Object> propertyNames, String assetPath,
      ModifiableValueMap modifiableValueMap) throws PersistenceException {
    List<String> propertyValues = assets.get(assetPath);
    for(int index =0 ; index < propertyValues.size(); index++ ){
      if (propertyNames.get(index) instanceof String){
        modifiableValueMap.put(propertyNames.get(index).toString(), propertyValues.get(index));
      } else {
        modifiableValueMap.put( ((String[])propertyNames.get(index))[0], new String[] {propertyValues.get(index)});
      }
    }
    request.getResourceResolver().commit();
  }

  /**
   * @param assets
   * @param propertyNames
   * @param assetPath
   */
  private void extractExcelHeaders(Map<String, List<String>> assets, List<Object> propertyNames,
      String assetPath) {
    List<String> namesWithType = assets.get(assetPath);
    for(String name : namesWithType){
      if(name.contains("{{")){
        String values[] = name.split("\\{\\{");
        if(StringUtils.contains(values[1], "multi")){
          propertyNames.add(new String[] {values[0]});
        } else {
          propertyNames.add(values[0]);
        }
      }
    }
  }

  /**
   * @param request 
   * @return
   * @throws IOException
   */
  private Map<String, List<String>> getAssetData(SlingHttpServletRequest request)
      throws IOException {
    Map<String, List<String>> assets = new HashMap<>();
    try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("file").getInputStream()))) {
      String line;
      while (null != (line = br.readLine())) {
        String[] details = line.split(",");
        List<String> values = new ArrayList<>();
        for (int index = 1; index < details.length; index++) {
          values.add(details[index]);
        }
        assets.put(details[0], values);
      }
    }

    return assets;
  }
}
