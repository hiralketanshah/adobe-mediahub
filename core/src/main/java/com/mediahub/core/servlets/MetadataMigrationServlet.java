package com.mediahub.core.servlets;

import static com.adobe.xfa.STRS.COMMA;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.tagging.InvalidTagFormatException;
import com.day.cq.tagging.Tag;
import com.day.cq.tagging.TagManager;
import com.mediahub.core.constants.BnpConstants;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
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
import org.apache.sling.api.resource.ResourceResolver;
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
  public static final String ASSET_PATH = "assetPath";
  public static final String DATE = "Date:";
  public static final String KEYWORDS = "keywords";
  public static final String MULTI = "multi";

  @Override
  protected void doPost(final SlingHttpServletRequest request,
      final SlingHttpServletResponse response) throws IOException {
    LOGGER.debug("Executing Migration Servlet...");
    Map<String, List<String>> assets = getAssetData(request);
    List<Object> propertyNames = new ArrayList<>();
    String folderMetadataSchema = getFolderSchema(request);
    for(String assetPath : assets.keySet()){
      try{
        migrateMetadataDetails(request, assets, propertyNames, folderMetadataSchema, assetPath);
      }catch(Exception e){
        LOGGER.error("Error while migrating metadata of the asset : ", e);
      }
    }
  }

  /**
   * @param request
   * @param assets
   * @param propertyNames
   * @param folderMetadataSchema
   * @param assetPath
   * @throws PersistenceException
   */
  private void migrateMetadataDetails(SlingHttpServletRequest request,
      Map<String, List<String>> assets, List<Object> propertyNames, String folderMetadataSchema,
      String assetPath) throws PersistenceException {
    if(propertyNames.isEmpty()){
      extractExcelHeaders(assets, propertyNames, ASSET_PATH);
    }

    if (!StringUtils.equals(assetPath, ASSET_PATH)) {
      Resource asset = null;

      if(StringUtils.contains(assetPath, "à")){
        asset = request.getResourceResolver().getResource(StringUtils.replace(assetPath, "à", "�"));
      } else {
        asset = request.getResourceResolver().getResource(assetPath);
      }

      ModifiableValueMap contentValueMap = null;
      if(null != asset && asset.getChild(JcrConstants.JCR_CONTENT) != null ){
        Resource content  = asset.getChild(JcrConstants.JCR_CONTENT);
        contentValueMap = content.adaptTo(ModifiableValueMap.class);
        contentValueMap.put(BnpConstants.FOLDER_METADATA_SCHEMA, folderMetadataSchema);
        asset = createOrGetMetadata(request, content);
      }
      ModifiableValueMap ModifiableValueMap = null;
      if(null != asset){
        ModifiableValueMap = asset.adaptTo(ModifiableValueMap.class);
      }
      if(ModifiableValueMap != null){
        setResourceMetadata(request, assets, propertyNames, assetPath, ModifiableValueMap, contentValueMap);
      }
    }
  }

  /**
   * @param request
   * @param content
   * @return
   * @throws PersistenceException
   */
  private Resource createOrGetMetadata(SlingHttpServletRequest request, Resource content)
      throws PersistenceException {
    Resource asset;
    if( content.getChild(BnpConstants.METADATA) == null){
      ResourceResolver resourceResolver = request.getResourceResolver();
      asset = resourceResolver.create(content, BnpConstants.METADATA,
          Collections.singletonMap(JcrConstants.JCR_PRIMARYTYPE, JcrConstants.NT_UNSTRUCTURED));
      resourceResolver.commit();
    } else {
      asset = content.getChild(BnpConstants.METADATA);
    }
    return asset;
  }

  /**
   * @param request
   * @return
   */
  private String getFolderSchema(SlingHttpServletRequest request) {
    String folderMetadataSchema = "/conf/global/settings/dam/adminui-extension/foldermetadataschema/mediahub-medias-schema";
    if(request.getRequestParameterMap().containsKey(BnpConstants.FOLDER_METADATA_SCHEMA)){
      folderMetadataSchema = request.getRequestParameterMap().getValue(BnpConstants.FOLDER_METADATA_SCHEMA).getString();
    }
    return folderMetadataSchema;
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
      ModifiableValueMap modifiableValueMap, ModifiableValueMap contentValueMap) throws PersistenceException {
    List<String> propertyValues = assets.get(assetPath);
    for(int index =0 ; index < propertyValues.size(); index++ ){
      if (propertyNames.get(index) instanceof String){
        if(StringUtils.equals(propertyNames.get(index).toString(), JcrConstants.JCR_TITLE)){
          contentValueMap.put(propertyNames.get(index).toString(), propertyValues.get(index));
        } else if(StringUtils.equals(propertyNames.get(index).toString(), "bnpp-media")){
          modifiableValueMap.put(propertyNames.get(index).toString(), propertyValues.get(index).toLowerCase());
        } else if(StringUtils.contains(propertyNames.get(index).toString(), DATE)) {
          try {
            Calendar cal = Calendar. getInstance();
            Date date = new SimpleDateFormat("dd-MM-yyyy").parse(propertyValues.get(index));
            cal.setTime(date);
            modifiableValueMap.put( StringUtils.replace(propertyNames.get(index).toString(), DATE, StringUtils.EMPTY), cal );
          } catch (ParseException e) {
            LOGGER.error("Error while parsing Date", e);
          }
        } else {
          modifiableValueMap.put(propertyNames.get(index).toString(), propertyValues.get(index));
        }
      } else {
        setMultiValueProperty(request, propertyNames, modifiableValueMap, propertyValues, index);
      }
    }
    request.getResourceResolver().commit();
  }

  /**
   * Method to add mutivalue property separated by comma
   *
   * @param propertyNames
   * @param modifiableValueMap
   * @param propertyValues
   * @param index
   */
  private void setMultiValueProperty(SlingHttpServletRequest request, List<Object> propertyNames,
      ModifiableValueMap modifiableValueMap, List<String> propertyValues, int index) {

    String propertyName = ((String[])propertyNames.get(index))[0];

    if(StringUtils.equals(propertyName, KEYWORDS)){
      createKeywordTags(request, propertyValues, index);
    }

    if(propertyValues.get(index).contains(COMMA)){
      List<String> multiValueList = Arrays.asList(propertyValues.get(index).split(COMMA));
      modifiableValueMap.put(propertyName, multiValueList.toArray(new String[multiValueList.size()])) ;
    } else {
      modifiableValueMap.put(propertyName, new String[] {propertyValues.get(index)});
    }
  }

  private void createKeywordTags(SlingHttpServletRequest request, List<String> propertyValues,
      int index) {
    TagManager tagManager =request.getResourceResolver().adaptTo(TagManager.class);
    for(String value : propertyValues.get(index).split(COMMA)){
      if(null == tagManager.resolveByTitle(value)){
        Tag mediahub = tagManager.resolve("/content/cq:tags/mediahub");
        if(null != mediahub ) {
          Tag keywords = tagManager.resolve("/content/cq:tags/mediahub/keywords");
          if(null == keywords){
            keywords = tagManager.resolve("/content/cq:tags/mediahub/keywords");
          }
          try {
            tagManager.createTag(keywords.getPath() + "/" + value, value, value,true);
          } catch (InvalidTagFormatException e) {
            LOGGER.error("Error while creating tags",e);
          }
        }
      }
    }
  }

  /**
   * @param assets
   * @param propertyNames
   * @param assetPath
   */
  private void extractExcelHeaders(Map<String, List<String>> assets, List<Object> propertyNames,
      String assetPath) {
    List<String> namesWithType = assets.containsKey(assetPath) ? assets.get(assetPath) : Collections.emptyList();

      for(String name : namesWithType){
        if(name.contains("{{")){
          String values[] = name.split("\\{\\{");
          if(StringUtils.contains(values[1], MULTI)){
            propertyNames.add(new String[] {values[0]});
          } else if(StringUtils.contains(values[1], "Date")){
            propertyNames.add(DATE + values[0]);
          } else {
            propertyNames.add(values[0]);
          }
        }
      }

  }

  /**
   * To get the asset data from excel
   *
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
        String[] details = line.split("\\|");
        List<String> values = new ArrayList<>();
        for (int index = 1; index < details.length; index++) {
          values.add(details[index]);
        }
        if(details.length > 0){
          assets.put(details[0], values);
        }

      }
    }

    return assets;
  }
}
