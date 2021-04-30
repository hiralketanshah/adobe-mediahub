package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.tagging.Tag;
import com.day.cq.tagging.TagManager;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.sling.api.request.RequestParameter;
import org.apache.sling.api.resource.ModifiableValueMap;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletRequest;
import org.apache.sling.testing.mock.sling.servlet.MockSlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class MetadataMigrationServletTest {

  private MetadataMigrationServlet fixture = new MetadataMigrationServlet();

  @Mock
  RequestParameter requestParameter;

  @Mock
  BufferedReader br;

  @Mock
  ResourceResolver resourceResolver;

  @Mock
  Resource resource;

  @Mock
  TagManager tagManager;

  @Mock
  Tag tag;

  @Mock
  ModifiableValueMap modifiableValueMap;

  @BeforeEach
  public void setupMock() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void doGet(AemContext context) throws IOException {

    Map<String, List<String>> assets = new HashMap<>();
    InputStream inputStream = new InputStream() {
      @Override
      public int read() throws IOException {
        return 0;
      }
    };

    MockSlingHttpServletRequest request = context.request();
    MockSlingHttpServletResponse response = context.response();

    String heading = "assetPath|bnpp-media{{ String }}|bnpp-type{{String}}|bnpp-status{{String}}|bnpp-title-local{{String}}|bnpp-desc-local{{String}}|bnpp-geographicalarea{{String: multi }}|jcr:title{{String }}|dc:description{{String}}|bnpp-type-production{{String: multi }}|bnpp-patrimoine{{String: multi }}|bnpp-contact{{String: multi }}|bnpp-country-prod{{String: multi }}|bnpp-date-production{{Date}}|bnpp-type-production{{String: multi }}_1|bnpp-contact-country{{String: multi }}|bnpp-keywords{{String: multi }}\n/content/dam/medialibrary/group_functions/company_engagement/group_communications/relations_exterieures/archives-historiques/archives/rapports-annuels-historiques/Comptoir national d'escompte de Paris/exercice 1939|TRUE|office-automation|validated|Rapport annuel du Comptoir national d'escompte de Paris; exercice 1939|Rapport du conseil d'administration à l'assemblée générale ordinaire du 30 avril 1940|Europe|Annual Report of the Comptoir National d'Escompte de Paris; financial year 1939|Report of the Board of Directors to the ordinary AGM of 30 April 1940|mediahub:production-types/internal|heritage|mediahub:contacts/laperdrix-marie--d89164-|mediahub:geography/europe/france|01-01-1939|mediahub:production-types/internal|mediahub:geography/europe/france,mediahub:geography/europe/germany,mediahub:geography/europe/england|Annual Report,Banque Nationale de Crédit (BNC),War (First World War)";

    context.request().addRequestParameter("file", heading.getBytes(), "file");
    context.request().addRequestParameter(BnpConstants.FOLDER_METADATA_SCHEMA, "/conf/global/settings/dam/adminui-extension/foldermetadataschema/mediahub-medias-schema");


    when(resourceResolver.getResource(anyString())).thenReturn(resource);
    when(requestParameter.getInputStream()).thenReturn(inputStream);
    when(resource.getChild(JcrConstants.JCR_CONTENT)).thenReturn(resource);
    fixture.doPost(request, response);
  }

  @Test
  public void testCreateOrGetMetadata() throws Exception {
    when(resource.getChild(BnpConstants.METADATA)).thenReturn(resource);
    assertNotNull(fixture.createOrGetMetadata(resourceResolver, resource));
  }

  @Test
  public void testCreateOrGetMetadataqEmptyMetadata() throws Exception {
    when(resource.getChild(BnpConstants.METADATA)).thenReturn(null);
    when(resourceResolver.create(resource, BnpConstants.METADATA,
        Collections.singletonMap(JcrConstants.JCR_PRIMARYTYPE, JcrConstants.NT_UNSTRUCTURED))).thenReturn(resource);
    assertNotNull(fixture.createOrGetMetadata(resourceResolver, resource));
  }

  @Test
  public void testCreateKeywordTags() throws Exception {
    List<String> propertyValues = new ArrayList<>();
    propertyValues.add("Tag1");
    when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);
    when(tagManager.resolveByTitle("Tag1")).thenReturn(null);
    when(tagManager.resolve("/content/cq:tags/mediahub")).thenReturn(tag);
    when(tagManager.resolve("/content/cq:tags/mediahub/keywords")).thenReturn(null);
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords", "keywords", "keywords", true)).thenReturn(tag);
    when(tag.getPath()).thenReturn("/content/cq:tags/mediahub/keywords");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag1", "Tag1", "Tag1", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag1");

    assertTrue(!fixture.createKeywordTags(resourceResolver, propertyValues, 0).isEmpty());
  }

  @Test
  public void testMultiValueProperty() throws Exception {

    List<Object> propertyNames = new ArrayList<>();
    propertyNames.add(new String[]{MetadataMigrationServlet.KEYWORDS});
    when(modifiableValueMap.put(any(String.class),any())).thenReturn(null);

    List<String> propertyValues = new ArrayList<>();
    propertyValues.add("Tag1");
    when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);
    when(tagManager.resolveByTitle("Tag1")).thenReturn(null);
    when(tagManager.resolve("/content/cq:tags/mediahub")).thenReturn(tag);
    when(tagManager.resolve("/content/cq:tags/mediahub/keywords")).thenReturn(null);
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords", "keywords", "keywords", true)).thenReturn(tag);
    when(tag.getPath()).thenReturn("/content/cq:tags/mediahub/keywords");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag1", "Tag1", "Tag1", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag1");
    fixture.setMultiValueProperty(resourceResolver, propertyNames, modifiableValueMap, propertyValues, 0);
  }

  @Test
  public void testMultiValuePropertyWithComma() throws Exception {

    List<Object> propertyNames = new ArrayList<>();
    propertyNames.add(new String[]{""});
    when(modifiableValueMap.put(any(String.class),any())).thenReturn(null);

    List<String> propertyValues = new ArrayList<>();
    propertyValues.add("Tag1,Tag2");
    when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);
    when(tagManager.resolveByTitle("Tag1")).thenReturn(null);
    when(tagManager.resolve("/content/cq:tags/mediahub")).thenReturn(tag);
    when(tagManager.resolve("/content/cq:tags/mediahub/keywords")).thenReturn(null);
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords", "keywords", "keywords", true)).thenReturn(tag);
    when(tag.getPath()).thenReturn("/content/cq:tags/mediahub/keywords");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag1", "Tag1", "Tag1", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag1");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag2", "Tag2", "Tag2", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag2");
    fixture.setMultiValueProperty(resourceResolver, propertyNames, modifiableValueMap, propertyValues, 0);
  }

  @Test
  public void testMultiValuePropertyWithoutComma() throws Exception {

    List<Object> propertyNames = new ArrayList<>();
    propertyNames.add(new String[]{""});
    when(modifiableValueMap.put(any(String.class),any())).thenReturn(null);

    List<String> propertyValues = new ArrayList<>();
    propertyValues.add("Tag1");
    when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);
    when(tagManager.resolveByTitle("Tag1")).thenReturn(null);
    when(tagManager.resolve("/content/cq:tags/mediahub")).thenReturn(tag);
    when(tagManager.resolve("/content/cq:tags/mediahub/keywords")).thenReturn(null);
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords", "keywords", "keywords", true)).thenReturn(tag);
    when(tag.getPath()).thenReturn("/content/cq:tags/mediahub/keywords");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag1", "Tag1", "Tag1", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag1");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag2", "Tag2", "Tag2", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag2");
    fixture.setMultiValueProperty(resourceResolver, propertyNames, modifiableValueMap, propertyValues, 0);
  }

  @Test
  public void testResourceMetadata() throws Exception {

    Map<String, List<String>> assets = new HashMap<>();
    List<String> excelHeading = new ArrayList<>();
    excelHeading.add("bnpp-keywords{{String: multi }}");
    assets.put("assetPath", excelHeading);
    /*List<String> asset = new ArrayList<>();
    asset.add("Annual Report,Banque Nationale de Crédit (BNC),War (First World War)");*/
    String assetPath = "/content/dam/medialibrary/group_functions/company_engagement/group_communications/relations_exterieures/archives-historiques/archives/rapports-annuels-historiques/Comptoir national d'escompte de Paris/exercice 1939";


    List<Object> propertyNames = new ArrayList<>();
    propertyNames.add(MetadataMigrationServlet.KEYWORDS);
    propertyNames.add(JcrConstants.JCR_TITLE);
    propertyNames.add("bnpp-media");
    propertyNames.add(MetadataMigrationServlet.DATE);
    when(modifiableValueMap.put(any(String.class),any())).thenReturn(null);

    List<String> propertyValues = new ArrayList<>();
    propertyValues.add("Tag1");
    propertyValues.add("Tag2");
    propertyValues.add("Tag3");
    propertyValues.add("29-04-2021");

    assets.put(assetPath, propertyValues);

    when(resourceResolver.adaptTo(TagManager.class)).thenReturn(tagManager);
    when(tagManager.resolveByTitle("Tag1")).thenReturn(null);
    when(tagManager.resolve("/content/cq:tags/mediahub")).thenReturn(tag);
    when(tagManager.resolve("/content/cq:tags/mediahub/keywords")).thenReturn(null);
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords", "keywords", "keywords", true)).thenReturn(tag);
    when(tag.getPath()).thenReturn("/content/cq:tags/mediahub/keywords");
    when(tagManager.createTag("/content/cq:tags/mediahub/keywords/Tag1", "Tag1", "Tag1", true)).thenReturn(tag);
    when(tag.getTagID()).thenReturn("/content/cq:tags/mediahub/keywords/Tag1");
    fixture.setResourceMetadata(resourceResolver, assets, propertyNames, assetPath, modifiableValueMap, modifiableValueMap);
  }

}
