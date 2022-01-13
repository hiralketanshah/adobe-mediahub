/*
 *  Copyright 2015 Adobe Systems Incorporated
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package com.mediahub.core.servlets;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.jcr.nodetype.ConstraintViolationException;
import javax.servlet.Servlet;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.commons.jcr.JcrUtil;
import com.mediahub.core.constants.BnpConstants;

/**
 * Servlet to fix media after running the importation process
 */
@SuppressWarnings("CQRules:CQBP-75")
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST, "sling.servlet.paths=" + "/bin/mediahub/updatemedia"})
public class MediaUpdaterServlet extends SlingAllMethodsServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger LOGGER = LoggerFactory.getLogger(MediaUpdaterServlet.class);
    
    private static final String MEDIA = "MEDIA";
    private static final String MEDIAHUB_TAG_BASE = "mediahub:";
    private static final String DATE_OUTPUT_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSSZ";
    private static final String[] FAILURES_HEADER = { "Path", "Message" };
    private static final String[] MEDIA_REQUIRED_FIELDS = { "bnpp-media-type", "bnpp-title-en", "dc:description", "bnpp-date-production", "bnpp-sponsor-entities", "bnpp-contact", "bnpp-contact-country" };
    private static final String MEDIAS_FAILURE_FILE = "C:\\bnp\\mediaUpdaterFailures.csv";
    
    @Override
    protected void doPost(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
        List<String[]> failuresList = new ArrayList<>();
        failuresList.add(FAILURES_HEADER);
        
        LOGGER.debug("Executing Media Updater Servlet...");
        Map<String, String[]> mediasAndFolders = getMediasData(request);
        for (String resourcePath : mediasAndFolders.keySet()) {
            try {
                updateMediaFolder(request, resourcePath, mediasAndFolders.get(resourcePath));
            } catch (ConstraintViolationException | ParseException e) {
                LOGGER.error("Could not update media: " + resourcePath, e);
                failuresList.add(new String[] {resourcePath, e.getMessage()});
            }
        }
        
        FileWriter fileWriter = new FileWriter(new File(MEDIAS_FAILURE_FILE));
        CSVPrinter csvPrinter = new CSVPrinter(fileWriter);
        csvPrinter.println(failuresList.toArray(new String[0][0]));
        fileWriter.flush();
        fileWriter.close();
    }
    
    public String updateMediaFolder(SlingHttpServletRequest request, String resourcePath, String[] resourceProperties) throws PersistenceException, ParseException, ConstraintViolationException {
        DateFormat dateFormat = new SimpleDateFormat(DATE_OUTPUT_FORMAT);
        // req is the SlingHttpServletRequest
        ResourceResolver resourceResolver = request.getResourceResolver();
        Resource resParent;
        boolean willThrowConstraintValidationException = false;
        
        if (StringUtils.contains(resourcePath, "à")) {
            resParent = resourceResolver.getResource(StringUtils.replace(resourcePath, "à", "�"));
        } else {
            resParent = resourceResolver.getResource(resourcePath);
        }
 
        if (resParent != null) {
            ModifiableValueMap  mediaProperties = resParent.adaptTo(ModifiableValueMap.class);
            mediaProperties.put("bnpp-usage-rights", resourceProperties[21]);
            mediaProperties.put("bnpp-usages-territories", resourceProperties[23]);
            if (StringUtils.isNotEmpty(resourceProperties[22])) {
                Calendar rightsEndCalendar = Calendar.getInstance();
                rightsEndCalendar.setTime(dateFormat.parse(resourceProperties[22]));
                mediaProperties.put("bnpp-usage-end-date", rightsEndCalendar);
            }

            Resource resJCR = createOrGetJcrContent(resParent.getResourceResolver(), resParent);

            if(resJCR != null) {
                ModifiableValueMap  properties = resJCR.adaptTo(ModifiableValueMap.class);
                properties.put("folderMetadataSchema", "/conf/global/settings/dam/adminui-extension/foldermetadataschema/mediahub-medias-schema");
                properties.put("sourcing", "false");
                String title = resourceProperties[4];
                if (StringUtils.isEmpty(title)) title = resParent.getName();
                properties.put(JcrConstants.JCR_TITLE, title);
                
                Resource resourceMetadata = createOrGetMetadata(resJCR.getResourceResolver(), resJCR);
                
                if(resourceMetadata != null) {
                    ModifiableValueMap  resourceMetadataProperties = resourceMetadata.adaptTo(ModifiableValueMap.class);
                    resourceMetadataProperties.put("bnpp-media", "true");
                    resourceMetadataProperties.put("bnpp-title-en", resourceProperties[2]);
                    resourceMetadataProperties.put("dc:description", resourceProperties[3]);
                    
                    resourceMetadataProperties.put(JcrConstants.JCR_CREATED_BY, resourceProperties[7]);
                    if (StringUtils.isNotEmpty(resourceProperties[8])) {
                        Calendar createdCalendar = Calendar.getInstance();
                        createdCalendar.setTime(dateFormat.parse(resourceProperties[8]));
                        resourceMetadataProperties.put(JcrConstants.JCR_CREATED, createdCalendar);
                    }
                    
                    resourceMetadataProperties.put(JcrConstants.JCR_LAST_MODIFIED_BY, resourceProperties[9]);
                    if (StringUtils.isNotEmpty(resourceProperties[10])) {
                        Calendar lastModCalendar = Calendar.getInstance();
                        lastModCalendar.setTime(dateFormat.parse(resourceProperties[10]));
                        resourceMetadataProperties.put(JcrConstants.JCR_LASTMODIFIED, lastModCalendar);
                    }
                    
                    resourceMetadataProperties.put("bnpp-local-language", resourceProperties[14]);
                    resourceMetadataProperties.put("bnpp-title-local", resourceProperties[15]);
                    resourceMetadataProperties.put("bnpp-desc-local", resourceProperties[16]);
                    
                    resourceMetadataProperties.put("bnpp-media-type", resourceProperties[43]);
                    resourceMetadataProperties.put("bnpp-status", resourceProperties[44]);
                    
                    resourceMetadataProperties.put("bnpp-contact", inlineAsListAndNormalized(resourceProperties[45]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-contact-country", inlineAsListAndNormalized(resourceProperties[46]).toArray(new String[0]));
                    
                    if (StringUtils.isNotEmpty(resourceProperties[47])) {
                        Calendar productionCalendar = Calendar.getInstance();
                        productionCalendar.setTime(dateFormat.parse(resourceProperties[47]));
                        resourceMetadataProperties.put("bnpp-date-production", productionCalendar);
                    }
                    
                    resourceMetadataProperties.put("bnpp-sponsor-entities", inlineAsListAndNormalized(resourceProperties[48]).toArray(new String[0]));
                    resourceMetadataProperties.put("cq:tags", inlineAsListAndNormalized(resourceProperties[49]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-category", resourceProperties[50] != null ? resourceProperties[50].toLowerCase() : "");
                    resourceMetadataProperties.put("bnpp-type-production", inlineAsListAndNormalized(resourceProperties[51]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-country-prod", inlineAsListAndNormalized(resourceProperties[52]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-identified-entities", inlineAsListAndNormalized(resourceProperties[53]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-identified-persons", inlineAsListAndNormalized(resourceProperties[54]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-theme", inlineAsListAndNormalized(resourceProperties[55]).toArray(new String[0]));
                    resourceMetadataProperties.put("bnpp-keywords", inlineAsListAndNormalized(resourceProperties[56]).toArray(new String[0]));
                    
                    willThrowConstraintValidationException = isAnyFieldEmpty(resourceMetadataProperties, MEDIA_REQUIRED_FIELDS);
                }
            }
            resourceResolver.commit();
            
            if (willThrowConstraintValidationException) {
                throw new ConstraintViolationException("Content is missing required attributes");
            }
        }
        
        return  "Media updated " + resourcePath;
    }
    
    private boolean isAnyFieldEmpty(ModifiableValueMap content, String ... fields) {
        for (String field : fields) {
            if (StringUtils.isBlank(content.get(field, String.class))) return true;
        }
        
        return false;
    }
    
    /**
     * To get the media data from excel
     *
     * @param request
     * @return
     * @throws IOException
     */
    private Map<String, String[]> getMediasData(SlingHttpServletRequest request) throws IOException {
        Map<String, String[]> assets = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("file").getInputStream(), StandardCharsets.UTF_8))) {
            CSVParser csvParser = new CSVParser(br);
            
            //ignore header
            String[] values = csvParser.getLine();
            
            while ((values = csvParser.getLine()) != null) {
                //ensure we have all the data
                if (values.length >= 57) {
                    //only want to update medias
                    if (values[42] != null && values[42].equals(MEDIA)) {
                        assets.put(values[0], values);                        
                    }
                }
            }
        }

        return assets;
    }
    
    /**
     * @param resourceResolver
     * @param content
     * @return
     * @throws PersistenceException
     */
    protected Resource createOrGetMetadata(ResourceResolver resourceResolver, Resource content) throws PersistenceException {
        Resource asset;
        if (content.getChild(BnpConstants.METADATA) == null) {
            asset = resourceResolver.create(content, BnpConstants.METADATA,
                    Collections.singletonMap(JcrConstants.JCR_PRIMARYTYPE, JcrConstants.NT_UNSTRUCTURED));
            resourceResolver.commit();
        } else {
            asset = content.getChild(BnpConstants.METADATA);
        }
        return asset;
    }
    
    /**
     * @param resourceResolver
     * @param content
     * @return
     * @throws PersistenceException
     */
    protected Resource createOrGetJcrContent(ResourceResolver resourceResolver, Resource content) throws PersistenceException {
        Resource asset;
        if (content.getChild(JcrConstants.JCR_CONTENT) == null) {
            asset = resourceResolver.create(content, JcrConstants.JCR_CONTENT,
                    Collections.singletonMap(JcrConstants.JCR_PRIMARYTYPE, JcrConstants.NT_UNSTRUCTURED));
            resourceResolver.commit();
        } else {
            asset = content.getChild(JcrConstants.JCR_CONTENT);
        }
        return asset;
    }
    
    private List<String> inlineAsListAndNormalized(String inline) {
        return StringUtils.isNotEmpty(inline) ? 
                Arrays.asList(inline.split("\\s*\\|\\s*")).stream().map(entity -> getFullTagId(entity.trim())).collect(Collectors.toList()) : 
                    new ArrayList<>();
    }
    
    private String getFullTagId(String tagStr) {
        String delimiter = "/";
        String tagPath = tagStr.replace(MEDIAHUB_TAG_BASE, "");
        String tagIdPath = Arrays.stream(tagPath.split("\\s*\\/\\s*")).map(entity -> JcrUtil.createValidName(entity.trim())).collect(Collectors.joining(delimiter));
        return MEDIAHUB_TAG_BASE + tagIdPath;
    }
}
