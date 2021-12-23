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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.Servlet;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.service.component.annotations.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.day.cq.commons.jcr.JcrUtil;
import com.day.cq.tagging.InvalidTagFormatException;
import com.day.cq.tagging.Tag;
import com.day.cq.tagging.TagManager;

/**
 * Servlet to create tags before running the importation process
 */
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST, "sling.servlet.paths=" + "/bin/mediahub/createtags"})
public class TagsCreatorServlet extends SlingAllMethodsServlet {
    

    private static final long serialVersionUID = 1L;

    private static final Logger LOGGER = LoggerFactory.getLogger(TagsCreatorServlet.class);

    private static final String MEDIA = "MEDIA";
    private static final String QUOTES = "\"";
    private static final String TAGS_FAILURE_FILE = "C:\\bnp\\tagFailures.csv";
    private static final String[] FAILURES_HEADER = {"Path", "Message"};
    
    @Override
    protected void doPost(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
        List<String[]> failuresList = new ArrayList<>();
        failuresList.add(FAILURES_HEADER);
        
        LOGGER.debug("Executing Tag Creator Servlet for Entities...");
        List<String> entities = getEntitiesData(request);
        for (String tag : entities) {
            createEntityTag(request, tag, failuresList);
        }
        
        LOGGER.debug("Executing Tag Creator Servlet for Assets...");
        Map<String, String[]> assets = getAssetsData(request);
        for (String resourcePath : assets.keySet()) {
            createAssetTags(request, resourcePath, assets.get(resourcePath), failuresList);
        }
        
        LOGGER.debug("Executing Tag Creator Servlet for Medias...");
        Map<String, String[]> mediasAndFolders = getMediasData(request);
        for (String resourcePath : mediasAndFolders.keySet()) {
            createMediaTags(request, resourcePath, mediasAndFolders.get(resourcePath), failuresList);
        }
        
        FileWriter fileWriter = new FileWriter(new File(TAGS_FAILURE_FILE));
        CSVPrinter csvPrinter = new CSVPrinter(fileWriter);
        csvPrinter.println(failuresList.toArray(new String[0][0]));
        fileWriter.flush();
        fileWriter.close();
    }
    
    public String createMediaTags(SlingHttpServletRequest request, String resourcePath, String[] resourceProperties, List<String[]> tagsFailuresList) {
        // req is the SlingHttpServletRequest
        ResourceResolver resourceResolver = request.getResourceResolver();
        TagManager tagManager = resourceResolver.adaptTo(TagManager.class);

        if (resourceProperties.length >= 46) {
            String contact = resourceProperties[45];
            
            if (StringUtils.isNoneEmpty(contact)) {
                createTags(tagManager, contact, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 47) {
            String contactCountry = resourceProperties[46];
            
            if (StringUtils.isNoneEmpty(contactCountry)) {
                createTags(tagManager, contactCountry, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 49) {
            String sponsor = resourceProperties[48];
            
            if (StringUtils.isNoneEmpty(sponsor)) {
                createTags(tagManager, sponsor, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 50) {
            String tags = resourceProperties[49];
            
            if (StringUtils.isNoneEmpty(tags)) {
                createTags(tagManager, tags, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 52) {
            String productionType = resourceProperties[51];
            
            if (StringUtils.isNoneEmpty(productionType)) {
                createTags(tagManager, productionType, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 53) {
            String productionCountry = resourceProperties[52];
            
            if (StringUtils.isNoneEmpty(productionCountry)) {
                createTags(tagManager, productionCountry, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 55) {
            String identifiedPersons = resourceProperties[54];
            
            if (StringUtils.isNoneEmpty(identifiedPersons)) {
                createTags(tagManager, identifiedPersons, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 56) {
            String themes = resourceProperties[55];
            
            if (StringUtils.isNoneEmpty(themes)) {
                createTags(tagManager, themes, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 57) {
            String keywords = resourceProperties[56];
            
            if (StringUtils.isNoneEmpty(keywords)) {
                createTags(tagManager, keywords, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 58) {
            String geographicalArea = resourceProperties[57];
            
            if (StringUtils.isNoneEmpty(geographicalArea)) {
                createTags(tagManager, geographicalArea, tagsFailuresList);
            }
        }
        
        if (resourceProperties.length >= 59) {
            String reportCampaign = resourceProperties[58];
            
            if (StringUtils.isNoneEmpty(reportCampaign)) {
                createTags(tagManager, reportCampaign, tagsFailuresList);
            }
        }

        LOGGER.debug("Tags created for media " + resourcePath);
        return  "Tags created for media " + resourcePath;
    }
    
    public String createAssetTags(SlingHttpServletRequest request, String resourcePath, String[] resourceProperties, List<String[]> tagsFailuresList) {
        // req is the SlingHttpServletRequest
        ResourceResolver resourceResolver = request.getResourceResolver();
        TagManager tagManager = resourceResolver.adaptTo(TagManager.class);

        String tags = resourceProperties[42];
        
        if (StringUtils.isNoneEmpty(tags)) {
            createTags(tagManager, tags, tagsFailuresList);
        }
        
        LOGGER.debug("Tags created for asset " + resourcePath);
        return  "Tags created for asset " + resourcePath;
    }
    
    public String createEntityTag(SlingHttpServletRequest request, String tag, List<String[]> tagsFailuresList) {
        // req is the SlingHttpServletRequest
        ResourceResolver resourceResolver = request.getResourceResolver();
        TagManager tagManager = resourceResolver.adaptTo(TagManager.class);
        
        if (StringUtils.isNoneEmpty(tag)) {
            createTags(tagManager, tag, tagsFailuresList);
        }
        
        LOGGER.debug("Entity tag created " + tag);
        return  "Entity tag created " + tag;
    }
    
    protected void createTags(TagManager tagManager, String tags, List<String[]> tagsFailuresList) {
        List<String> tagsList = Arrays.asList(tags.trim().split("\\s*\\|\\s*"));
        
        for (String tagStr : tagsList) {
            if (tagStr.startsWith(QUOTES)) tagStr = tagStr.substring(1, tagStr.length());
            if (tagStr.endsWith(QUOTES)) tagStr = tagStr.substring(0, tagStr.length() - 1);
            
            String fullTagId = getFullTagId(tagStr);
            
            if (null == tagManager.resolve(fullTagId)) {
                Tag mediahub = tagManager.resolve("mediahub:");
                if (null != mediahub) {
                    try {
                        tagManager.createTagByTitle(tagStr);
                    } catch (InvalidTagFormatException e) {
                        LOGGER.error("Error while creating tags", e);
                        tagsFailuresList.add(new String[] {tagStr, e.getMessage()});
                    }
                }
            }
        }
    }
    
    private String getFullTagId(String tagStr) {
        String delimiter = "/";
        String tagPath = tagStr.replace("mediahub:", "");
        String tagIdPath = Arrays.stream(tagPath.split("\\s*\\/\\s*")).map(entity -> JcrUtil.createValidName(entity)).collect(Collectors.joining(delimiter));
        return "mediahub:" + tagIdPath;
    }

    /**
     * To get the media data from excel
     *
     * @param request
     * @return
     * @throws IOException
     */
    private Map<String, String[]> getMediasData(SlingHttpServletRequest request) throws IOException {
        Map<String, String[]> medias = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("medias-file").getInputStream(), StandardCharsets.UTF_8))) {
            CSVParser csvParser = new CSVParser(br);
            
            //ignore header
            String[] values = csvParser.getLine();
            
            while ((values = csvParser.getLine()) != null) {
                //ensure we have the type
                if (values.length >= 43) {
                    //only want to get medias
                    if (values[42] != null && values[42].equals(MEDIA)) {
                        medias.put(values[0], values);                        
                    }
                }
            }
        }
        return medias;
    }
    
    /**
     * To get the asset data from excel
     *
     * @param request
     * @return
     * @throws IOException
     */
    private Map<String, String[]> getAssetsData(SlingHttpServletRequest request) throws IOException {
        Map<String, String[]> assets = new HashMap<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("assets-file").getInputStream(), StandardCharsets.UTF_8))) {
            CSVParser csvParser = new CSVParser(br);
            
            //ignore header
            String[] values = csvParser.getLine();
            
            while ((values = csvParser.getLine()) != null) {
                //ensure we have the type
                if (values.length >= 43) {
                    //only want to get assets
                    assets.put(values[0], values);
                }
            }
        }
        return assets;
    }
    
    /**
     * To get the media data from excel
     *
     * @param request
     * @return
     * @throws IOException
     */
    private List<String> getEntitiesData(SlingHttpServletRequest request) throws IOException {
        List<String> entities = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("entities-file").getInputStream(), StandardCharsets.UTF_8))) {
            
            String value;
            
            while ((value = br.readLine()) != null) {
            	if (StringUtils.isNotBlank(value))
            	entities.add(value.trim());
            }
        }
        return entities;
    }
}
