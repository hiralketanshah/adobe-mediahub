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
import java.util.GregorianCalendar;
import java.util.List;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.ValueFormatException;
import javax.jcr.lock.LockException;
import javax.jcr.nodetype.ConstraintViolationException;
import javax.jcr.version.VersionException;
import javax.servlet.Servlet;

import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.lang.StringUtils;
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
import com.mediahub.core.constants.BnpConstants;

/**
 * Servlet to fix metadata after running the importation process
 */
@SuppressWarnings("CQRules:CQBP-75")
@Component(service = Servlet.class, property = {"sling.servlet.methods=" + HttpConstants.METHOD_POST, "sling.servlet.paths=" + "/bin/mediahub/updateasset"})
public class MetadataUpdaterServlet extends SlingAllMethodsServlet {

    private static final Logger LOGGER = LoggerFactory.getLogger(MetadataUpdaterServlet.class);

    private static final long serialVersionUID = 1L;
    
    private static final String[] FAILURES_HEADER = { "Path", "Message" };
    private static final String[] ASSET_REQUIRED_FIELDS = { "bnpp-confidentiality", "dc:title", "bnpp-broadcast-status" };
    private static final String ASSETS_FAILURE_FILE = "C:\\bnp\\assetUpdaterFailures.csv";
    
    @Override
    protected void doPost(final SlingHttpServletRequest request, final SlingHttpServletResponse response) throws IOException {
        List<String[]> failuresList = new ArrayList<>();
        failuresList.add(FAILURES_HEADER);
        
        LOGGER.debug("Executing Migration Servlet...");
        List<String> assets = getAssetData(request);
        for (String assetPath : assets) {
            try {
                updateImportedMetadata(request, assetPath);
            } catch (RepositoryException e) {
                LOGGER.error("Could not update asset metadata: " + assetPath, e);
                failuresList.add(new String[] {assetPath, e.getMessage()});
            }
        }
        
        FileWriter fileWriter = new FileWriter(new File(ASSETS_FAILURE_FILE));
        CSVPrinter csvPrinter = new CSVPrinter(fileWriter);
        csvPrinter.println(failuresList.toArray(new String[0][0]));
        fileWriter.flush();
        fileWriter.close();
    }
    
    /**
     * @param request
     * @param assetPath
     * @throws PersistenceException
     * @throws RepositoryException 
     * @throws ConstraintViolationException 
     * @throws LockException 
     * @throws VersionException 
     * @throws ValueFormatException 
     */
    private void updateImportedMetadata(SlingHttpServletRequest request, String assetPath) throws PersistenceException, ValueFormatException, VersionException, LockException, ConstraintViolationException, RepositoryException {
        
        if (StringUtils.isNotEmpty(assetPath)) {
            ResourceResolver resourceResolver = request.getResourceResolver();
            Resource asset;
            
            if (StringUtils.contains(assetPath, "à")) {
                asset = resourceResolver.getResource(StringUtils.replace(assetPath, "à", "�"));
            } else {
                asset = resourceResolver.getResource(assetPath);
            }
            
            if (null != asset && asset.getChild(JcrConstants.JCR_CONTENT) != null) {
                Resource parentMedia = asset.getParent();
                Resource content = asset.getChild(JcrConstants.JCR_CONTENT);
                Resource metadata = content.getChild(BnpConstants.METADATA);

                ModifiableValueMap contentValueMap = metadata.adaptTo(ModifiableValueMap.class);
                String lastModBy = (String) contentValueMap.get(JcrConstants.JCR_LAST_MODIFIED_BY);
                GregorianCalendar lastMod = (GregorianCalendar) contentValueMap.get(JcrConstants.JCR_LASTMODIFIED);

                Node contentNode = content.adaptTo(Node.class);
                contentNode.setProperty(JcrConstants.JCR_LAST_MODIFIED_BY, lastModBy);
                contentNode.setProperty(JcrConstants.JCR_LASTMODIFIED, lastMod);
                
                ModifiableValueMap mediaValueMap = parentMedia.adaptTo(ModifiableValueMap.class);
                contentNode.setProperty("bnpp-usage-rights", (String) mediaValueMap.get("bnpp-usage-rights"));
                contentNode.setProperty("bnpp-usage-end-date", (GregorianCalendar) mediaValueMap.get("bnpp-usage-end-date"));
                contentNode.setProperty("bnpp-usages-territories", (String) mediaValueMap.get("bnpp-usages-territories"));
                
                resourceResolver.commit();
                
                if (isAnyFieldEmpty(contentValueMap, ASSET_REQUIRED_FIELDS)) {
                    throw new ConstraintViolationException("Content is missing required attributes");
                }
            }
            
        }
    }
    
    private boolean isAnyFieldEmpty(ModifiableValueMap content, String ... fields) {
        for (String field : fields) {
            if (StringUtils.isBlank(content.get(field, String.class))) return true;
        }
        
        return false;
    }
    
    /**
     * To get the asset data from excel
     *
     * @param request
     * @return
     * @throws IOException
     */
    private List<String> getAssetData(SlingHttpServletRequest request) throws IOException {
        List<String> assets = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(request.getRequestParameter("file").getInputStream(), StandardCharsets.UTF_8))) {
            CSVParser csvParser = new CSVParser(br);
            
            //ignore header
            String[] values = csvParser.getLine();
            
            while ((values = csvParser.getLine()) != null) {
                //ensure we have the type
                if (values.length >= 0) {
                    //only want to get assets
                    assets.add(values[0]);
                }
            }
        }

        return assets;
    }
}
