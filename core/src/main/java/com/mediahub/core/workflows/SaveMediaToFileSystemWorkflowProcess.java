package com.mediahub.core.workflows;

import com.adobe.granite.asset.api.Asset;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.adobe.granite.workflow.model.WorkflowModel;
import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.zip.ZipOutputStream;

@Component(service = WorkflowProcess.class, immediate = true, property = {
        "process.label=MEDIAHUB : Save Media To File System" })
public class SaveMediaToFileSystemWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(SaveMediaToFileSystemWorkflowProcess.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {
  /*      if (!workItem.getWorkflowData().getPayloadType().equals("JCR_PATH")) {
            throw new WorkflowException("Unable to get the payload");
        }*/

        ResourceResolver resourceResolver = null;
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (payload != null) {
                crunchifyWriteToFile("", payload);
            }
        } catch (LoginException | IOException e) {
            throw new WorkflowException("Error while publishing asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }

    }

    private static void crunchifyWriteToFile(String myData, Resource payload) throws IOException {

        PipedOutputStream pipedOutputStream = new PipedOutputStream();
        PipedInputStream pipedInputStream = new PipedInputStream(pipedOutputStream);


       // DataBufferUtils.write(reqBody, pipedOutputStream).subscribe();
        InputStream userInputStream = payload.adaptTo(Asset.class)
                .getRendition(BnpConstants.ASSET_RENDITION_ORIGINAL).getStream();
        ZipArchiveInputStream zipArchiveInputStream = new ZipArchiveInputStream(userInputStream);

        String baseFolder = "C:\\Users\\hishah\\Desktop";
        File folderFile = new File(baseFolder);
        try {
            ArchiveEntry archiveEntry = getEntry(zipArchiveInputStream);
            while (archiveEntry != null) {
                File entry = new File(folderFile.getAbsolutePath(), archiveEntry.getName());
                entry.getParentFile().mkdirs();
                entry.createNewFile();
                FileOutputStream fileOutputStream = new FileOutputStream(entry);
                byte[] buffer = new byte[4096];
                int read = zipArchiveInputStream.read(buffer);
                while (read != -1) {
                    fileOutputStream.write(buffer, 0, read);
                    read = zipArchiveInputStream.read(buffer);
                }
                
                archiveEntry = getEntry(zipArchiveInputStream);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        
        
    }

    private static ArchiveEntry getEntry(ZipArchiveInputStream zipArchiveInputStream) {
        try {
            return zipArchiveInputStream.getNextEntry();
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

}
