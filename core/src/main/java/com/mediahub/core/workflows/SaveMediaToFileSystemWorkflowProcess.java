package com.mediahub.core.workflows;

import com.adobe.granite.asset.api.Rendition;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;

@SuppressWarnings("findsecbugs:PATH_TRAVERSAL_IN")
@Component(service = WorkflowProcess.class, immediate = true, property = {
        "process.label=MEDIAHUB : Save Media To File System"})
@Designate(ocd = SaveMediaToFileSystemWorkflowProcess.Config.class)
public class SaveMediaToFileSystemWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(SaveMediaToFileSystemWorkflowProcess.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    private static String destinationPath;

    @Activate
    protected void activate(Config config) {
        this.destinationPath = config.destinationPath();
    }

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap)
            throws WorkflowException {
        ResourceResolver resourceResolver = null;
        try {
            final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                    BnpConstants.WRITE_SERVICE);
            resourceResolver = resolverFactory.getServiceResourceResolver(authInfo);
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (null != payload && StringUtils.isNotBlank(destinationPath)) {
                Resource resourceAsset = payload.getParent().getParent();
                Resource metadata = resourceAsset.getChild(BnpConstants.METADATA);
                ValueMap metadataProperties = metadata.adaptTo(ValueMap.class);
                if (metadataProperties.containsKey("bnpp-rich-media")
                        && metadataProperties.get("bnpp-rich-media", String.class).equalsIgnoreCase("true")) {
                    crunchifyWriteToFile(payload);
                }

            } else {
                log.info("Either payload or destination path is empty");
            }
        } catch (LoginException | IOException e) {
            throw new WorkflowException("Error while moving rich media asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }
    }

    private static void crunchifyWriteToFile(Resource payload) throws IOException {

        InputStream userInputStream = payload.adaptTo(Rendition.class).getStream();
        ZipArchiveInputStream zipArchiveInputStream = new ZipArchiveInputStream(userInputStream);
        File folderFile = new File(destinationPath);
        try {
            ArchiveEntry archiveEntry = getEntry(zipArchiveInputStream);
            while (archiveEntry != null) {
                File entry = new File(folderFile.getAbsolutePath(), archiveEntry.getName());
                if (entry.exists()) {
                    File parent = new File(entry.getParent());
                    log.info(parent.getAbsolutePath());
                    File[] files = parent.listFiles();
                    for (int i = 0; i < files.length; i++) {
                        files[i].delete();
                    }
                }
                entry.getParentFile().mkdirs();
                entry.createNewFile();
                FileOutputStream fileOutputStream = new FileOutputStream(entry);
                byte[] buffer = new byte[4096];
                int read = zipArchiveInputStream.read(buffer);
                while (read != -1) {
                    fileOutputStream.write(buffer, 0, read);
                    read = zipArchiveInputStream.read(buffer);
                }
                fileOutputStream.close();
                archiveEntry = getEntry(zipArchiveInputStream);
            }
        } catch (IOException e) {
            log.error("Exception while extracting zip file and storing into file system, {}", e);
            throw new RuntimeException(e);
        }
    }

    private static ArchiveEntry getEntry(ZipArchiveInputStream zipArchiveInputStream) {
        try {
            return zipArchiveInputStream.getNextEntry();
        } catch (IOException e) {
            log.error("Exception while extracting zip file, {}", e);
            throw new RuntimeException(e);
        }
    }

    @ObjectClassDefinition(name = "Mediahub Save Media To File System", description = "Configuration for sending rich media to file system")
    public static @interface Config {
        @AttributeDefinition(name = "Destination path", description = "Destination path for storing the media")
        String destinationPath() default "";
    }
}
