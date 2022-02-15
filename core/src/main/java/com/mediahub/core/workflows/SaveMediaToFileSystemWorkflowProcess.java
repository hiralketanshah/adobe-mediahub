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
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
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

        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Resource payload = resourceResolver.getResource(payloadPath);
            if (null != payload && StringUtils.isNotBlank(destinationPath)) {
                Resource resourceAsset = payload.getParent().getParent();
                ValueMap properties = resourceAsset.getParent().getValueMap();
                Resource metadata = resourceAsset.getChild(BnpConstants.METADATA);
                ValueMap metadataProperties = metadata.adaptTo(ValueMap.class);
                if (metadataProperties.containsKey("bnpp-rich-media")
                        && metadataProperties.get("bnpp-rich-media", String.class).equalsIgnoreCase("true")) {
                    crunchifyWriteToFile(payload, properties);
                }
            } else {
                log.info("Either payload or destination path is empty");
            }
        } catch (LoginException | IOException e) {
            throw new WorkflowException("Error while moving rich media asset", e);
        }
    }

    @SuppressWarnings("squid:S899")
    private static void crunchifyWriteToFile(Resource payload, ValueMap properties) throws IOException {

        InputStream userInputStream = payload.adaptTo(Rendition.class).getStream();
        ZipArchiveInputStream zipArchiveInputStream = new ZipArchiveInputStream(userInputStream);
        File folderFile = new File(destinationPath);
        ArchiveEntry archiveEntry = getEntry(zipArchiveInputStream);
        while (archiveEntry != null) {
            String archiveEntryName = getName(archiveEntry.getName(), properties);
            
            File entry = new File(folderFile.getAbsolutePath(), archiveEntryName);
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
            try (FileOutputStream fileOutputStream = new FileOutputStream(entry)) {
                byte[] buffer = new byte[4096];
                int read = zipArchiveInputStream.read(buffer);
                while (read != -1) {
                    fileOutputStream.write(buffer, 0, read);
                    read = zipArchiveInputStream.read(buffer);
                }
            }
            archiveEntry = getEntry(zipArchiveInputStream);
        }
    }

    private static String getName(String archiveEntryName, ValueMap properties) {
        System.out.println(properties);
        if(-1 != archiveEntryName.indexOf("/") && properties.containsKey("jcr:uuid")) {
            String uuid = properties.get("jcr:uuid", String.class);
            archiveEntryName = archiveEntryName.substring(archiveEntryName.indexOf("/"), archiveEntryName.length());
            return uuid.concat(archiveEntryName);
        }
        return archiveEntryName;
    }

    private static ArchiveEntry getEntry(ZipArchiveInputStream zipArchiveInputStream) throws IOException {
        try {
            return zipArchiveInputStream.getNextEntry();
        } catch (IOException e) {
            log.error("Exception while extracting zip file, {}", e);
            throw new IOException(e);
        }
    }

    @ObjectClassDefinition(name = "Mediahub Save Media To File System", description = "Configuration for sending rich media to file system")
    public static @interface Config {
        @AttributeDefinition(name = "Destination path", description = "Destination path for storing the media")
        String destinationPath() default "/";
    }
}
