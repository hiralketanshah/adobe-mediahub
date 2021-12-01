package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.mediahub.core.constants.BnpConstants;

import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;

@Component(service = WorkflowProcess.class, immediate = true, property = {
        "process.label=MEDIAHUB : Remove Media To File System" })
@Designate(ocd = RemoveMediaToFileSystemWorkflowProcess.Config.class)
public class RemoveMediaToFileSystemWorkflowProcess implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(RemoveMediaToFileSystemWorkflowProcess.class);

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
            String assetName = payloadPath.substring(payloadPath.lastIndexOf("/") + 1, payloadPath.lastIndexOf('.'));
            removeFile(assetName);
        } catch (LoginException | IOException e) {
            throw new WorkflowException("Error while removing asset", e);
        } finally {
            if (resourceResolver != null && resourceResolver.isLive()) {
                resourceResolver.close();
            }
        }
    }

    private static void removeFile(String assetName) throws IOException {

        File folderFile = new File(destinationPath);
        File entry = new File(folderFile.getAbsolutePath(), assetName);
        if (entry.exists()) {
            File[] files = entry.listFiles();
            for (int i = 0; i < files.length; i++) {
                files[i].delete();
            }
        }
        boolean deleted = entry.delete();
        log.info("File removed : {}",deleted); 
    }

    @ObjectClassDefinition(name = "Mediahub Remove Media To File System", description = "Configuration for removing rich media to file system")
    public static @interface Config {
        @AttributeDefinition(name = "Destination path", description = "Destination path for removing the media")
        String destinationPath() default "";
    }
}
