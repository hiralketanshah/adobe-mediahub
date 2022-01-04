package com.mediahub.core.workflows;

import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.WorkflowProcess;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.SlingJobUtils;
import java.util.Collections;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.JobManager;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Abuthahir Ibrahim
 * <p>
 * Process step for External Activation of Video Asset
 */
@Component(service = WorkflowProcess.class, immediate = true, property = {"process.label=MEDIAHUB : Publish Video Asset to Scene 7 if active"})
public class PublishScene7VideoAsset implements WorkflowProcess {

    private static final Logger log = LoggerFactory.getLogger(PublishScene7VideoAsset.class);

    @Reference
    ResourceResolverFactory resolverFactory;

    @Reference
    JobManager jobManager;

    @Override
    public void execute(WorkItem workItem, WorkflowSession workflowSession, MetaDataMap metaDataMap) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
            String payloadPath = workItem.getWorkflowData().getPayload().toString();
            Asset scene7Video = DamUtil.resolveToAsset(resourceResolver.getResource(payloadPath));
            if (null != scene7Video) {
                String status = scene7Video.getMetadataValue("dam:scene7FileStatus");
                if (status != null && StringUtils.equals(status, "PublishComplete")) {
                    SlingJobUtils.startS7ActivationJob(resourceResolver.getResource(scene7Video.getPath()), jobManager, SlingJobUtils.S7_ACTIVATE_VALUE);
                }
            }
        } catch (Exception e) {
            log.error("Error while Activating Video Asset {0}",e);
        }

    }

}
