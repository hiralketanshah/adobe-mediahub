package com.mediahub.core.workflows;

import com.adobe.granite.workflow.PayloadMap;
import com.adobe.granite.workflow.WorkflowException;
import com.adobe.granite.workflow.WorkflowSession;
import com.adobe.granite.workflow.exec.WorkItem;
import com.adobe.granite.workflow.exec.Workflow;
import com.adobe.granite.workflow.exec.WorkflowData;
import com.adobe.granite.workflow.metadata.MetaDataMap;
import com.day.cq.dam.api.Asset;
import com.day.cq.dam.api.Rendition;
import com.day.image.Layer;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Method;

/**
 * @author arthur bamy
 */
public class WorkflowUtils {

    private static final Logger log = LoggerFactory.getLogger(MoveAssetsProcessWorkflow.class);

    public static void updateWorkflowPayload(WorkItem item, WorkflowSession wfSession, String contentPath) throws WorkflowException {
        log.info("Asset : {}. updateWorkflowPayload contentPath", contentPath);
        WorkflowData data = cloneWorkflowDataWithNewPayload(wfSession, contentPath, item.getWorkflowData());
        wfSession.updateWorkflowData(item.getWorkflow(), data);

        // updateWorkflowData does not update the in-memory workflow data
        // however changing this behaviour in WorkflowSession has to big of an
        // impact, so using reflection to updating the in-memory copy.
        // Without updating the in-memory copy the next workflow step resets to
        // the original payload data.
        try {
            Workflow instance = item.getWorkflow();
            Method method = instance.getClass().getMethod("setWorkflowData", WorkflowData.class);
            if (method != null) {
                method.invoke(instance, data);
            }
        } catch (Exception e) {
            throw new WorkflowException("failed to update in-memory payload", e);
        }

    }

    public static WorkflowData cloneWorkflowDataWithNewPayload(WorkflowSession wfSession, String contentPath, WorkflowData wfToClone) {
        WorkflowData result = wfSession.newWorkflowData(PayloadMap.TYPE_JCR_PATH, contentPath);
        MetaDataMap sourceMap = wfToClone.getMetaDataMap();
        MetaDataMap targetMap = result.getMetaDataMap();
        for (String key : sourceMap.keySet()) {
            targetMap.put(key, sourceMap.get(key));
        }

        return result;
    }

}
