package com.mediahub.core.utils;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.replication.ReplicationActionType;
import com.day.cq.replication.ReplicationOptions;
import com.day.cq.replication.Replicator;
import com.mediahub.core.constants.BnpConstants;
import javax.jcr.Session;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReplicationUtils {

  private static Logger log = LoggerFactory.getLogger(ReplicationUtils.class);

  private ReplicationUtils() {
    // private Constructor
  }

  /**
   * Replicate the playload using the Replication API
   *
   * @param path
   * @param resourceResolver
   * @param replicator
   */
  public static void replicationConent(String path,  ResourceResolver resourceResolver, Replicator replicator)
  {
    try {
      // Create leanest replication options for activation
      ReplicationOptions options = new ReplicationOptions();
      // Do not create new versions as this adds to overhead
      options.setSuppressVersions(true);
      // Avoid sling job overhead by forcing synchronous. Note this will result in serial activation.
      options.setSynchronous(true);
      // Do NOT suppress status update of resource (set replication properties accordingly)
      options.setSuppressStatusUpdate(false);

      log.debug("**** ABOUT TO REPLICATE" ) ;
      //Rep the content   replicate(Session session, ReplicationActionType type, String path)
      replicator.replicate(resourceResolver.adaptTo(Session.class), ReplicationActionType.ACTIVATE,path);
      log.debug("**** REPLICATED" ) ;
    } catch(Exception e) {
      log.error("**** Error while replicating Node : {} ", e.getMessage()) ;
    }
  }

  /**
   *Replicating parent Metadata and Jcr content
   *
   * @param resourceResolver
   * @param movedAsset
   * @param replicator
   */
  public static void replicateParentMetadata(ResourceResolver resourceResolver, Resource movedAsset, Replicator replicator) {
    if(movedAsset.getParent().getChild(JcrConstants.JCR_CONTENT) != null){
      ReplicationUtils.replicationConent(movedAsset.getParent().getChild(JcrConstants.JCR_CONTENT).getPath(), resourceResolver, replicator);
      if(movedAsset.getParent().getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null ){
        ReplicationUtils.replicationConent(movedAsset.getParent().getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getPath(), resourceResolver, replicator);
      }
    }
  }

}
