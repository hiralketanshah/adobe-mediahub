package com.mediahub.core.utils;

import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.resource.ValueMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AssetUtils {

  private AssetUtils(){
    //private constructor to restrict static class
  }

  private final static Logger logger = LoggerFactory.getLogger(AssetUtils.class);

  /**
   * @param properties - Value map object
   * @param key - property key whose value to be obtained
   * @return String array with the value from asset metadata
   */
  public static String[] getBroadcastStatus(ValueMap properties, String key) {
    Object statusValue = properties.get(BnpConstants.BNPP_BROADCAST_STATUS);
    String[] broadcastStatus;
    if(statusValue instanceof  String[]){
      broadcastStatus = (String[])statusValue;
    } else {
      String staus = (String)statusValue;
      broadcastStatus = new String[]{staus};
    }
    return broadcastStatus;
  }
}
