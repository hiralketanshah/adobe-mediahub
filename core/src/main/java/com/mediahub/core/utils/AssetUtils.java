package com.mediahub.core.utils;

import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.model.Scene7Asset;
import com.mediahub.core.constants.BnpConstants;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
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

  /**
   *
   * This is used in assetpreview.jsp Kindly check reference before deleting the method.
   *
   * @param s7Config
   * @param scene7Service
   * @param scene7ID
   * @return
   */
  public static String getVideoShareLinkId(S7Config s7Config, Scene7Service scene7Service, String scene7ID) {
    List<Scene7Asset> scene7Assets = scene7Service.getAssets(new String[]{scene7ID}, null, null, s7Config);
    if (scene7Assets != null && !scene7Assets.isEmpty()) {
      Scene7Asset associatedAsset = scene7Service.getAssociatedAssets(scene7Assets.get(0), s7Config);
      if (null != associatedAsset) {
        List<Scene7Asset> subAssets = associatedAsset.getSubAssets();
        for (Scene7Asset asset : subAssets) {
          if (asset != null && asset.getHeight() != null) {
            if (asset.getHeight() == 720L) {
              return asset.getName();
            }
          }
        }
      }
    }
    return StringUtils.EMPTY;
  }
}
