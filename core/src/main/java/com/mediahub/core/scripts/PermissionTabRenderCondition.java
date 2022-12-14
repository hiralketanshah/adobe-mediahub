package com.mediahub.core.scripts;

import com.adobe.acs.commons.ondeploy.scripts.OnDeployScript;
import com.adobe.acs.commons.ondeploy.scripts.OnDeployScriptBase;
import org.apache.sling.api.resource.Resource;

/**
 * On Deploy Script class for MED-303 for adding render condition in Permission Tab
 */
@SuppressWarnings("CQRules:CQBP-71")
public class PermissionTabRenderCondition extends OnDeployScriptBase implements OnDeployScript {

  @Override
  protected void execute() throws Exception {
      Resource permissionTabRenderCondition = this.getResourceResolver().getResource("/apps/dam/gui/content/assets/v2/foldersharewizard/jcr:content/content/items/form/items/wizard/items/settingStep/items/fixedColumns/items/fixedColumn4/items/tabs/items/permissions-tab/granite:rendercondition");
      if(null != permissionTabRenderCondition){
        Resource destination = this.getResourceResolver().getResource("/apps/dam/temp/mediahub-medias-schema/tabs/items/permissions-tab");
        if(null != destination){
          this.getResourceResolver().copy(permissionTabRenderCondition.getPath(), destination.getPath());
        }
      }
  }
}