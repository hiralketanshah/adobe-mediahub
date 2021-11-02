package com.mediahub.core.scripts;

import com.adobe.acs.commons.ondeploy.OnDeployScriptProvider;
import com.adobe.acs.commons.ondeploy.scripts.OnDeployScript;
import java.util.Arrays;
import java.util.List;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceDescription;

@Component(service = OnDeployScriptProvider.class,
    immediate = true
)
@ServiceDescription("Developer service that identifies code scripts to execute upon deployment")
public class OnDeployScriptProviderImpl implements OnDeployScriptProvider {

  @Override
  public List<OnDeployScript> getScripts() {
    return Arrays.asList(
        // List of script instances - e.g. new MyScript1(), new MyScript2(), new MyScript3()
        new PermissionTabRenderCondition()
    );
  }
}
