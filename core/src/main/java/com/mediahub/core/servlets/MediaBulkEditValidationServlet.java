package com.mediahub.core.servlets;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.dam.commons.util.DamUtil;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ProjectExpireNotificationUtil;
import com.mediahub.core.utils.ResponseUtil;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.servlet.Servlet;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.api.servlets.SlingAllMethodsServlet;
import org.osgi.framework.Constants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SuppressWarnings("CQRules:CQBP-75")
@Component(
		service = Servlet.class,
		property = {
				Constants.SERVICE_DESCRIPTION + "=Create/Update Internal Users",
				"sling.servlet.methods=" + HttpConstants.METHOD_GET,
				"sling.servlet.paths=" + "/bin/media/bulkedit"
})
@ServiceDescription("Bulk Edit of Media")
public class MediaBulkEditValidationServlet extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	protected static final Logger log = LoggerFactory.getLogger(MediaBulkEditValidationServlet.class);

	@Reference
	private transient ResourceResolverFactory resolverFactory;

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response) {
		Map<String, String[]> parameterMap = request.getParameterMap();
		if( !(parameterMap.containsKey("path") && parameterMap.get("path").length > 0) ){
			return;
		}
		final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
				BnpConstants.WRITE_SERVICE);
		try(ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)){

				String action = parameterMap.get(BnpConstants.PATH)[0];
				Map<String, Object> responseMap = new HashMap<>();
				boolean fieldMissed = false;

				if(StringUtils.contains(action, "%2c")){
					String[] paths = action.split("%2c");
					for(String path: paths){
						Resource mediaResource = adminResolver.getResource(path);
						String assetSchema = DamUtil.getInheritedProperty(BnpConstants.METADATA_SCHEMA, mediaResource, "/conf/global/settings/dam/adminui-extension/metadataschema/mediahub-assets-schema");
						List<String> requiredFields = ProjectExpireNotificationUtil.getRequiredMetadataFields(adminResolver, assetSchema);
						fieldMissed = checkChildAssetRequiredFields(responseMap, fieldMissed, mediaResource,
								requiredFields);

						if (fieldMissed) {
							responseMap.put("media",path);
							break;
						}

					}
				}

				if(fieldMissed){
					ResponseUtil.setJsonResponse(400, response, responseMap);
				} else {
					ResponseUtil.setJsonResponse(200, response, responseMap);
				}
		} catch (IOException | LoginException e) {
			log.error("Error while validating media : {0}", e);
		}
	}

	private boolean checkChildAssetRequiredFields(Map<String, Object> responseMap,
			boolean fieldMissed, Resource mediaResource, List<String> requiredFields) {
		if (mediaResource.hasChildren()) {
			Iterator<Resource> children = mediaResource.listChildren();
			while (children.hasNext()) {
				Resource child = children.next();
				fieldMissed = isFieldMissed(fieldMissed, requiredFields, child);
				if (fieldMissed) {
					responseMap.put("asset",child.getPath());
					break;
				}
			}
		}
		return fieldMissed;
	}

	private boolean isFieldMissed(boolean fieldMissed, List<String> requiredFields, Resource child) {
		if (DamUtil.isAsset(child) && child.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA) != null) {
			Map<String, Object> metadata = child.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA).getValueMap();
			for (String field : requiredFields) {
				if (!metadata.containsKey(field)) {
					fieldMissed = true;
					break;
				}
			}
		}
		return fieldMissed;
	}

}

