package com.mediahub.core.servlets;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ResponseUtil;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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
import org.apache.sling.event.jobs.JobManager;
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
				"sling.servlet.paths=" + "/bin/media/bulkunpublish"
})
@ServiceDescription("Bulk Unpublish of Media")
public class MediaBulkUnpublishServlet extends SlingAllMethodsServlet {
	private static final long serialVersionUID = 8565934343267006374L;

	protected static final Logger log = LoggerFactory.getLogger(MediaBulkUnpublishServlet.class);

	@Reference
	private transient ResourceResolverFactory resolverFactory;

	@Reference
	private transient JobManager jobManager;

	@Override
	protected void doGet(SlingHttpServletRequest request, SlingHttpServletResponse response)
			throws IOException {
		Map<String, String[]> parameterMap = request.getParameterMap();
		if( !(parameterMap.containsKey(BnpConstants.PATH) && parameterMap.get(BnpConstants.PATH).length > 0) ){
			return;
		}
		final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
				BnpConstants.WRITE_SERVICE);
		Map<String, Object> responseMap = new HashMap<>();
		try(ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)){
				String action = parameterMap.get(BnpConstants.PATH)[0];
				if(StringUtils.contains(action, "%2c")){
					String[] paths = action.split("%2c");
					for(String path: paths){
						startUnpublishJob(adminResolver, path);
					}
				}
				responseMap.put("message", "The Assets will be deacitivated in sometime");
				ResponseUtil.setJsonResponse(200, response, responseMap);
		} catch (IOException | LoginException e) {
			log.error("Error while unpublishing assets inside media : {0}", e);
			responseMap.put("message", e.getMessage());
			ResponseUtil.setJsonResponse(400, response, responseMap);
		}
	}

	private void startUnpublishJob(ResourceResolver adminResolver, String path) {
		Resource mediaResource = adminResolver.getResource(path);
		if (mediaResource.hasChildren()) {
			Iterator<Resource> children = mediaResource.listChildren();
			while (children.hasNext()) {
				Resource child = children.next();
				final Map<String, Object> properties = new HashMap<>();
				properties.put("offloading.input.payload",child.getPath());
				jobManager.addJob("unpublish/media/folder", properties);
			}
		}
	}





}

