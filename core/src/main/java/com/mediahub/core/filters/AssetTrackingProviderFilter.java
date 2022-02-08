package com.mediahub.core.filters;

import java.io.IOException;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.servlets.HttpConstants;
import org.apache.sling.engine.EngineConstants;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.propertytypes.ServiceRanking;
import org.osgi.service.component.propertytypes.ServiceVendor;

@Component(service = Filter.class, property = {
		EngineConstants.SLING_FILTER_SCOPE + "=" + EngineConstants.FILTER_SCOPE_REQUEST,
		EngineConstants.SLING_FILTER_PATTERN + "=" + "/mh/internal/.*",
		EngineConstants.SLING_FILTER_METHODS + "=" + HttpConstants.METHOD_GET })
@ServiceRanking(-700)
@ServiceVendor("Adobe")
public class AssetTrackingProviderFilter implements Filter {

	private static final ThreadLocal<String> threadLocal = new ThreadLocal<>();

	@Override
	public void doFilter(final ServletRequest request, final ServletResponse response, final FilterChain filterChain)
			throws IOException, ServletException {
		try {
			final SlingHttpServletRequest slingRequest = (SlingHttpServletRequest) request;
			final SlingHttpServletResponse slingResponse = (SlingHttpServletResponse) response;
			// Changes as per MED-513 to avoid file download
			//slingResponse.setHeader("Content-disposition", "attachment; filename=" + threadLocal.get());
			filterChain.doFilter(slingRequest, slingResponse);
		} finally {
			remove();
		}
	}

	@Override
	public void destroy() {
	}

	@Override
	public void init(FilterConfig arg0) throws ServletException {
	}

	public static void set(String filename) {
		threadLocal.set(filename);
	}

	public static void remove() {
		threadLocal.remove();
	}
}