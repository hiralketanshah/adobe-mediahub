package com.mediahub.core.listeners;

import static org.mockito.Mockito.when;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChange.ChangeType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith({ AemContextExtension.class })
public class TagListenerTest {

	@InjectMocks
	private TagListener projectsResourceListener = new TagListener();

	private AemContext context;

	@Mock
	private ResourceResolverFactory resourceResolverFactory;

	@Mock
	private ResourceResolver resolver;

	@Mock
	private Resource resource;

	Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
			BnpConstants.WRITE_SERVICE);

	@BeforeEach
	void setup() throws LoginException {
		MockitoAnnotations.initMocks(this);
		context.registerService(ResourceResolverFactory.class, resourceResolverFactory);
		String[] tags = { "test1", "test2" };

		Resource res = context.create().resource("/content/test1", JcrConstants.JCR_PRIMARYTYPE, "dam:Asset");
		context.create().resource("/content/test1/jcr:content", JcrConstants.JCR_PRIMARYTYPE, "nt:unstructured");
		context.create().resource("/content/test1/jcr:content/metadata", JcrConstants.JCR_PRIMARYTYPE,
				"nt:unstructured", "cq:tags", tags);

		when(resourceResolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
		when(resolver.getResource("/content/dam/technique/do-not-delete.png")).thenReturn(res);

	}

	@Test
	public void onEventTest() {

		List<ResourceChange> changedResources = new ArrayList<>();
		ResourceChange change = new ResourceChange(ChangeType.ADDED, "/content/cq:tags/default/parentTag/tag", true);
		changedResources.add(change);
		projectsResourceListener.onChange(changedResources);

	}

}
