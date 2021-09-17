package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.tagging.TagConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.resource.observation.ResourceChange;
import org.apache.sling.api.resource.observation.ResourceChangeListener;
import org.osgi.service.cm.ConfigurationAdmin;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Session;
import java.security.Principal;
import java.util.*;

/**
 * @author Shipra Arora
 * <p>
 * Listener class to provide read permissions to the parent project folders until projects.
 */

@Component(service = {
        ResourceChangeListener.class},
        immediate = true,
        property = {
                ResourceChangeListener.CHANGES + "=ADDED",
                ResourceChangeListener.PATHS + "=glob:" + TagListener.DEFAULT_TAGS + "**"// for handling custom invalidation for AF, AFF
        }
)

@ServiceDescription("listen on changes in the resource tree")
public class TagListener implements ResourceChangeListener {

    public final static String DEFAULT_TAGS = "/content/cq:tags/default/";

    private final Logger log = LoggerFactory.getLogger(getClass());

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Override
    public void onChange(List<ResourceChange> arg0) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        try (ResourceResolver adminResolver = resolverFactory.getServiceResourceResolver(authInfo)) {

            Resource masterAsset = adminResolver.getResource("/content/dam/technique/do-not-delete.png");
            // getting session of System User
            for (ResourceChange my : arg0) {
                String tagPath = my.getPath().substring(DEFAULT_TAGS.length());
                if (tagPath.split("/").length == 2) {

                    Resource contentResourse = masterAsset.getChild(JcrConstants.JCR_CONTENT);
                    if (contentResourse != null) {
                        Resource metadata = contentResourse.getChild(BnpConstants.METADATA);
                        ModifiableValueMap mvp = metadata.adaptTo(ModifiableValueMap.class);
                        List<String> tags;
                        if (mvp.containsKey(TagConstants.PN_TAGS)) {
                            tags = new ArrayList<>(Arrays.asList((String[]) mvp.get(TagConstants.PN_TAGS)));
                        } else {
                            tags = new ArrayList<>();
                        }
                        tags.add(tagPath);
                        mvp.put(TagConstants.PN_TAGS, tags.toArray());
                        adminResolver.commit();
                    }
                }


            }
        } catch (LoginException | PersistenceException e) {
            log.error("RepositoryException while Executing events", e);
        }

    }
}
