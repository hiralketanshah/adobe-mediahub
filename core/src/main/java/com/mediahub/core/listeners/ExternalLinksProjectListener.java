package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;

import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.PersistenceException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.api.resource.ValueMap;
import org.apache.sling.jcr.resource.api.JcrResourceConstants;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.AttributeType;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component(service = EventHandler.class, immediate = true, property = {
        EventConstants.EVENT_TOPIC + "=" + BnpConstants.TOPIC_RESOURCE_ADDED,
        EventConstants.EVENT_FILTER + "=(path=/content/projects/*/jcr:content/dashboard/gadgets/externallinks)" })
@ServiceDescription("listens on addition of external links tile for project")
@Designate(ocd = ExternalLinksProjectListener.SchedulerConfig.class)
public class ExternalLinksProjectListener implements EventHandler {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    ResourceResolverFactory resourceResolverFactory;

    String defaultLinkPath;
    private static final String genericListName = "external-links";
    private static final String genericListParentPath = "/etc/acs-commons/lists";

    @Activate
    protected void activate(ExternalLinksProjectListener.SchedulerConfig config) {
        this.defaultLinkPath = config.defaultLinkPath();

    }

    public void handleEvent(final Event event) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
                BnpConstants.WRITE_SERVICE);
        ResourceResolver resolver = null;

        try {
            resolver = resourceResolverFactory.getServiceResourceResolver(authInfo);
            Session session = resolver.adaptTo(Session.class);
            String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();
            Resource contentResourse;
            contentResourse = resolver.getResource(path);
            if (contentResourse != null) {
                String genericListForLinks = getGenericListPath(resolver, path);
                Map<String, String> defaultLinks = getDefaultLinks(resolver, genericListForLinks);
                for (Map.Entry<String, String> link : defaultLinks.entrySet()) {
                    addDefaultLink(session, path, link);
                }
            }
            if (resolver.hasChanges()) {
                resolver.commit();
            }
        } catch (LoginException e) {
            logger.error("Error while Creating resource resolver {}", e.getMessage());
        } catch (PersistenceException e) {
            logger.error("Error while saving the resource {}", e.getMessage());
        } finally {
            if (null != resolver) {
                resolver.close();
            }
        }
    }

    private String getGenericListPath(ResourceResolver resolver, String path) {
        String genericListPath = null;
        String resourcePath = genericListParentPath + path.split("/" + JcrConstants.JCR_CONTENT)[0];
        Resource listPath = resolver.getResource(resourcePath);
        boolean hasGenericList = false;
        while (!hasGenericList) {
            if (resourcePath.equalsIgnoreCase(genericListParentPath)) {
                break;
            }
            if (null != listPath && null != listPath.getChild(genericListName)) {
                hasGenericList = true;
            } else {
                resourcePath = resourcePath.substring(0, resourcePath.lastIndexOf("/"));
                listPath = resolver.getResource(resourcePath);
            }
        }
        if (!hasGenericList) {
            genericListPath = defaultLinkPath;
        } else {
            genericListPath = listPath.getChild(genericListName).getPath();
        }
        return genericListPath;
    }

    private void addDefaultLink(Session session, String path, Entry<String, String> link) {
        Node node;
        try {
            node = session.getNode(path);
            Node externalLink = node.addNode(getNodeName(link.getKey()), JcrConstants.NT_UNSTRUCTURED);
            externalLink.setProperty(JcrConstants.JCR_TITLE, link.getKey());
            externalLink.setProperty("consolePath", link.getValue());
            externalLink.setProperty("linkParent", path);
            externalLink.setProperty("name", "External");
            externalLink.setProperty("target", "_blank");
            externalLink.setProperty("class", "card-article");
            externalLink.setProperty(JcrResourceConstants.SLING_RESOURCE_TYPE_PROPERTY, "cq/gui/components/projects/admin/card/linkcard");
            session.save();

        } catch (RepositoryException e) {
            logger.error("Error while accessing the resource at {} : {}", path, e.getMessage());
        }

    }

    private String getNodeName(String key) {
        key = key.replace(' ', '-').toLowerCase();
        return key;
    }

    private Map<String, String> getDefaultLinks(ResourceResolver resolver, String genericListForLinks) {

        Resource genericListResource = resolver.getResource(genericListForLinks);
        Map<String, String> mapOfDefaultLinks = new HashMap<>();
        if (null != genericListResource) {
            Resource defaultLinksResource = genericListResource.getChild("jcr:content/list");
            if (null != defaultLinksResource) {
                Iterator<Resource> listofLink = defaultLinksResource.listChildren();
                while (listofLink.hasNext()) {
                    Resource linkResource = listofLink.next();
                    ValueMap properties = linkResource.adaptTo(ValueMap.class);
                    mapOfDefaultLinks.put(properties.get(JcrConstants.JCR_TITLE, String.class),
                            properties.get("value", String.class));
                }
            }
        }
        return mapOfDefaultLinks;

    }

    @ObjectClassDefinition(name = "MediaHub path for project's default external list of links", description = "MediaHub path for project's default external list of links")
    public @interface SchedulerConfig {

        @AttributeDefinition(name = "Generic list path for default links", description = "Generic list path for default links", type = AttributeType.STRING)
        public String defaultLinkPath() default "/etc/acs-commons/lists/external-links";
    }

}
