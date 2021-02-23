package com.mediahub.core.listeners;

import com.day.cq.commons.jcr.JcrConstants;
import com.mediahub.core.constants.BnpConstants;
import org.apache.sling.api.SlingConstants;
import org.apache.sling.api.resource.*;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

@Component(service = EventHandler.class,
        immediate = true,
        property = {
                EventConstants.EVENT_TOPIC + "=org/apache/sling/api/resource/Resource/*",
                EventConstants.EVENT_FILTER + "=(path=" + BnpConstants.CONF_FOLDERMETADATASCHEMA + "/*)"
        })
@ServiceDescription("listen on changes in the resource tree")
public class FolderMetadataSchemaListener implements EventHandler {

    public static final String TABS_ITEMS = "/tabs/items";
    public static final String SCHEMA_TABS_ITEMS_WTAB_2 = "/apps/dam/temp/mediahub-medias-schema/tabs/items/wtab2";
    public static final String WTAB_2 = "wtab2";
    private final Logger logger = LoggerFactory.getLogger(getClass());

    @Reference
    ResourceResolverFactory resourceResolverFactory;

    public void handleEvent(final Event event) {
        final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
        Resource schemaHolder = null;
        try (ResourceResolver resolver = resourceResolverFactory.getServiceResourceResolver(authInfo)) {
            String path = event.getProperty(SlingConstants.PROPERTY_PATH).toString();
            if (path.startsWith(BnpConstants.CONF_FOLDERMETADATASCHEMA)) {

                if (path.contains("tabs/")) {
                    path = path.split("tabs/")[0] + BnpConstants.TABS;
                }
                Resource tabs = resolver.getResource(path);
                Resource temporaryPath = getTemporaryResource(resolver);
                schemaHolder = createSchemaFolder(resolver, tabs, temporaryPath);
                copyDynamicAssetSchema(schemaHolder, resolver, tabs);

                Resource wizard = resolver.getResource(BnpConstants.FOLDER_WIZARD_PATH);
                Iterator<Resource> items = wizard.listChildren();
                while (items.hasNext()) {
                    Resource item = items.next();
                    if (schemaHolder.getChild(BnpConstants.TABS).getChild(BnpConstants.ITEMS).getChild(item.getName()) == null) {
                        resolver.copy(item.getPath(), schemaHolder.getChild(BnpConstants.TABS).getChild(BnpConstants.ITEMS).getPath());
                    }

                }
                resolver.commit();
            }
        } catch (LoginException e) {
            logger.error("Error while fetching resource resolver from serviceuser", e);
        } catch (PersistenceException e) {
            logger.error("Error while Saving resource resolver", e);
        } catch (RepositoryException e) {
            logger.error("Error while ordering tab resource", e);
        }
        logger.debug("Resource event: {} at: {}", event.getTopic(), event.getProperty(SlingConstants.PROPERTY_PATH));
    }

    private void copyDynamicAssetSchema(Resource schemaHolder, ResourceResolver resolver,
                                        Resource tabs) throws PersistenceException, RepositoryException {
        if (schemaHolder.getChild("tabs") != null) {
            Iterator<Resource> scheamTabs = tabs.getChild("items").listChildren();
            while (scheamTabs.hasNext()) {
                Resource sourceTab = scheamTabs.next();
                if (resolver.getResource(schemaHolder.getPath() + TABS_ITEMS) != null &&
                        resolver.getResource(schemaHolder.getPath() + TABS_ITEMS).getChild(sourceTab.getName()) != null) {
                    resolver.delete(resolver.getResource(schemaHolder.getPath() + TABS_ITEMS).getChild(sourceTab.getName()));
                    resolver.commit();
                }

                Resource tab = resolver.copy(sourceTab.getPath(), schemaHolder.getPath() + TABS_ITEMS);
                resolver.commit();
                if (resolver.getResource(SCHEMA_TABS_ITEMS_WTAB_2) != null) {
                    tab.adaptTo(Node.class).getParent().orderBefore(tab.getName(), WTAB_2);
                }

            }
            resolver.commit();
        } else {
            resolver.copy(tabs.getPath(), schemaHolder.getPath());
            resolver.commit();
        }
    }

    protected Resource createSchemaFolder(ResourceResolver resolver, Resource tabs,
                                          Resource temporaryPath) throws PersistenceException {
        Resource schemaHolder;
        if (temporaryPath.getChild(tabs.getParent().getParent().getName()) == null) {
            schemaHolder = resolver
                    .create(temporaryPath, tabs.getParent().getParent().getName(), new HashMap<>());
        } else {
            schemaHolder = temporaryPath.getChild(tabs.getParent().getParent().getName());
        }
        resolver.commit();
        return schemaHolder;
    }

    protected Resource getTemporaryResource(ResourceResolver resolver) throws PersistenceException {
        Resource temporaryPath = resolver.getResource(BnpConstants.APPS_DAM);
        if (temporaryPath.getChild(BnpConstants.TEMP) == null) {
            temporaryPath = resolver.create(temporaryPath, BnpConstants.TEMP, Collections.singletonMap(
                    JcrConstants.JCR_PRIMARYTYPE, BnpConstants.SLING_FOLDER));
            resolver.commit();
        } else {
            temporaryPath = temporaryPath.getChild(BnpConstants.TEMP);
        }
        return temporaryPath;
    }


}