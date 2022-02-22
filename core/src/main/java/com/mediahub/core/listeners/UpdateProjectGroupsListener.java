package com.mediahub.core.listeners;

import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.utils.ProjectPermissionsUtil;
import org.apache.commons.lang3.StringUtils;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.jcr.api.SlingRepository;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.propertytypes.ServiceDescription;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import javax.jcr.observation.ObservationManager;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

@Component(service = EventListener.class, immediate = true)
@ServiceDescription("listen on changes in the /home/groups/projects resource tree")
public class UpdateProjectGroupsListener implements EventListener {

    private ObservationManager observationManager;

    @Reference
    private ResourceResolverFactory resolverFactory;

    @Reference
    private SlingRepository repository;

    private Session session;

    @Reference
    private QueryBuilder builder;

    private static final Logger log = LoggerFactory.getLogger(UpdateProjectGroupsListener.class);

    @Activate
    protected void activate() {
        try {
            session = repository.loginService(BnpConstants.WRITE_SERVICE, null);
            observationManager = session.getWorkspace().getObservationManager();
            observationManager.addEventListener(this, Event.PROPERTY_CHANGED | Event.PROPERTY_ADDED,
                    "/home/groups/projects", true, null, new String[]{"rep:Group"}, false);

        } catch (RepositoryException e) {
            log.error("Error while accessing repository : {}", e);
        }
    }

    @Deactivate
    protected void deactivate() {
        try {
            if (observationManager != null) {
                observationManager.removeEventListener(this);
                log.info("*************removed JCR event listener");
            }
        } catch (RepositoryException re) {
            log.error("*************error removing the JCR event listener ", re);
        } finally {
            if (session != null) {
                session.logout();
                session = null;
            }
        }
    }

    @Override
    public void onEvent(EventIterator events) {

        while (events.hasNext()) {
            Event event = events.nextEvent();
            try {
                String path = event.getPath();
                String propertyName = path.substring(path.lastIndexOf("/") + 1);
                if (propertyName.equalsIgnoreCase("rep:members")) {
                    Node node = session.getNode(path.substring(0, path.lastIndexOf("/")));
                    String name = node.getProperty("rep:principalName").getString();
                    if (name.endsWith("-owner") || name.endsWith("-project-publisher")) {
                        Map<String, String> predicateMapForQuery = ProjectPermissionsUtil.getPredicateMapProjectSearch(name.endsWith("-owner") ? "role_owner" : "role_project-publisher", name);
                        Query query = builder.createQuery(PredicateGroup.create(predicateMapForQuery), session);
                        SearchResult result = query.getResult();
                        Iterator<Resource> projectResources = result.getResources();

                        projectResources.forEachRemaining(project -> {
                            if (StringUtils.equals(project.getValueMap().get(BnpConstants.SLING_RESOURCETYPE, StringUtils.EMPTY), "cq/gui/components/projects/admin/card/projectcard")) {
                                String projectPath = project.getPath();
                                final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
                                try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
                                    ProjectPermissionsUtil.assignMembersToMedialibrary(resourceResolver, projectPath);
                                } catch (Exception e) {
                                    log.error("Unable to access repository", e);
                                }
                            }
                        });
                    }
                }

            } catch (RepositoryException e) {
                log.error("*************error accessing the repository ", e);
            }

        }

    }

}
