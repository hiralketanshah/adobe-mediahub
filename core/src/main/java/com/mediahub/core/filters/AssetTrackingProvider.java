package com.mediahub.core.filters;

import com.day.cq.commons.jcr.JcrConstants;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.AnalyticsTrackingService;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.wrappers.ValueMapDecorator;
import org.apache.sling.spi.resource.provider.ResolveContext;
import org.apache.sling.spi.resource.provider.ResourceContext;
import org.apache.sling.spi.resource.provider.ResourceProvider;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jcr.Session;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import static com.mediahub.core.constants.BnpConstants.*;

/**
 * Catch tracking URLs with the following pattern https://domain/mh/STATUS/FORMAT/UUID
 * <p>
 * Where STATUS has the value 'external' or 'internal'
 * Where FORMAT has the value 'player' or 'master' or 'hd' or 'md'
 * Where UUID is the JCR property jcr:uuid of the requested asset
 */
@Component(
        service = ResourceProvider.class,
        property = {
                ResourceProvider.PROPERTY_NAME + "=mediahub.asset-tracking-provider",
                ResourceProvider.PROPERTY_ROOT + "=" + AssetTrackingProvider.ROOT,
        },
        immediate = true
)
public class AssetTrackingProvider extends ResourceProvider<Object> {

    public static final String ROOT = "/mh";

    private static final Logger log = LoggerFactory.getLogger(AssetTrackingProvider.class);

    @Reference
    private AnalyticsTrackingService trackingService;

    @Override
    public Resource getResource(final ResolveContext<Object> resolveContext,
                                final String path,
                                final ResourceContext resourceContext,
                                final Resource parentResource) {

        final ResourceResolver resourceResolver = resolveContext.getResourceResolver();

        String[] segments = path.split("/");

        Resource asset = null;
        if (segments.length == 5) {
            String uuid = segments[4];
            String status = segments[2];
            String format = segments[3];

            QueryBuilder builder = resourceResolver.adaptTo(QueryBuilder.class);
            Map<String, String> map = new HashMap<>();
            map.put(BnpConstants.PATH, "/content/dam/medialibrary");
            map.put(BnpConstants.FIRST_PROPERTY, JcrConstants.JCR_UUID);
            map.put(BnpConstants.FIRST_PROPERTY_VALUE, uuid);
            map.put("p.limit", "-1");

            Query query = builder.createQuery(PredicateGroup.create(map), resourceResolver.adaptTo(Session.class));
            SearchResult result = query.getResult();
            Iterator<Resource> userResources = result.getResources();

            Map<String, String> globalProperties = GlobalFilter.getGlobalProperties();

            if (userResources.hasNext()) {
                asset = userResources.next();
                Resource metadata = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
                String[] broadcastStatus = (String[]) metadata.getValueMap().get(BnpConstants.BNPP_BROADCAST_STATUS);
                try {
                    if (metadata != null && metadata.getValueMap().get(BnpConstants.BNPP_BROADCAST_STATUS) != null) {
                        switch (status) {
                            case BnpConstants.BROADCAST_VALUE_EXTERNAL:
                                if (Arrays.asList(broadcastStatus).contains(BnpConstants.BROADCAST_VALUE_EXTERNAL)) {
                                    trackingService.trackExternal(asset, format, globalProperties);
                                    if (S7_FILE_STATUS_NOT_SUPPORTED.equals(metadata.getValueMap().get(S7_FILE_STATUS_PROPERTY, String.class))) {
                                        return processInternalUrl(asset, path, format);
                                    } else {
                                        return processExternalUrl(asset, path, format);
                                    }
                                }
                                break;
                            case BnpConstants.BROADCAST_VALUE_INTERNAL:
                                if (Arrays.asList(broadcastStatus).contains(BnpConstants.BROADCAST_VALUE_INTERNAL)) {
                                    trackingService.trackInternal(asset, format, globalProperties);
                                    return processInternalUrl(asset, path, format);
                                }
                                break;
                        }
                    }

                } catch (Exception e) {
                    log.error("Error when processing asset", e);
                }
            }

        }


        // Note that ResourceMetadata is NOT the data that populates a resources ValueMap; that is done below via the ProvidedResourceWrapper
        ResourceMetadata resourceMetaData = new ResourceMetadata();
        // Set the resolution path
        resourceMetaData.setResolutionPath(path);
        return new SyntheticResource(resourceResolver, path, "sling:Folder");


    }

    @Override
    public Iterator<Resource> listChildren(ResolveContext<Object> resolveContext, Resource resource) {
        // unwrap resource if it is a wrapped resource
        final Resource currentResource;
        if (resource instanceof ResourceWrapper) {
            currentResource = ((ResourceWrapper) resource).getResource();
        } else {
            currentResource = resource;
        }

        // delegate resource listing to resource resolver
        if (currentResource instanceof InternalResource) {
            final InternalResource res = (InternalResource) currentResource;
            final ResourceResolver resolver = res.getResource().getResourceResolver();
            return resolver.listChildren(res.getResource());
        }
        return null;
    }

    private Resource processExternalUrl(Resource asset, String path, String format) {
        Resource metadata = asset.getChild(JcrConstants.JCR_CONTENT).getChild(BnpConstants.METADATA);
        String externalUrl = null;
        switch (format) {
            case "player":
                externalUrl = metadata.getValueMap().get(BNPP_EXTERNAL_BROADCAST_URL, String.class);
                break;
            case "master":
                externalUrl = metadata.getValueMap().get(BNPP_EXTERNAL_FILE_URL, String.class);
                break;
            case "hd":
                externalUrl = metadata.getValueMap().get(BNPP_EXTERNAL_FILE_URL_HD, String.class);
                break;
            case "md":
                externalUrl = metadata.getValueMap().get(BNPP_EXTERNAL_FILE_URL_MD, String.class);
                break;
        }

        ValueMap vm = new ValueMapDecorator(new HashMap<>());
        vm.put("redirectTarget", externalUrl);
        return new ExternalResource(asset, path, vm);

    }

    private Resource processInternalUrl(Resource asset, String path, String format) {
        return new InternalResource(asset, asset.getPath());

    }

}