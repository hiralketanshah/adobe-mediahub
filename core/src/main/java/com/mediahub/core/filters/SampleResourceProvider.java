package com.mediahub.core.filters;

import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.*;
import org.apache.sling.api.wrappers.ValueMapDecorator;
import org.apache.sling.spi.resource.provider.ResolveContext;
import org.apache.sling.spi.resource.provider.ResourceContext;
import org.apache.sling.spi.resource.provider.ResourceProvider;
import org.osgi.service.component.annotations.Component;

import java.util.*;

@Component(
        property = {
                ResourceProvider.PROPERTY_NAME + "=acs-aem-sample.sample-resource-provider",
                ResourceProvider.PROPERTY_ROOT + "=" + SampleResourceProvider.ROOT,
                "provider.roots=" + SampleResourceProvider.ROOT,
                ResourceProvider.PROPERTY_REFRESHABLE + "=true"
        },
        immediate = true
)
public class SampleResourceProvider extends ResourceProvider<Object> {

    public static final String ROOT = "/numbers";

    private static final int MIN_NUMBER = 0;
    private static final int MAX_NUMBER = 100;

    @Override
    public Resource getResource(final ResolveContext<Object> resolveContext,
                                final String path,
                                final ResourceContext resourceContext,
                                final Resource parentResource) {

        final ResourceResolver resourceResolver = resolveContext.getResourceResolver();

        // Make getResource() return as fast as possible!
        // Return null early if getResource() cannot/should not process the resource request

        // If path is a root, return a Synthetic Sling Folder
        // This could be any "type" of SyntheticResource
        if (ROOT.equals(path)) {
            return new SyntheticResource(resourceResolver, path, "sling:Folder");
        }

        // Parse the path to figure out what it "resolves" to in the custom implementation
        // In this case well look for integer value at the end of a path formatted like: /numbers/number-###
        String numberStr = StringUtils.substringAfter(path, ROOT + "/number-");
        int number;
        try {
            number = Integer.parseInt(numberStr);
            // Do some checking to make sure the path make sense for the provider
            if (number < MIN_NUMBER || number > MAX_NUMBER) {
                // If it doesnt, return null
                return null;
            }
        } catch (NumberFormatException e) {
            return null;
        }

        // Note that ResourceMetadata is NOT the data that populates a resources ValueMap; that is done below via the ProvidedResourceWrapper
        ResourceMetadata resourceMetaData = new ResourceMetadata();

        // Set the resolution path
        resourceMetaData.setResolutionPath(path);

        // This resourceType is completely customizable
        // Often it is set in the OSGi Properties if the value is fixed for all resources this provider returns
        // It is important to ensure that any scripts associated w this resourceType stay in the Sling APIs and
        // do not drop down to the JCR Node APIs as this synthetic resource is a Sling abstraction and the JCR APIs
        // will see it as an invalid path/resource.
        final String resourceType = "acs-samples/content/number";

        // Create the synthetic resource
        final Resource numberResource = new SyntheticResource(resourceResolver, resourceMetaData, resourceType);

        // Make a call to some other system using the path/request and resolve the data to return on the "Provided" resource's ValueMap
        final Map<String, Object> properties = new HashMap<>();

        // Mocking some data that represents this resource
        properties.put("sampleData", "This is sample data for the number " + number);
        properties.put("gotAt", new Date());
        properties.put("meaningOfLife", 42);

        // Add the properties for this resource by wrapping the synthetic resource with a ResourceWrapper (defined below)
        // that exposes a custom ValueMap via this resources .adaptTo(ValueMap.class)
        return new ProvidedResourceWrapper(numberResource, properties);
    }

    @Override
    public Iterator<Resource> listChildren(final ResolveContext<Object> resolveContext, final Resource parentResource) {
        final List<Resource> numberResources = new ArrayList<Resource>();

        // This example will only list children of the registered root
        if (!ROOT.equals(parentResource.getPath())) {
            return null;
        }

        // Collect some resources to list - this is often from a third party system.
        // In this case well generate a list of resources that represent numbers get and create a list of resources in a similar fashion as in getResource
        for (int i = 0; i <= 100; i++) {
            ResourceMetadata resourceMetaData = new ResourceMetadata();

            // Create the "path" for this resource; this pathing scheme must be compatible with getResource(..)
            resourceMetaData.setResolutionPath("/content/numbers/number-" + i);
            resourceMetaData.put("index", String.valueOf(i));

            // This resourceType is completely customizable
            // Often it is set in the OSGi Properties if the value is fixed for all resources this provider returns
            // It is important to ensure that any scripts associated w this resourceType stay in the Sling APIs and
            // do not drop down to the JCR Node APIs as this synthetic resource is a Sling abstraction and the JCR APIs
            // will see it as an invalid path/resource.
            final String resourceType = "acs-samples/content/number";

            // Create the synthetic resource
            final Resource numberResource = new SyntheticResource(resolveContext.getResourceResolver(), resourceMetaData, resourceType);

            // Create a ValueMap representation of this resource, this might come from a 3rd party system
            final Map<String, Object> properties = new HashMap<String, Object>();

            // Mocking some data that represents this resource
            properties.put("sampleData", "This is sample data for the number " + i);
            properties.put("listedAt", new Date());
            properties.put("meaningOfLife", 42);

            // Add the properties for this resource by wrapping the synthetic resource with a ResourceWrapper (defined below)
            // that exposes a custom ValueMap via this resources .adaptTo(ValueMap.class)
            numberResources.add(new ProvidedResourceWrapper(numberResource, properties));
        }

        return numberResources.iterator();
    }


    /**
     * Custom Resource Wrapper that is used to expose a custom ValueMap via the "Provided" resource's .adaptTo(ValueMap.class);
     */
    private class ProvidedResourceWrapper extends ResourceWrapper {
        private final ValueMap properties;

        public ProvidedResourceWrapper(Resource resource, Map<String, Object> properties) {
            super(resource);
            this.properties = new ValueMapDecorator(properties);
        }

        @Override
        public final <AdapterType> AdapterType adaptTo(Class<AdapterType> type) {
            if (type != ValueMap.class) {
                return super.adaptTo(type);
            }

            // Return the ValueMap of the properties passed in
            return (AdapterType) this.properties;
        }
    }
}