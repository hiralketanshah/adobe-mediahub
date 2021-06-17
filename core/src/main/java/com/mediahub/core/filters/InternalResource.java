package com.mediahub.core.filters;

import org.apache.sling.api.resource.*;

public class InternalResource extends AbstractResource {

    private final Resource resource;
    private final ResourceMetadata resourceMetadata;
    private final String path;

    public InternalResource(Resource mappedResource, String path) {
        this.resource = mappedResource;

        // make a copy of resource metadata object
        this.resourceMetadata = new ResourceMetadata();
        if (mappedResource.getResourceMetadata() != null) {
            this.resourceMetadata.putAll(mappedResource.getResourceMetadata());
        }

        this.path = path;

    }

    @Override
    public ValueMap getValueMap() {
        return super.getValueMap();
    }

    @Override
    public String getPath() {
        return this.path;
    }

    @Override
    public String getResourceType() {
        return resource.getResourceType();
    }

    @Override
    public String getResourceSuperType() {
        return resource.getResourceSuperType();
    }

    @Override
    public ResourceMetadata getResourceMetadata() {
        return this.resourceMetadata;
    }

    @Override
    public ResourceResolver getResourceResolver() {
        return resource.getResourceResolver();
    }

    @Override
    public <AdapterType> AdapterType adaptTo(Class<AdapterType> type) {
        AdapterType adapted = super.adaptTo(type);
        if (null == adapted) {
            // fallback to adapt from mapped resource (although this may lead sometimes to unexpected results e.g. original JCR node)
            adapted = resource.adaptTo(type);
        }
        return adapted;
    }

    Resource getResource() {
        return this.resource;
    }

    @Override
    public String toString() {
        return new StringBuilder(getClass().getSimpleName())
                .append("[type=").append(getResourceType())
                .append(", path=").append(getPath())
                .append(", resource=[").append(getResource()).append("]]").toString();
    }

}