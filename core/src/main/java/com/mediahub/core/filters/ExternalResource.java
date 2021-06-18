package com.mediahub.core.filters;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceMetadata;
import org.apache.sling.api.resource.ResourceWrapper;
import org.apache.sling.api.resource.ValueMap;

public class ExternalResource extends ResourceWrapper {

    private Resource resource;
    private String path;
    private ValueMap valueMap;
    private String resourceType = "mediahub/assets/tracking";

    public ExternalResource(Resource resource, String path, ValueMap valueMap) {
        super(resource);
        this.path = path;
        this.resource = resource;
        this.valueMap = valueMap;
    }

    @Override
    public ResourceMetadata getResourceMetadata() {
        ResourceMetadata metadata = new ResourceMetadata();
        metadata.setCharacterEncoding(resource.getResourceMetadata().getCharacterEncoding());
        metadata.setContentType(resource.getResourceMetadata().getContentType());
        metadata.setContentLength(resource.getResourceMetadata().getContentLength());
        metadata.setCreationTime(resource.getResourceMetadata().getCreationTime());
        metadata.setModificationTime(resource.getResourceMetadata().getModificationTime());
        metadata.setResolutionPath(path);
        metadata.setResolutionPathInfo(".html");
        return metadata;
    }

    @Override
    public String getResourceType() {
        if (resourceType != null) {
            return resourceType;
        } else {
            return super.getResourceType();
        }
    }

    @Override
    public ValueMap getValueMap() {
        if (valueMap != null) {
            return valueMap;
        } else {
            return super.getValueMap();
        }

    }

}
