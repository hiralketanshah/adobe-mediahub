package com.mediahub.core.scripts;

import static org.junit.jupiter.api.Assertions.assertAll;

import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.ondeploy.scripts.OnDeployScript;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class PermissionTabRenderConditionTest {

    @InjectMocks
    OnDeployScript script = new PermissionTabRenderCondition();
    
    @Mock
    ResourceResolver resourceResolver;
    
    @Mock
    Resource resource;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testGetScripts() {
        when(resourceResolver.getResource(Mockito.anyString())).thenReturn(resource);
        assertAll(() -> script.execute(resourceResolver));
    }
}
