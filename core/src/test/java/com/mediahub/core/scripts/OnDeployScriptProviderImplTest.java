package com.mediahub.core.scripts;

import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class OnDeployScriptProviderImplTest {

    @InjectMocks
    OnDeployScriptProviderImpl onDeployScriptProviderImpl;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testGetScripts() {
        assertAll(() -> onDeployScriptProviderImpl.getScripts());
    }
}
