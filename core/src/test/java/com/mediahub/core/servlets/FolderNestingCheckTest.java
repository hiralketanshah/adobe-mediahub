package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.IOException;
import javax.jcr.RepositoryException;
import javax.jcr.ValueFormatException;
import javax.servlet.FilterChain;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class FolderNestingCheckTest {

    @InjectMocks
    FolderNestingCheck folderNestingCheck;

    @Mock
    SlingHttpServletRequest req;

    @Mock
    SlingHttpServletResponse resp;

    @Mock
    FilterChain chain;

    @BeforeEach
    public void setupMock() throws RepositoryException, IOException {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testDoFilter() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(req.getRequestURI()).thenReturn(
                "/content/dam/medialibrary/test-folder-one/test-folder-two/test-folder-three/test-asset.jpg");
        assertAll(() -> folderNestingCheck.doFilter(req, resp, chain));
    }

    @Test
    public void testDoFilterInitialUpload() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(req.getRequestURI())
                .thenReturn("/content/dam/medialibrary/test-folder-one/test-asset.initiateUpload.json");
        assertAll(() -> folderNestingCheck.doFilter(req, resp, chain));
    }

    @Test
    public void testDoFilterCreateAsset() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(req.getRequestURI()).thenReturn("/content/dam/medialibrary/test-folder-one/test-asset.createasset.html");
        assertAll(() -> folderNestingCheck.doFilter(req, resp, chain));
    }

    @Test
    public void testDoFilterMaxLength() throws ValueFormatException, IllegalStateException, RepositoryException {

        when(req.getRequestURI()).thenReturn(
                "/content/dam/medialibrary/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-folder-one/test-asset.createasset.html");
        assertAll(() -> folderNestingCheck.doFilter(req, resp, chain));
    }

}
