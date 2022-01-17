package com.mediahub.core.utils;

import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.day.cq.dam.scene7.api.S7Config;
import com.day.cq.dam.scene7.api.Scene7Service;
import com.day.cq.dam.scene7.api.model.Scene7Asset;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class AssetUtilsTest {

    AemContext context = new AemContext();

    @InjectMocks
    AssetUtils fixture;

    @Mock
    private S7Config s7Config;

    @Mock
    Scene7Service scene7Service;

    @Mock
    Scene7Asset asset;

    List<Scene7Asset> listOfAssets = new ArrayList<>();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        listOfAssets.add(asset);
        when(scene7Service.getAssets(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
                .thenReturn(listOfAssets);
        when(asset.getSubAssets()).thenReturn(listOfAssets);
        when(asset.getHeight()).thenReturn(720L);
        when(scene7Service.getAssociatedAssets(Mockito.any(), Mockito.any())).thenReturn(asset);
    }

    @Test
    public void execute() throws Exception {

        fixture.getVideoShareLinkId(s7Config, scene7Service, "1234");
    }

}
