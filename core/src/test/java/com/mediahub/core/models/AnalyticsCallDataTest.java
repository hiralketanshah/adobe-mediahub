package com.mediahub.core.models;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallFilter;
import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallMetric;
import com.mediahub.core.models.AnalyticsCallData.AnalyticsCallSearch;
import com.mediahub.core.models.AnalyticsCallData.Sort;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class AnalyticsCallDataTest {

   
    AnalyticsCallData analyticsCallData;

    @Mock
    AnalyticsCallMetric metric;

    @Mock
    AnalyticsCallFilter filter;

    @Mock
    AnalyticsCallSearch search;
    
    Sort sort;
    

    
    List<AnalyticsCallMetric> listofMetric = new ArrayList<>();
    List<AnalyticsCallFilter> listofFilter = new ArrayList<>();

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

    }

    @Test
    public void testGet() {
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofMetric, listofFilter, listofFilter, search, sort, 270L, "none");
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofMetric, listofFilter, listofFilter, search, sort, 270L);
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofMetric, listofFilter, listofFilter, search, sort, "none");
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofMetric, listofFilter, listofFilter, search, sort);
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofMetric, listofFilter, listofFilter, search, sort);
        analyticsCallData = new AnalyticsCallData("1234", "testdimension", listofFilter, listofMetric, listofFilter);
    }

}
