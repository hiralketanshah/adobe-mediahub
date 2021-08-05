package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith({ AemContextExtension.class })
class UserWelcomeEmailTest {

    private final AemContext context = new AemContext();

    @InjectMocks
    private UserWelcomeEmail userWelcomeEmail;

    @Mock
    Job job;

    @Mock
    GenericEmailNotification genericEmailNotification;

    @Mock
    SlingSettingsService slingSettingsService;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        Set<String> runModes = new HashSet<>();
        runModes.add("stage");
        when(slingSettingsService.getRunModes()).thenReturn(runModes);
        context.registerService(GenericEmailNotification.class, genericEmailNotification);
        String[] emailRecipients = { "abibrahi@adobe.com" };
        Map<String, String> emailParams = new HashMap<>();
        emailParams.put(BnpConstants.SUBJECT, "Mediahub - Welcome Email");
        emailParams.put("firstname", "Abu");
        doNothing().when(genericEmailNotification).sendEmail("/etc/mediahub/mailtemplates/welcome.html",emailRecipients, emailParams);;
        when(job.getProperty(BnpConstants.EMAIL, StringUtils.EMPTY)).thenReturn("abibrahi@adobe.com");
        when(job.getProperty(BnpConstants.EMAIL)).thenReturn("abibrahi@adobe.com");

    }

    @Test
    void process() {
        assertEquals(JobConsumer.JobResult.OK, userWelcomeEmail.process(job));
    }

}
