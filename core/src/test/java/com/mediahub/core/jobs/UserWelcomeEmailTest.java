package com.mediahub.core.jobs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import com.adobe.acs.commons.i18n.I18nProvider;
import com.day.cq.commons.Externalizer;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.GenericEmailNotification;
import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.apache.sling.event.jobs.Job;
import org.apache.sling.event.jobs.consumer.JobConsumer;
import org.apache.sling.settings.SlingSettingsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.osgi.service.component.annotations.Reference;

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

    @Mock
    I18nProvider provider;

    @Mock
    ResourceResolverFactory resolverFactory;

    @Mock
    ResourceResolver resolver;

    @Mock
    Externalizer externalizer;

    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        BnpConstants.WRITE_SERVICE);

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        Set<String> runModes = new HashSet<>();
        runModes.add("stage");
        when(slingSettingsService.getRunModes()).thenReturn(runModes);
        when(provider.translate("Welcome Email", LocaleUtils.toLocale(job.getProperty(BnpConstants.LANGUAGE, "en")))).thenReturn("Welcome Email");
        context.registerService(GenericEmailNotification.class, genericEmailNotification);
        context.registerService(ResourceResolverFactory.class, resolverFactory);
        context.registerService(Externalizer.class, externalizer);

        when(resolverFactory.getServiceResourceResolver(authInfo)).thenReturn(resolver);
        when(externalizer.authorLink(resolver, BnpConstants.CHANGE_PASSWORD_RESOURCE_PATH)).thenReturn(StringUtils.EMPTY);
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
