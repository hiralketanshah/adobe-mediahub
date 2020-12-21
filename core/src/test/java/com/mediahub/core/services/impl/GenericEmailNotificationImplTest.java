package com.mediahub.core.services.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.adobe.acs.commons.email.EmailService;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
public class GenericEmailNotificationImplTest {

    @InjectMocks
    GenericEmailNotificationImpl genericEmailNotificationImpl;

    @Mock
    EmailService emailService;

    String arg = "dummy";

    String[] array = { "test@gmail.com" };

    Map<String, String> map = new HashMap<>();

    List<String> failureList;

    List<String> emptylist = Collections.emptyList();

    @BeforeEach
    public void setupMock() throws IOException {

    }

    @Test
    public void testSendEmail() throws Exception {

        when(emailService.sendEmail(arg, map, array)).thenReturn(emptylist);

        genericEmailNotificationImpl.sendEmail(arg, array, map);
        verify(emailService).sendEmail(arg, map, array);
    }

    @Test
    public void testSendEmail2() throws Exception {

        map.put("subject", "Test Email");

        failureList = new ArrayList<>();
        failureList.add("success");
        when(emailService.sendEmail(arg, map, array)).thenReturn(failureList);

        genericEmailNotificationImpl.sendEmail(arg, array, map);
        verify(emailService).sendEmail(arg, map, array);
    }

}
