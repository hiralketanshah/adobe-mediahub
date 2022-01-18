package com.mediahub.core.models;

import javax.jcr.RepositoryException;
import org.apache.sling.api.resource.LoginException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertEquals;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
class UserDataTest {

    @InjectMocks
    UserData userData;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

    }

    @Test
    void testUserData() throws LoginException, RepositoryException {
        userData.setId("test");
        userData.setFirstName("test-firstname");
        userData.setLastName("test-lastname");
        userData.setEmail("test@test.com");

        assertEquals("test", userData.getId());
        assertEquals("test-firstname", userData.getFirstName());
        assertEquals("test-lastname", userData.getLastName());
        assertEquals("test@test.com", userData.getEmail());
    }
}
