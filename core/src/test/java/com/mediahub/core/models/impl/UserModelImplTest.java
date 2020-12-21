package com.mediahub.core.models.impl;

import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doReturn;

import com.adobe.granite.security.user.UserProperties;

import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class, MockitoExtension.class })
public class UserModelImplTest {
    
    @InjectMocks
    UserModelImpl userModelImpl;
    
    @Mock
    UserProperties userProperties;
    
    @Mock
    private UserManager userManager;
    
    @Mock
    User user;
    
    @Mock
    Authorizable authorizable;
    
    @BeforeEach
    public void setUp() throws Exception {
        
    }
    
    @Test
    public void testGetJobTitle() throws Exception {
        when(userProperties.getProperty("jobTitle")).thenReturn("Tset Title");
        userModelImpl.getJobTitle();
        assertEquals("Tset Title",userProperties.getProperty("jobTitle"));
    }
    
    @Test
    public void testIsSystemUser() throws Exception {
        when(authorizable.isGroup()).thenReturn(true); 
        userModelImpl.isSystemUser();
        assertEquals(true,authorizable.isGroup());
    } 
    
    @Test
    public void testIsDisabled() throws Exception {
        when(authorizable.isGroup()).thenReturn(true); 
        userModelImpl.isDisabled();
        assertEquals(true,authorizable.isGroup());
    } 

}
