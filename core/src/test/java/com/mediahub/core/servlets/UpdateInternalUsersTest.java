package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.when;

import java.io.PrintWriter;
import javax.management.NotCompliantMBeanException;

import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.services.UpdateInternalUsersService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith(AemContextExtension.class)
public class UpdateInternalUsersTest {
    
    AemContext context = new AemContext();

	@InjectMocks
	UpdateInternalUsers updateInternalUsers;

	@Mock
	UpdateInternalUsersService updateUsers;

	@Mock
	SlingHttpServletRequest req;

	@Mock
	SlingHttpServletResponse resp;

	@Mock
	PrintWriter printWriter;

	@BeforeEach
	public void setupMock() throws NotCompliantMBeanException {
		MockitoAnnotations.initMocks(this);
		context.registerService(UpdateInternalUsersService.class,updateUsers);
		context.registerInjectActivateService(updateInternalUsers);
	}

	@Test
	public void testDoGet() throws Exception {
		when(req.getParameter("remove")).thenReturn("true");
		when(resp.getWriter()).thenReturn(printWriter);
		assertAll(() -> updateInternalUsers.doGet(req, resp));
	}
}
