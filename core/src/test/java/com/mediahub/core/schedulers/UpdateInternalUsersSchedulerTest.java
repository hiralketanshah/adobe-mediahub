package com.mediahub.core.schedulers;

import static org.junit.jupiter.api.Assertions.assertAll;
import javax.management.NotCompliantMBeanException;

import org.apache.sling.api.resource.LoginException;
import org.apache.sling.commons.scheduler.ScheduleOptions;
import org.apache.sling.commons.scheduler.Scheduler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.mediahub.core.services.UpdateInternalUsersService;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;

@ExtendWith({ AemContextExtension.class })
public class UpdateInternalUsersSchedulerTest {

	private final AemContext context = new AemContext();

	@InjectMocks
	private UpdateInternalUsersScheduler updateInternalUsersScheduler;

	@Mock
	Scheduler scheduler;

	@Mock
	UpdateInternalUsersService updateUsers;

	@Mock
	ScheduleOptions scheduleOptions;

	@BeforeEach
	public void setup() throws NotCompliantMBeanException, LoginException {
		MockitoAnnotations.initMocks(this);
		context.registerService(UpdateInternalUsersService.class, updateUsers);
		context.registerService(Scheduler.class, scheduler);
		Mockito.when(scheduler.EXPR(Mockito.any(String.class))).thenReturn(scheduleOptions);
	}

	@Test
	public void testRun() {
		updateInternalUsersScheduler = context.registerInjectActivateService(new UpdateInternalUsersScheduler());
		assertAll(() -> updateInternalUsersScheduler.run());
	}
}
