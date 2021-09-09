package com.mediahub.core.schedulers;

import org.apache.sling.commons.scheduler.ScheduleOptions;
import org.apache.sling.commons.scheduler.Scheduler;
import org.apache.sling.settings.SlingSettingsService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Modified;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.AttributeType;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.mediahub.core.services.UpdateInternalUsersService;

@Component(immediate = true, service = Runnable.class)
@Designate(ocd = UpdateInternalUsersScheduler.SchedulerConfig.class)
public class UpdateInternalUsersScheduler implements Runnable {

	@Reference
	UpdateInternalUsersService updateInternalUsersService;

	@Reference
	private SlingSettingsService slingSettingsService;

	String cronExpression;
	private int schedulerId;
	private String schedulerName;
	private String csvUserInfo;
	private String csvAdditionalInfo;
	private boolean isEnabled;

	/** LOGGER */
	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateInternalUsersScheduler.class);

	@Reference
	private Scheduler scheduler;

	@Activate
	public void activate(UpdateInternalUsersScheduler.SchedulerConfig config) {
		LOGGER.debug("Activate method");
		addScheduler(config);
	}

	@Modified
	protected void modified(SchedulerConfig config) {
		removeScheduler();
		if (null != schedulerName) {
			schedulerId = schedulerName.hashCode();
		}
		LOGGER.debug("In modified method");
		addScheduler(config);
	}

	@Deactivate
	protected void deactivate(SchedulerConfig config) {
		removeScheduler();
	}

	private void removeScheduler() {
		scheduler.unschedule(String.valueOf(schedulerId));
	}

	private void addScheduler(SchedulerConfig config) {
		LOGGER.debug("Add scheduler method");
		isEnabled = config.serviceEnabled();
		if (isEnabled) {
			schedulerName = config.schedulerName();
			cronExpression = config.cronExpression();
			csvUserInfo = config.csvUserInfo();
			csvAdditionalInfo = config.csvAdditionalInfo();
			ScheduleOptions scheduleOptions = scheduler.EXPR(cronExpression);
			scheduleOptions.canRunConcurrently(false);
			scheduleOptions.name(schedulerName);
			scheduler.schedule(this, scheduleOptions);
			LOGGER.debug("MediaHub Updating Internal Users Scheduler");
		}

	}

	@Override
	public void run() {
		LOGGER.debug("In run method"+updateInternalUsersService);
		if (isEnabled) {
			updateInternalUsersService.createAndUpdateUsers(csvUserInfo, csvAdditionalInfo);
			LOGGER.debug(
					"Internal Users are successfully created/updated or deleted as per the records present in the latest CSV file");

		}
	}

	@ObjectClassDefinition(name = "MediaHub Updating Internal Users Scheduler", description = "MediaHub Updating Internal Users Scheduler")
	public @interface SchedulerConfig {

		@AttributeDefinition(name = "Enabled", description = "Enable Scheduler", type = AttributeType.BOOLEAN)
		boolean serviceEnabled() default true;

		@AttributeDefinition(name = "Cron Job Expression", description = "Cron Job Expression", type = AttributeType.STRING)
		public String cronExpression() default "0 0 3 1/1 * ? *";

		@AttributeDefinition(name = "Scheduler name", description = "Scheduler name", type = AttributeType.STRING)
		public String schedulerName() default "MediaHub Updating Internal Users Scheduler";

		@AttributeDefinition(name = "CSV File Path For User Info", description = "CSV Path For User Info", type = AttributeType.STRING)
		public String csvUserInfo();

		@AttributeDefinition(name = "CSV File Path For Additional Info", description = "CSV File Path For Additional Info", type = AttributeType.STRING)
		public String csvAdditionalInfo();

	}

}
