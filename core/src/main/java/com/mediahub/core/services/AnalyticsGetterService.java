package com.mediahub.core.services;

import java.io.IOException;
import java.util.Date;
import java.util.Map;

import org.apache.sling.api.resource.LoginException;

import com.google.gson.JsonObject;


public interface AnalyticsGetterService {
	
	/**
     * Number of Impressions
     */
	JsonObject getCustomReport(Map<String, String> parametersMap);
	
	JsonObject getFolders(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getAssetImpressionsPerDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getAssetImpressionsPerDayCounter(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsPerMonthTarget(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsPerMonth(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsPerDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	/**
	 * Assets Characteristic
	 */
	JsonObject getTopAssetImpressions(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getTopAssetMediaTypes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getTopAssetCategories(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getTopAssetTypes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getTopMediaThemes(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getTopAssetsSensibility(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getBroadcastStatus(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsWithSubs(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getSubtitles(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getDetailedAssetInfo(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getFreeForm(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	/**
	 * Technical Information
	 */
	JsonObject getDeviceType(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getDeviceTypeTop(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsPerBrowser(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getOperatingSystems(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getOperatingSystemsTable(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getReferrer(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	/**
	 * Geoloc
	 */
	JsonObject getMap(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getImpressionsPerCountry(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getLanguage(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	/**
	 * Time Parting
	 */
	JsonObject getWeekdayImpressions(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getWeekday(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;

	JsonObject getHourOfDay(String rootPath, Date startDate, Date endDate) throws IOException, LoginException;
}
