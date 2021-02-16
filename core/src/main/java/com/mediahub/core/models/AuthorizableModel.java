package com.mediahub.core.models;

import org.osgi.annotation.versioning.ProviderType;

/**
 * Represents an interface for datasource items related to authorizables and is intended to be used by UI components.
 */
@ProviderType
public interface AuthorizableModel {

  /**
   * @return the authorizable ID or {@code null} if this information is not available.
   */
  String getId();

  /**
   * @return the authorizable home path or {@code null} if this information is not available.
   */
  String getHomePath();

  /**
   * @return the authorizable name or {@code null} if this information is not available.
   */
  String getName();

  /**
   * @return {@code true} if the authorizable has been created 24h ago.<br>
   *         {@code false} otherwise
   */
  boolean isNew();

  /**
   * Returns the formatted last modification date of the authorizable or {@code null} if this information is not available.
   *
   * @return last change date formatted using yyyy-MM-dd HH:mm pattern or {@code null}
   */
  String getLastModified();

  /**
   * Returns the formatted user name by whom the authorizable has been modified last time or {@code null} if this information is not
   * available.
   *
   * @return formatted user name or {@code null} if an error occurred.
   */
  String getLastModifiedBy();

  /**
   * Returns the formatted last publication date of the authorizable or {@code null} if this information is not available.
   *
   * @return last publish date formatted using yyyy-MM-dd HH:mm pattern or {@code null}
   */
  String getLastPublished();

  /**
   * Returns the formatted user name by whom the authorizable has been published last time or {@code null} if this information is not
   * available.
   *
   * @return formatted user name or {@code null} if an error occurred.
   */
  String getLastPublishedBy();

  /**
   * @return the authorizable's profile photo path or {@code null} if the authorizable doesn't have a profile photo
   */
  String getPhotoPath();

  /**
   * @return  {@code true} if the authorizable is a group<br>
   *          {@code false} otherwise
   */
  boolean isGroup();

  String getType();

  String getExpiry();

  String getCountry();

  String getCompany();
}
