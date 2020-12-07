package com.mediahub.core.models;

import org.osgi.annotation.versioning.ProviderType;

/**
 * Represents an interface for datasource items related to users and is intended to be used by UI components.
 */
@ProviderType
public interface UserModel extends AuthorizableModel {
  /**
   * @return the user's profile job title or {@code null} if the user doesn't have a job title
   */
  String getJobTitle();

  /**
   * @return {@code true} if the user is a systems user, {@code false} otherwise
   */
  boolean isSystemUser();

  /**
   * @return {@code true} if the user is disabled, {@code false} otherwise
   */
  boolean isDisabled();



}
