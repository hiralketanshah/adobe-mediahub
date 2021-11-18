package com.mediahub.core.services;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;

import org.apache.sling.api.resource.LoginException;
import org.json.JSONException;

public interface AuthService {

	String getAuthToken() throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, JSONException, LoginException;

}
