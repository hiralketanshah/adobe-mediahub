package com.mediahub.core.services.impl;

import static java.lang.Boolean.TRUE;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.json.JSONException;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.ConfigurationPolicy;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.mediahub.core.constants.BnpConstants;
import com.mediahub.core.services.AuthService;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

@Component(
        service = {AuthService.class},
        configurationPolicy = ConfigurationPolicy.REQUIRE
)

@Designate(ocd = AuthServiceImpl.Config.class)
public class AuthServiceImpl implements AuthService {
    private static final Logger log = LoggerFactory.getLogger(AuthServiceImpl.class);
    
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE, BnpConstants.WRITE_SERVICE);
    
    @Reference
	private ResourceResolverFactory resolverFactory;

    @ObjectClassDefinition(
            name = "Mediahub Authentication Token Manager",
            description = "Service for managing athentication access to web services"
    )
    @interface Config {
    	@AttributeDefinition(
                name = "Organization ID",
                description = "JWT Organization ID parameter"
        )
        String orgId() default "";
    	
    	@AttributeDefinition(
                name = "Technical Account ID",
                description = "JWT technical account ID parameter"
        )
        String technicalAccountId() default "";
    	
    	@AttributeDefinition(
                name = "API Key",
                description = "JWT API key parameter"
        )
        String apiKey() default "";
    	
    	@AttributeDefinition(
                name = "Certificate Resource Path",
                description = "JWT signing certificate path in AEM"
        )
        String keyPath() default "";
    	
    	@AttributeDefinition(
                name = "Auth host",
                description = "IMS Authentication Service host"
        )
        String authHost() default "";
    	
    	@AttributeDefinition(
                name = "Metascopes",
                description = "JWT integration scopes"
        )
        String metascopes() default "";
    	
    	@AttributeDefinition(
                name = "Auth Exchange endpoint",
                description = "IMS Authentication Service exchange path"
        )
        String exchangeEndpoint() default "";
    	
    	@AttributeDefinition(
                name = "Client Secret",
                description = "Secret for exchanging JWT token"
        )
        String secret() default "";
        
        @AttributeDefinition(
                name = "JWT Expiration",
                description = "JWT token expiration time in hours"
        )
        int jwtExpiration() default 24;
        
        @AttributeDefinition(
                name = "Key Expiration",
                description = "Private Key refresh time in hours"
        )
        int keyExpiration() default 24;
    }

    private Config config;
    private ExecutorService executorService;
    private String jwtToken, authToken;
    private Date authTokenExpiry;
    
    private Supplier<RSAPrivateKey> cachedPrivateKey;

    @Activate
    protected void activate(Config config) {
        this.executorService = Executors.newCachedThreadPool();
        this.config = config;
        this.jwtToken = null;
        this.authToken = null;
        this.authTokenExpiry = Date.from(Instant.EPOCH);
        this.cachedPrivateKey = Suppliers.memoizeWithExpiration(privateKeySupplier(), this.config.keyExpiration(), TimeUnit.HOURS);
    }

    @Deactivate
    protected void deactivate(Config config) {
        this.executorService.shutdown();
    }
    
    @Override
	public String getAuthToken() throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, JSONException, LoginException {
		String jwtToken = getJWTToken();
		if(jwtToken != null && jwtToken != "") {
			log.info("JWT Token: " + jwtToken);
			String accessToken = getAccessToken(jwtToken);
			log.info("Access Token: " + accessToken);
			return accessToken;
		} else {
			log.error("Failed to generate JWT token");
			throw new IOException("Could not generate JWT token");
		}		
	}
    
    private Boolean isJwtTokenExpired(String token) throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, LoginException {
    	try {
    		final Date expiration = getExpirationDateFromJwtToken(token);
    		return expiration.before(new Date());
    	} catch (ExpiredJwtException e) {
			return true;
		}
	}
    
    private Date getExpirationDateFromJwtToken(String token) throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, LoginException {
		return getClaimFromJwtToken(token, Claims::getExpiration);
	}
    
    private <T> T getClaimFromJwtToken(String token, Function<Claims, T> claimsResolver) throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, LoginException {
		final Claims claims = getAllClaimsFromJwtToken(token);
		return claimsResolver.apply(claims);
	}
    
    private Claims getAllClaimsFromJwtToken(String token) throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, LoginException {
		return Jwts.parser().setSigningKey(getKey()).parseClaimsJws(token).getBody();
	}

	private String getJWTToken() throws NoSuchAlgorithmException, InvalidKeySpecException, IOException, LoginException {
		if (jwtToken != null && !isJwtTokenExpired(jwtToken)) {
			return jwtToken;
		}
		
		// Hours to seconds
		Long expirationSeconds = this.config.jwtExpiration() * 3600L;
		// Expiration time in seconds
		Long expirationTime = System.currentTimeMillis() / 1000 + expirationSeconds;
		// Metascopes associated to key
		String metascopes[] = this.config.metascopes().split(",");

		RSAPrivateKey privateKey = getKey();

		// Create JWT payload
		Map<String, Object> jwtClaims = new HashMap<>();
		jwtClaims.put("iss", this.config.orgId());
		jwtClaims.put("sub", this.config.technicalAccountId());
		jwtClaims.put("exp", expirationTime);
		jwtClaims.put("aud", "https://" + this.config.authHost() + "/c/" + this.config.apiKey());
		for(String metascope : metascopes) {
			jwtClaims.put("https://" + this.config.authHost() + "/s/" + metascope, TRUE);
		}

		// Create the final JWT token
		jwtToken = Jwts.builder().setClaims(jwtClaims).signWith(SignatureAlgorithm.RS256, privateKey).compact();

		return jwtToken;
	}
	
	public RSAPrivateKey getKey() {
        return cachedPrivateKey.get();
    }

    private Supplier<RSAPrivateKey> privateKeySupplier() {
        return new Supplier<RSAPrivateKey>() {
            public RSAPrivateKey get() {
                try {
					return transformKey();
				} catch (NoSuchAlgorithmException | InvalidKeySpecException | LoginException | IOException e) {
					log.error("Failed to transform private key", e);
					return null;
				}
            }
        };
    }

	private RSAPrivateKey transformKey() throws LoginException, IOException, NoSuchAlgorithmException, InvalidKeySpecException {
		try (ResourceResolver resourceResolver = resolverFactory.getServiceResourceResolver(authInfo)) {
			Resource privateKeyRes = resourceResolver.getResource(this.config.keyPath());
			InputStream is = privateKeyRes.adaptTo(InputStream.class);
			BufferedInputStream bis = new BufferedInputStream(is);
			ByteArrayOutputStream buf = new ByteArrayOutputStream();
			int result = bis.read();
			while (result != -1) {
				byte b = (byte) result;
				buf.write(b);
				result = bis.read();
			}
			
			String privatekeyString = buf.toString();
			privatekeyString  = privatekeyString.replaceAll("\\n", "").replace("-----BEGIN PRIVATE KEY-----", "").replace("-----END PRIVATE KEY-----", "");
			log.debug("The sanitized private key string is "+privatekeyString);
			// Create the private key
			KeyFactory keyFactory = KeyFactory.getInstance("RSA");
			log.debug("The key factory algorithm is "+keyFactory.getAlgorithm());
			byte []byteArray = privatekeyString.getBytes();
			log.debug("The array length is "+byteArray.length);
			//KeySpec ks = new PKCS8EncodedKeySpec(Base64.getDecoder().decode(keyString));
			byte[] encodedBytes = javax.xml.bind.DatatypeConverter.parseBase64Binary(privatekeyString);
			
			// Read the private key
			KeySpec ks = new PKCS8EncodedKeySpec(encodedBytes);
			RSAPrivateKey privateKey = (RSAPrivateKey) keyFactory.generatePrivate(ks);
			
			return privateKey;
		}		
	}

	private String getAccessToken(String jwtToken) throws IOException, JSONException {
		if (authToken != null && !isAuthTokenExpired()) {
			return authToken;
		}
		
		HttpClient httpClient = HttpClientBuilder.create().build();
		
		HttpHost authServer = new HttpHost(this.config.authHost(), 443, "https");
        HttpPost authPostRequest = new HttpPost(this.config.exchangeEndpoint());
        authPostRequest.addHeader("Cache-Control", "no-cache");
        List<NameValuePair> params = new ArrayList<NameValuePair>();
        params.add(new BasicNameValuePair("client_id", this.config.apiKey()));
        params.add(new BasicNameValuePair("client_secret", this.config.secret()));
        params.add(new BasicNameValuePair("jwt_token", jwtToken));
        authPostRequest.setEntity(new UrlEncodedFormEntity(params, Consts.UTF_8));
        HttpResponse response = httpClient.execute(authServer, authPostRequest);
        if (200 != response.getStatusLine().getStatusCode()) {
            HttpEntity ent = response.getEntity();
            String content = EntityUtils.toString(ent);
            log.error("JWT: Server Returned Error\n", response.getStatusLine().getReasonPhrase());
            log.error("ERROR DETAILS: \n", content);
            throw new IOException("Server returned error: " + response.getStatusLine().getReasonPhrase());
        }
        HttpEntity entity = response.getEntity();
		
		JsonObject jo = new JsonParser().parse(EntityUtils.toString(entity)).getAsJsonObject();
		
		Long expiresIn = jo.get("expires_in").getAsLong();
		authTokenExpiry = Date.from(Instant.now().plusMillis(expiresIn));
		authToken = jo.get("access_token").getAsString();
		
        log.debug("Returning access_token " + authToken);
        return authToken;
	}
	
	private Boolean isAuthTokenExpired() {
		return authTokenExpiry.before(new Date());
	}
}