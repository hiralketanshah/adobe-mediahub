package com.mediahub.core.services.impl;

import com.google.common.base.Supplier;
import com.google.common.base.Suppliers;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.mediahub.core.services.AuthService;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.*;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.*;
import org.osgi.service.metatype.annotations.AttributeDefinition;
import org.osgi.service.metatype.annotations.Designate;
import org.osgi.service.metatype.annotations.ObjectClassDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.interfaces.RSAPrivateKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;
import java.security.spec.PKCS8EncodedKeySpec;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import static java.lang.Boolean.TRUE;

@Component(
        service = {AuthService.class},
        configurationPolicy = ConfigurationPolicy.REQUIRE
)

@Designate(ocd = AuthServiceImpl.Config.class)
public class AuthServiceImpl implements AuthService {
    private static final Logger log = LoggerFactory.getLogger(AuthServiceImpl.class);

    private static final String RSA = "RSA";
    private static final String ACCESS_TOKEN = "access_token";
    private static final String EXPIRES_IN = "expires_in";
    private static final String END_PRIVATE_KEY = "-----END PRIVATE KEY-----";
    private static final String BEGIN_PRIVATE_KEY = "-----BEGIN PRIVATE KEY-----";
    private static final String NEW_LINE = "\\n";
    private static final String EMPTY = "";

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
        String orgId() default EMPTY;

        @AttributeDefinition(
                name = "Technical Account ID",
                description = "JWT technical account ID parameter"
        )
        String technicalAccountId() default EMPTY;

        @AttributeDefinition(
                name = "API Key",
                description = "JWT API key parameter"
        )
        String apiKey() default EMPTY;

        @AttributeDefinition(
                name = "Signing Certificate",
                description = "JWT signing certificate"
        )
        String privateKey() default EMPTY;

        @AttributeDefinition(
                name = "Auth host",
                description = "IMS Authentication Service host"
        )
        String authHost() default EMPTY;

        @AttributeDefinition(
                name = "Metascopes",
                description = "JWT integration scopes"
        )
        String metascopes() default EMPTY;

        @AttributeDefinition(
                name = "Auth Exchange endpoint",
                description = "IMS Authentication Service exchange path"
        )
        String exchangeEndpoint() default EMPTY;

        @AttributeDefinition(
                name = "Client Secret",
                description = "Secret for exchanging JWT token"
        )
        String secret() default EMPTY;

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
    public String getAuthToken() throws IOException {
        String accessToken = getAccessToken();
        log.debug("Access Token: " + accessToken);
        return accessToken;
    }

    private Boolean isJwtTokenExpired(String token) {
        try {
            final Date expiration = getExpirationDateFromJwtToken(token);
            return expiration.before(new Date());
        } catch (ExpiredJwtException | NoSuchAlgorithmException | InvalidKeySpecException | IOException | LoginException e) {
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

    private String getJWTToken() {
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
        for (String metascope : metascopes) {
            jwtClaims.put("https://" + this.config.authHost() + "/s/" + metascope, TRUE);
        }

        // Create the final JWT token
        jwtToken = Jwts.builder().setClaims(jwtClaims).signWith(SignatureAlgorithm.RS256, privateKey).compact();

        return jwtToken;
    }

    private RSAPrivateKey getKey() {
        return cachedPrivateKey.get();
    }

    private Supplier<RSAPrivateKey> privateKeySupplier() {
        return new Supplier<RSAPrivateKey>() {
            public RSAPrivateKey get() {
                return transformKey();
            }
        };
    }

    private RSAPrivateKey transformKey() {
        try {
            String privatekeyString = this.config.privateKey();
            privatekeyString = privatekeyString.replaceAll(NEW_LINE, EMPTY).replace(BEGIN_PRIVATE_KEY, EMPTY).replace(END_PRIVATE_KEY, EMPTY);
            log.debug("The sanitized private key string is " + privatekeyString);
            // Create the private key
            KeyFactory keyFactory = KeyFactory.getInstance(RSA);
            log.debug("The key factory algorithm is " + keyFactory.getAlgorithm());
            byte[] byteArray = privatekeyString.getBytes();
            log.debug("The array length is " + byteArray.length);
            //KeySpec ks = new PKCS8EncodedKeySpec(Base64.getDecoder().decode(keyString));
            byte[] encodedBytes = javax.xml.bind.DatatypeConverter.parseBase64Binary(privatekeyString);

            // Read the private key
            KeySpec ks = new PKCS8EncodedKeySpec(encodedBytes);
            RSAPrivateKey privateKey = (RSAPrivateKey) keyFactory.generatePrivate(ks);

            return privateKey;
        } catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
            log.error("Could not parse the provided private key", e);
        }

        return null;
    }

    private String getAccessToken() throws IOException {
        if (authToken != null && !isAuthTokenExpired()) {
            return authToken;
        }

        String jwtToken = getJWTToken();

        if (!StringUtils.isEmpty(jwtToken)) {
            log.debug("JWT Token: " + jwtToken);
            int timeout = 5;
            RequestConfig config = RequestConfig.custom()
                    .setConnectTimeout(timeout * 1000)
                    .setConnectionRequestTimeout(timeout * 1000)
                    .setSocketTimeout(timeout * 1000).build();
            try (CloseableHttpClient httpClient = HttpClientBuilder.create().setDefaultRequestConfig(config).build()) {
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

                Long expiresIn = jo.get(EXPIRES_IN).getAsLong();
                authTokenExpiry = Date.from(Instant.now().plusMillis(expiresIn));
                authToken = jo.get(ACCESS_TOKEN).getAsString();

                log.debug("Returning access_token " + authToken);
                return authToken;
            }
        } else {
            log.error("Failed to generate JWT token");
            throw new IOException("Could not generate JWT token");
        }

    }

    private Boolean isAuthTokenExpired() {
        return authTokenExpiry.before(new Date());
    }
}