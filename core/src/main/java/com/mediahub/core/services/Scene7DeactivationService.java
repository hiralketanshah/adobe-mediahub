package com.mediahub.core.services;

import com.day.cq.commons.Externalizer;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.message.BasicNameValuePair;

public interface Scene7DeactivationService {

  int getConnectionTimeOut();

  int getSocketTimeOut();

  String getDomainName();

  String getDeactivationUser();

  /**
   * Created Post Request Object to activate or deactivate asset to scene7
   *
   * @param path - Asset path to be activated or deactivated
   * @param action - Can either be activate or deactivate
   * @return PostRequest Object
   */
  default public HttpPost createGetRequestForMigration(String endpoint, String path, String action)
      throws UnsupportedEncodingException {
    HttpPost post = new HttpPost(endpoint + ".dmpublish.json");
    String encoding = Base64.getEncoder().encodeToString((getDeactivationUser()).getBytes(Charset.defaultCharset()));
    post.setHeader("User-Agent", "PostmanRuntime/7.26.8");
    post.setHeader("Accept", "*/*");
    post.setHeader("Host", "mediahub.bnpparibas");
    post.setHeader("Authorization", "Basic " + encoding);

    List<NameValuePair> params = new ArrayList<>(2);
    params.add(new BasicNameValuePair("action", action));
    params.add(new BasicNameValuePair("paths", path));
    post.setEntity(new UrlEncodedFormEntity(params, "UTF-8"));
    return post;
  }

}
