package com.mediahub.core.utils;

import com.google.gson.Gson;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Map;
import org.apache.sling.api.SlingHttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ResponseUtil {

  private static Logger log = LoggerFactory.getLogger(ResponseUtil.class);

  private ResponseUtil() {
    // private Constructor
  }

  /**
   * Method to set Json Response
   *
   * @param status
   * @param response
   * @param responseMap
   * @throws IOException
   */
  public static void setJsonResponse(int status, SlingHttpServletResponse response, Map<String, Object> responseMap)
      throws IOException {
    response.setStatus(status);
    response.setContentType("application/json");
    response.setCharacterEncoding("UTF-8");
    PrintWriter out = response.getWriter();
    if (out != null) {
      out.write(new Gson().toJson(responseMap));
      out.flush();
      out.close();
    }
  }
}
