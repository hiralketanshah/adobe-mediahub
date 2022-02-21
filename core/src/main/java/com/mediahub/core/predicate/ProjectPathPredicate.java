package com.mediahub.core.predicate;

import com.day.cq.commons.predicate.AbstractNodePredicate;
import com.day.cq.search.PredicateGroup;
import com.day.cq.search.Query;
import com.day.cq.search.QueryBuilder;
import com.day.cq.search.result.SearchResult;
import com.mediahub.core.utils.QueryUtils;
import com.mediahub.core.utils.UserUtils;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import org.apache.commons.collections.Predicate;
import org.apache.commons.lang.StringUtils;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.UserManager;
import org.apache.sling.api.resource.LoginException;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ResourceResolver;
import org.apache.sling.api.resource.ResourceResolverFactory;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@Component(
    immediate = true,
    service = Predicate.class,
    property = {
        "predicate.name=mediahubProjectPath"
    }
)
public class ProjectPathPredicate extends AbstractNodePredicate implements Predicate{

  private final Logger logger = LoggerFactory.getLogger(ProjectPathPredicate.class);

  @Reference
  ResourceResolverFactory resolverFactory;

  List<String> allowedPaths = new ArrayList<>();

  Session session;

  @Override
  public boolean evaluate(Node node) throws RepositoryException {
    logger.debug("Project Path Predicate : {}", node.getPath());
    final Map<String, Object> authInfo = Collections.singletonMap(ResourceResolverFactory.SUBSERVICE,
        "writeService");
    try (ResourceResolver resolver = resolverFactory.getServiceResourceResolver(authInfo)) {
      UserManager userManager = resolver.adaptTo(UserManager.class);
      String userID = node.getSession().getUserID();
      Authorizable user = userManager.getAuthorizable(userID);

      if(!node.getPrimaryNodeType().isNodeType("sling:Folder")){
        return false;
      }

      if(UserUtils.isTechnicalAdmin(user)){
        return Boolean.TRUE;
      }

      // to avoid query execution for same session
      if( session != node.getSession()){
        session = node.getSession();
        allowedPaths.clear();
      }
      getAllowedPath(resolver, user);

      for(String path : allowedPaths){
        if(StringUtils.contains(node.getPath(),path)){
          return Boolean.TRUE;
        }
      }
    } catch (LoginException e) {
      logger.error("Error while fetching system user {0}", e);
      return Boolean.FALSE;
    }
    return Boolean.FALSE;
  }

  /**
   * Allowed paths for the entity manager
   *
   * @param resolver - Resource Resolver
   * @param user - current user authorizable
   * @throws RepositoryException
   */
  private void getAllowedPath(ResourceResolver resolver, Authorizable user)
      throws RepositoryException {
    if(allowedPaths.isEmpty()){
      Iterator<Group> groups = user.memberOf();
      while(groups.hasNext()){
        Group group = groups.next();
        String groupID = group.getID();
        if(StringUtils.contains(groupID, "-entity-manager")){
          String uuid = StringUtils.replace(groupID, "-entity-manager", StringUtils.EMPTY);
          QueryBuilder builder = resolver.adaptTo(QueryBuilder.class);
          Map<String, String> predicateMap = QueryUtils
              .getMetadataByUUID("/content/dam", uuid);
          Query query = builder.createQuery(
              PredicateGroup.create(predicateMap), resolver.adaptTo(Session.class));
          SearchResult result = query.getResult();
          logger.info("Query {}", result.getQueryStatement());

          Iterator<Resource> resources = result.getResources();
          while(resources.hasNext()){
            Resource resource = resources.next();
            allowedPaths.add(resource.getParent().getParent().getParent().getPath());
          }
        }
      }
    }
  }
}
