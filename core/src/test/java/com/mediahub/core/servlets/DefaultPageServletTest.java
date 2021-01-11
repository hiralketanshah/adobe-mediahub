package com.mediahub.core.servlets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

import io.wcm.testing.mock.aem.junit5.AemContext;
import io.wcm.testing.mock.aem.junit5.AemContextExtension;
import java.io.IOException;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import javax.jcr.RepositoryException;
import javax.jcr.UnsupportedRepositoryOperationException;
import javax.jcr.Value;
import javax.servlet.ServletException;
import org.apache.jackrabbit.api.security.user.Authorizable;
import org.apache.jackrabbit.api.security.user.Group;
import org.apache.jackrabbit.api.security.user.User;
import org.apache.sling.api.SlingHttpServletRequest;
import org.apache.sling.api.SlingHttpServletResponse;
import org.apache.sling.api.resource.ResourceResolver;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@ExtendWith(AemContextExtension.class)
public class DefaultPageServletTest {

  DefaultPageServlet defaultPageServlet = new DefaultPageServlet();

  @Mock
  SlingHttpServletRequest request;

  @Mock
  SlingHttpServletResponse response;

  @Mock
  ResourceResolver resourceResolver;

  @Mock
  User user;

  @BeforeEach
  public void setupMock() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  void doGet(AemContext context) throws ServletException, IOException, RepositoryException {

    Group group = new Group() {
      @Override
      public Iterator<Authorizable> getDeclaredMembers() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Authorizable> getMembers() throws RepositoryException {
        return null;
      }

      @Override
      public boolean isDeclaredMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public boolean isMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public boolean addMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public Set<String> addMembers(String... strings) throws RepositoryException {
        return null;
      }

      @Override
      public boolean removeMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public Set<String> removeMembers(String... strings) throws RepositoryException {
        return null;
      }

      @Override
      public String getID() throws RepositoryException {
        return null;
      }

      @Override
      public boolean isGroup() {
        return false;
      }

      @Override
      public Principal getPrincipal() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Group> declaredMemberOf() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Group> memberOf() throws RepositoryException {
        return null;
      }

      @Override
      public void remove() throws RepositoryException {

      }

      @Override
      public Iterator<String> getPropertyNames() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<String> getPropertyNames(String s) throws RepositoryException {
        return null;
      }

      @Override
      public boolean hasProperty(String s) throws RepositoryException {
        return false;
      }

      @Override
      public void setProperty(String s, Value value) throws RepositoryException {

      }

      @Override
      public void setProperty(String s, Value[] values) throws RepositoryException {

      }

      @Override
      public Value[] getProperty(String s) throws RepositoryException {
        return new Value[0];
      }

      @Override
      public boolean removeProperty(String s) throws RepositoryException {
        return false;
      }

      @Override
      public String getPath() throws UnsupportedRepositoryOperationException, RepositoryException {
        return null;
      }
    };

    List<Group> groups = new ArrayList<>();
    groups.add(group);

    Iterator<Group> currentUserGroups = groups.iterator();

    //MockSlingHttpServletRequest request = context.request();
    //MockSlingHttpServletResponse response = context.response();
    when(request.getResourceResolver()).thenReturn(resourceResolver);
    when(resourceResolver.adaptTo(User.class)).thenReturn(user);
    when(user.memberOf()).thenReturn(currentUserGroups);
    defaultPageServlet.doGet(request, response);
  }

  @Test
  void doGetNoGroup(AemContext context) throws ServletException, IOException, RepositoryException {
    when(request.getResourceResolver()).thenReturn(resourceResolver);
    when(resourceResolver.adaptTo(User.class)).thenReturn(user);
    when(user.memberOf()).thenThrow(new RepositoryException());
    defaultPageServlet.doGet(request, response);
  }

  @Test
  void doGetForBnpUser(AemContext context) throws ServletException, IOException, RepositoryException {

    Group group = new Group() {
      @Override
      public Iterator<Authorizable> getDeclaredMembers() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Authorizable> getMembers() throws RepositoryException {
        return null;
      }

      @Override
      public boolean isDeclaredMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public boolean isMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public boolean addMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public Set<String> addMembers(String... strings) throws RepositoryException {
        return null;
      }

      @Override
      public boolean removeMember(Authorizable authorizable) throws RepositoryException {
        return false;
      }

      @Override
      public Set<String> removeMembers(String... strings) throws RepositoryException {
        return null;
      }

      @Override
      public String getID() throws RepositoryException {
        return "mediahub-basic-contributor";
      }

      @Override
      public boolean isGroup() {
        return false;
      }

      @Override
      public Principal getPrincipal() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Group> declaredMemberOf() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<Group> memberOf() throws RepositoryException {
        return null;
      }

      @Override
      public void remove() throws RepositoryException {

      }

      @Override
      public Iterator<String> getPropertyNames() throws RepositoryException {
        return null;
      }

      @Override
      public Iterator<String> getPropertyNames(String s) throws RepositoryException {
        return null;
      }

      @Override
      public boolean hasProperty(String s) throws RepositoryException {
        return false;
      }

      @Override
      public void setProperty(String s, Value value) throws RepositoryException {

      }

      @Override
      public void setProperty(String s, Value[] values) throws RepositoryException {

      }

      @Override
      public Value[] getProperty(String s) throws RepositoryException {
        return new Value[0];
      }

      @Override
      public boolean removeProperty(String s) throws RepositoryException {
        return false;
      }

      @Override
      public String getPath() throws UnsupportedRepositoryOperationException, RepositoryException {
        return null;
      }
    };

    List<Group> groups = new ArrayList<>();
    groups.add(group);

    Iterator<Group> currentUserGroups = groups.iterator();

    //MockSlingHttpServletRequest request = context.request();
    //MockSlingHttpServletResponse response = context.response();
    when(request.getResourceResolver()).thenReturn(resourceResolver);
    when(resourceResolver.adaptTo(User.class)).thenReturn(user);
    when(user.memberOf()).thenReturn(currentUserGroups);
    defaultPageServlet.doGet(request, response);
    assertEquals(group.getID(), "mediahub-basic-contributor");
  }

}
