## Leveraging Sorting Capabilities

This section describes how different product team can utilize the sorting capability provided by foundationUI in their respective collection pages. Please follow the provided step to configure sorting.<br>

### For CollectionPage
1. Add a configuration support for sort component in collectionpage content structure. The available configuration for sort component can be found [here](./sort.rst). The sort component configuration needs to be added inside `actions/secondary` of collectionpage content structure.
    
    > Example:<br>
    CollectionPage with sorting capability [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml)<br>
    Sorting configuration added in collectionPage [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L24-L29)<br>
 
2. Configure the each view layout datasource to provide the layout items based on different sorting option.<br>
   Add the configuration for `sortName` and `sortDir` in views datasource content structure. This is necessary because views/layout read the `sortName` and `sortDir` values from datasource valueMap.<br>
    
    + sortName : This maps the value on which layout needs to be sorted.<br> 
        The sortName should be assigned the below mentioned value.<br>

          sortName = "${empty param.sortName ? (empty cookie["${collectionPage-consoleId}-sortName"].value
          ? "": cookie["${collectionPage-consoleId}-sortName"].value) : param.sortName}"

        `${collectionPage-consoleId}` should be replaced with consoleId property of collectionPage provided by product team [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.content/blob/master/src/main/content/jcr_root/libs/granite/ui/components/shell/collectionpage/collectionpage.jsp#L74)<br>
        `${collectionPage-consoleId}-sortName` key used to store sort option value in cookie<br>
        
        > Example:<br>
        CollectionPage consoleId example [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L8)<br>
        sortName configuration added in datasource content structure [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L102)<br>
        sortName value retrieved in datasource resource [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/components/datasource/datasource.jsp#L55)<br>
        
    + sortDir : This maps the direction value.<br>
        Supported values :
        * "asc" -> ascending direction
        * "desc" -> descending direction
        
        The sortDir should be assigned the below mentioned value.<br>
        
          sortDir = "${empty param.sortDir ? (empty cookie["${collectionPage-consoleId}-sortDir"].value
          ? "" : cookie["${collectionPage-consoleId}-sortDir"].value) : param.sortDir}"

        `${collectionPage-consoleId}` should be replaced with consoleId property of collectionPage provided by product [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.content/blob/master/src/main/content/jcr_root/libs/granite/ui/components/shell/collectionpage/collectionpage.jsp#L74)<br>
        `${collectionPage-consoleId}-sortDir` key used to store sort direction value in cookie.<br>
        
        > Example:<br>
        CollectionPage consoleId example [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L8)<br>
        sortDir configuration added in datasource content structure [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L103)<br>
        sortDir value retrieved in datasource resource [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/components/datasource/datasource.jsp#L56)<br>
        
    > Example:<br>
    Datasource content structure with sortName and sortDir property [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L74-L82)<br>
    Datasource with sorting capability [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/components/datasource/datasource.jsp)<br>

3. Change the src property of collectionpage views to support queryparams `sortName` and `sortDir`. Append the `{?sortName,sortDir}` at the end of src string to support URI template variables.

   > Example:<br>
    src property changed [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L52)
    
4. The foundationUI by default shows four sort options namely `none`, `created`, `modified` and `name`. Each team can also specify their custom options by using _configPath_ property of sort component.
   Even if a team does not add custom sort options, they need to provide support for sorting for default options in their datasource.<br>
   To maintain the backward compatibility with table layout sorting, it is the responsibility of each product team to provide all the supported custom sort options. 
    
    > Example:<br>
    Support for Custom sort options [here](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.it.e2e/blob/master/test/content_package/extensions/shell/CollectionPage/jcr_root/apps/tests/shell/collectionpage/.content.xml#L132-L139)

5. In case a product team uses a custom/own layout view, they need to add jsp changes done in [commit](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.content/commit/f337f780ad3decf9246663559b126cc3fd8ea9ff) and [commit](https://git.corp.adobe.com/Granite/com.adobe.granite.ui.content/commit/e55331c06ba2f228f3db407165f6a2a80120f98a) in their custom layout jsp.

### For Omnisearch

1. WIP : [GRANITE-31068](https://jira.corp.adobe.com/browse/GRANITE-31068)
_________________

For query, please contact :<br>
    <paregupt@adobe.com>
    
