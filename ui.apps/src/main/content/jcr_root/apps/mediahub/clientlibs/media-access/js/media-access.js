(function ($window) {
  var registry = $window.adaptTo("foundation-registry");
  registry.register("foundation.collection.action.activecondition", {
      name: "dam.gui.mediaaccess.activecondition",
      handler: function (name, el, config, collection, selections) {
          var showDownload = true;
          if (( selections.length > 0)) {
              selections.forEach(function myfunction(item) {
                  if(item.baseURI.includes("aem/search")){
                      if(item.getElementsByClassName("foundation-collection-assets-meta").length > 0 && item.getElementsByClassName("foundation-collection-assets-meta")[0].getAttribute("data-foundation-collection-meta-type") === 'asset'){
                          if($("#media-access") && $("#media-access").length > 0){
                              if(item.dataset.graniteCollectionItemId){
                                  var assetPath = item.dataset.graniteCollectionItemId.substring(0, item.dataset.graniteCollectionItemId.lastIndexOf('/'));
                                  $("#media-access")[0].setAttribute("onclick","window.location.href ='/assets.html"+ assetPath + "';");
                              }
                          }
                      } else {
                          showDownload = false;
                      }
                  }
              });
          } else {
            showDownload = false;
          }
          return showDownload;
      }
  });
})($(window));