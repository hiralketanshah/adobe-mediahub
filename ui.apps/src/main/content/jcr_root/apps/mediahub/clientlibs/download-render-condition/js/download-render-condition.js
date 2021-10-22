(function ($window) {
  var registry = $window.adaptTo("foundation-registry");
  registry.register("foundation.collection.action.activecondition", {
    name: "dam.gui.download.activecondition",
    handler: function (name, el, config, collection, selections) {
        var isAsset = false;
          if (( selections.length  > 0)) {
              selections.forEach(function myfunction(item) {
                  if(item.getAttribute("bnpp-download-auth") === 'true'){
                      isAsset = true;
                  } else {
                      return false;
                  }
              });
          } else {
              return false;
          }
          return isAsset;
    }
  });
})($(window));