(function ($window) {
  var registry = $window.adaptTo("foundation-registry");
  registry.register("foundation.collection.action.activecondition", {
    name: "dam.gui.download.activecondition",
    handler: function (name, el, config, collection, selections) {
        var showDownload = true;
          if (( selections.length  > 0)) {
              selections.forEach(function myfunction(item) {
                  if(item.getAttribute("bnpp-download-auth")){
                      showDownload =  false;
                  }

                  if(item.getElementsByClassName("foundation-collection-assets-meta").length > 0 && item.getElementsByClassName("foundation-collection-assets-meta")[0].getAttribute("data-foundation-collection-meta-folder") === 'true'){
                    showDownload =  true;
                  }
              });
          }
        return showDownload;
    }
  });

  registry.register("foundation.collection.action.activecondition", {
      name: "dam.gui.customdownload.activecondition",
      handler: function (name, el, config, collection, selections) {
          var isAsset = true;
            if (( selections.length  > 0)) {
                selections.forEach(function myfunction(item) {
                    if(item.getAttribute("bnpp-download-auth") === 'true'){
                        isAsset = isAsset && true;
                    } else {
                        isAsset = isAsset && false;
                    }
                });
            } else {
                return false;
            }
            return isAsset;
      }
    });
	
	$(document).on("assets-download-modal-ready", function(e){
        var res = $.ajax({
			url: "/bin/mediahub/checkInternal",
			type: "get",
			success: function (data) {},
			async: false,
			error: function (err) {
				console.log(err);
			}
		}).responseText;

      if(res == "true"){
          var email = $("div.email.row");
          if(email != undefined){
            email.hide();
          }
      }
  });
})($(window));