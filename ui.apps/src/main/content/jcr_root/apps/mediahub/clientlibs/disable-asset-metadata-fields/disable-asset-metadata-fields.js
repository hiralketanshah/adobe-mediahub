(function ($, $document) {
    var DOWNLOAD_DETAILS = "./jcr:content/metadata/downloadDetails",
    initialized = false;

    $document.on("foundation-contentloaded", init);

    function init() {
        disableAssetMetadata();
    }

    function disableAssetMetadata() {
        var isTechnicalAdmin = document.getElementById("user.technical.admin");
        var $status = document.getElementsByName(DOWNLOAD_DETAILS);
        if ($status === undefined || $status.length < 0) {
            return;
        }

        if (window.location.href === undefined || !window.location.href.match("/metadataeditor.external.html")) {
            return;
        }

        if(isTechnicalAdmin && isTechnicalAdmin.value === "true"){
          $status[0].removeAttribute("disabled");
        } else {
          $status[0].setAttribute("disabled", "true");
        }

    }

}(jQuery, jQuery(document)));