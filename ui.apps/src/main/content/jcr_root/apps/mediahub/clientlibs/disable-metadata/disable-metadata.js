(function ($, $document) {
    var BNP_STATUS = "./jcr:content/metadata/bnpp-status",
        initialized = false;

    $document.on("foundation-contentloaded", init);

    function init() {
        disableStatus();
    }

    function disableStatus() {
        var $status = document.getElementsByName(BNP_STATUS);
        if ($status === undefined || $status.length < 0) {
            return;
        }

        if (window.location.href === undefined || !window.location.href.match("/foldersharewizard.html")) {
            return;
        }

        if (window.location.href.match("/content/dam/medialibrary")) {
            $status[0].setAttribute("disabled", "true");
        }
    }

}(jQuery, jQuery(document)));