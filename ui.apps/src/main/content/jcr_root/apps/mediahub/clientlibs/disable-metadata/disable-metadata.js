(function($, $document) {
    var BNP_STATUS = "./jcr:content/metadata/bnpp-status",
    initialized = false;

    $document.on("foundation-contentloaded", init);

    function init(){
        disableStatus();
    }

    function disableStatus(){
        var $status = document.getElementsByName(BNP_STATUS);
        alert(window.location.href);
        alert($status);
        if(_.isEmpty($status) || $status.length < 0){
            return;
        }

        if(_.isEmpty(window.location.href) || !window.location.href.match("/foldersharewizard.html")){
            return;
        }

        if(window.location.href.match("/content/dam/medialibrary")){
            $status[0].setAttribute("disabled", "true");
        }
    }

}(jQuery, jQuery(document)));