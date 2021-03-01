(function($, $document) {
    var FILE_SIZE_NAME = "./jcr:content/metadata/dam:size",
    FORMATTED_ASSET_SIZE = "./jcr:content/metadata/asset:size",
    initialized = false;

    $document.on("foundation-contentloaded", init);
 
    function init(){
        if(initialized){
            return;
        }
 
        initialized = true;
 
        convertFileSize();
    }

    function convertFileSize(){
        var $damSize = document.getElementsByName(FILE_SIZE_NAME);
        var $formattedAssetSize = document.getElementsByName(FORMATTED_ASSET_SIZE);
        if(_.isEmpty($damSize) || $damSize.length < 0){
            return;
        }

        if(_.isEmpty($formattedAssetSize) || $formattedAssetSize.length < 0){
            return;
        }
        var sizeInBytes = $damSize[0].getAttribute("value");
        $formattedAssetSize[0].value = (!sizeInBytes ?  "Unavailable" : formatBytes(parseInt(sizeInBytes), 2));
        $formattedAssetSize[0].setAttribute("value", !sizeInBytes ?  "Unavailable" : formatBytes(parseInt(sizeInBytes), 2));
    }
 
    function formatBytes(bytes, decimals) {
        if (bytes === 0){
            return '0 Bytes';
        }
 
        const k = 1024;
        const dm = decimals < 0 ? 0 : decimals;
        const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'];
        const i = Math.floor(Math.log(bytes) / Math.log(k));
 
        return parseFloat((bytes / Math.pow(k, i)).toFixed(dm)) + ' ' + sizes[i];
    }
}(jQuery, jQuery(document)));