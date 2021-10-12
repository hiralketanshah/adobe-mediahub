(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      var internalfolderCheckbox = $("#internalfolder");
      if(internalfolderCheckbox && document.getElementById("internaluserselection")) {
          if($("#internalfolder")[0] && $("#internalfolder")[0].getAttribute("checked") === ''){
              document.getElementById("internaluserselection").hidden = false;
          } else {
              document.getElementById("internaluserselection").hidden = true;
          }

          internalfolderCheckbox.on('change', function handleChanged(event) {
             if(event.currentTarget.checked){
              document.getElementById("internaluserselection").hidden = false;
             } else {
              document.getElementById("internaluserselection").hidden = true;
             }
         });
      }
    }

}(jQuery, jQuery(document)));