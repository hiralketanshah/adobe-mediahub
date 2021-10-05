(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
       var privateCheckbox = $("#addprivaterestriction");
       var internalfolderCheckbox = $("#internalfolder");

        if(privateCheckbox) {

            if($("#addprivaterestriction")[0] && $("#addprivaterestriction")[0].getAttribute("checked") === ''){
                document.getElementById("internalfolder").hidden = true;
                document.getElementById("internaluserselection").hidden = true;
            } else {
                document.getElementById("internalfolder").hidden = false;
                document.getElementById("internaluserselection").hidden = false;
            }

            privateCheckbox.on('change', function handleChanged(event) {
               if(event.currentTarget.checked){
                document.getElementById("internalfolder").hidden = true;
                document.getElementById("internaluserselection").hidden = true;
               } else {
                document.getElementById("internalfolder").hidden = false;
                if(internalfolderCheckbox.checked){
                  document.getElementById("internaluserselection").hidden = false;
                }
               }
           });
        } else {
			      document.getElementById("internalfolder").hidden = true;
            document.getElementById("internaluserselection").hidden = true;
        }




        if(internalfolderCheckbox) {
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