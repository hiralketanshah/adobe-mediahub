(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
       var privateCheckbox = $("#addprivaterestriction");

        if(privateCheckbox) {
            if(privateCheckbox.value == "true"){
				document.getElementById("internalfolder").hidden = true;
            	document.getElementById("internalusersection").hidden = true;
            }

            privateCheckbox.on('change', function handleChanged(event) {
               if(event.currentTarget.checked){
                document.getElementById("internalfolder").hidden = true;
                document.getElementById("internalusersection").hidden = true;
               } else {
                document.getElementById("internalfolder").hidden = false;
                document.getElementById("internalusersection").hidden = false;
               }
           });
        } else {
			document.getElementById("internalfolder").hidden = true;
            document.getElementById("internalusersection").hidden = true;
        }

	  	var internalfolderCheckbox = $("#internalfolder");

        if(internalfolderCheckbox) {
            if(internalfolderCheckbox.value == "true"){
            	document.getElementById("internalusersection").hidden = false;
            }

            internalfolderCheckbox.on('change', function handleChanged(event) {
               if(event.currentTarget.checked){
                document.getElementById("internalusersection").hidden = false;
               } else {
                document.getElementById("internalusersection").hidden = true;
               }
           });
        }

    }

}(jQuery, jQuery(document)));