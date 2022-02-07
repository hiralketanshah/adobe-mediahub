(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      // Changes as per MED-542
      var userType = document.getElementById("user.type");
      if(userType && userType.value === "external"){
        disableFields($("coral-datepicker[name='./profile/expiry']"));
        disableFields($("input[name='./profile/email']"));
        disableFields($("input[name='./profile/givenName']"));
        disableFields($("input[name='./profile/familyName']"));
        disableFields($("input[name='./profile/company']"));
        disableFields($("input[name='./profile/status']"));
        disableFields($("coral-select[name='./profile/type']"));
        disableFields($("input[name='./profile/pole']"));
        disableFields($("input[name='./profile/uo']"));
        disableFields($("input[name='./profile/jobTitle']"));
        disableFields($("input[name='./profile/city']"));
        disableFields($("coral-select[name='./profile/country']"));
        disableFields($("coral-datepicker[name='./profile/expiry']"));
      }
    }

    function disableFields(element){
      if(element && element.length > 0){
        element[0].disabled = true;
      }
    }

}(jQuery, jQuery(document)));