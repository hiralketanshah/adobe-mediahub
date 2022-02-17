(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      var millisecondsToWait = 700;

      setTimeout(function() {
        var isTechnicalAdmin = document.getElementById("user.technical.admin");
        var isManager = document.getElementById("user.entity.project.manager");
        var userType = document.getElementById("user.type");
        // Changes as per MED-224
        if($("coral-select[name='./profile/type']") && $("coral-select[name='./profile/type']").length > 0){
          var activities = $("coral-select[name='./profile/type']")[0];
          activities.addEventListener("change", function() {
              if(activities.value == "internal"){
                $("coral-datepicker[name='./profile/expiry']")[0].disabled = true;
              } else {
                $("coral-datepicker[name='./profile/expiry']")[0].disabled = false;
              }
          });
          // changes as per MED-542
          hideProfileElements(true);
          $("coral-select[name='./profile/type']")[0].disabled = true;
          if( (isTechnicalAdmin.value === "true" || isManager.value === "true") && userType.value === "external"){
              showProfileElements();
          } else if(isTechnicalAdmin.value === "true" && userType.value === "internal"){
              showProfileElements();
              $("coral-select[name='./profile/type']")[0].removeAttribute("disabled");
          }
        }

        if($("coral-datepicker[name='./profile/expiry']") && $("coral-datepicker[name='./profile/expiry']").length > 0){
          var interimDate = moment(new Date());
          interimDate.add(365, "days");  //display 365 days prior to today
          $("coral-datepicker[name='./profile/expiry']")[0].max = interimDate.format("YYYY-MM-DD");
        }

      }, millisecondsToWait);
    }

    function hideProfileElements(show){
      disableFields($("coral-datepicker[name='./profile/expiry']"));
      disableFields($("input[name='./profile/email']"));
      disableFields($("input[name='./profile/givenName']"));
      disableFields($("input[name='./profile/familyName']"));
      disableFields($("input[name='./profile/company']"));
      disableFields($("input[name='./profile/status']"));

      disableFields($("input[name='./profile/pole']"));
      disableFields($("input[name='./profile/uo']"));
      disableFields($("input[name='./profile/jobTitle']"));
      disableFields($("input[name='./profile/city']"));
      disableFields($("coral-select[name='./profile/country']"));
      disableFields($("coral-datepicker[name='./profile/expiry']"));
    }

    function showProfileElements(){
      enableFields($("coral-datepicker[name='./profile/expiry']"));
      enableFields($("input[name='./profile/email']"));
      enableFields($("input[name='./profile/givenName']"));
      enableFields($("input[name='./profile/familyName']"));
      enableFields($("input[name='./profile/company']"));
      enableFields($("input[name='./profile/status']"));

      enableFields($("input[name='./profile/pole']"));
      enableFields($("input[name='./profile/uo']"));
      enableFields($("input[name='./profile/jobTitle']"));
      enableFields($("input[name='./profile/city']"));
      enableFields($("coral-select[name='./profile/country']"));
      enableFields($("coral-datepicker[name='./profile/expiry']"));
    }

    function disableFields(element){
      if(element && element.length > 0){
        element[0].disabled = true;
      }
    }

    function enableFields(element){
      if(element && element.length > 0){
        element[0].removeAttribute("disabled");
      }
    }


}(jQuery, jQuery(document)));