(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      var millisecondsToWait = 500;
      setTimeout(function() {
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

          if($("coral-select[name='./profile/type']")[0].value === "internal"){
            $("coral-datepicker[name='./profile/expiry']")[0].disabled = true;
          } else {
            $("coral-datepicker[name='./profile/expiry']")[0].disabled = false;
          }
        }


        if($("coral-datepicker[name='./profile/expiry']") && $("coral-datepicker[name='./profile/expiry']").length > 0){
            var interimDate = moment(new Date());
          	interimDate.add(365, "days");  //display 365 days prior to today
			      $("coral-datepicker[name='./profile/expiry']")[0].max = interimDate.format("YYYY-MM-DD");
        }

      }, millisecondsToWait);

    }

}(jQuery, jQuery(document)));