(function ($, $document) {
    $document.on("foundation-contentloaded", init);

    function init() {
      var millisecondsToWait = 700;

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

          if(!isAdmin() && $("coral-select[name='./profile/type']")[0].value === "internal" && $("input[name='./profile/email']")[0].value !== ""){

            $("coral-datepicker[name='./profile/expiry']")[0].disabled = true;
			$("input[name='./profile/email']")[0].disabled = true;
			$("input[name='./profile/givenName']")[0].disabled = true;
			$("input[name='./profile/familyName']")[0].disabled = true;
			$("input[name='./profile/company']")[0].disabled = true;
			$("input[name='./profile/status']")[0].disabled = true;
			$("coral-select[name='./profile/type']")[0].disabled = true;
			$("input[name='./profile/pole']")[0].disabled = true;
			$("input[name='./profile/uo']")[0].disabled = true;
			$("input[name='./profile/jobTitle']")[0].disabled = true;
			$("input[name='./profile/city']")[0].disabled = true;
			$("coral-select[name='./profile/country']")[0].disabled = true;
          } else {
            $("coral-datepicker[name='./profile/expiry']")[0].disabled = false;
          }
        }
		if(isCurrentExternalUser()){
			$("coral-datepicker[name='./profile/expiry']")[0].disabled = true;
            $("coral-select[name='./profile/type']")[0].disabled = true;
        }


        if($("coral-datepicker[name='./profile/expiry']") && $("coral-datepicker[name='./profile/expiry']").length > 0){
            var interimDate = moment(new Date());
          	interimDate.add(365, "days");  //display 365 days prior to today
			      $("coral-datepicker[name='./profile/expiry']")[0].max = interimDate.format("YYYY-MM-DD");
        }
		
		

      }, millisecondsToWait);
	  
	  function isCurrentExternalUser() {
        var currentUser = getCurrentUser();
        var type;
		$.ajax( {
            url: "/apps/mediahub/projects/teammembers/datasource.userinfo.json?userid="+currentUser,
            async: false
        } ).done(handler);
        function handler(data){
            type = data.type;
        }
        if(type === "external"){
			return true;
        }
        return false;
	  }	
	  
	  function isAdmin() {
        var currentUser = getCurrentUser();
        var isAdmin;
		$.ajax( {
            url: "/apps/mediahub/projects/teammembers/datasource.userinfo.json?userid="+currentUser,
            async: false
        } ).done(handler);
        function handler(data){
            isAdmin = data.isAdmin;
        }

        if(isAdmin === true){
			return true;
        }
        return false;
	  }	

      function getCurrentUser() {
		var currentUserId;
        var result = Granite.$.ajax({
			type: "GET",
            async: false,
            url: Granite.HTTP.externalize("/libs/granite/security/currentuser.json")
        });
        if (result.status === 200) {
            currentUserId = JSON.parse(result.responseText).authorizableId;
        }
        return currentUserId;
      }
    }
}(jQuery, jQuery(document)));