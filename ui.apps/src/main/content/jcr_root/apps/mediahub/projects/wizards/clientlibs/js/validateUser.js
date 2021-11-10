(function ($, Granite, undefined) {

    function validateEmail(email) {
        const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        return re.test(String(email).toLowerCase());
    }

    $(window).adaptTo("foundation-registry").register("foundation.validation.validator", {
        selector: '.project-email',
        validate: function(e) {
        var value = e.value;
        if(validateEmail(value)){
            var userId;
            $.ajax( {
                url: "/apps/mediahub/projects/teammembers/datasource.userinfo.json?userid="+value,
                async: false
            } ).done(handler);
            function handler(data){
                userId = data.email;
            }
            if(userId){
                return Granite.I18n.get("External user already exists");
            }
        }
        else{
            return Granite.I18n.get("Email should be in correct format : example@example.com");
        }
    }
    });
})(Granite.$, Granite);