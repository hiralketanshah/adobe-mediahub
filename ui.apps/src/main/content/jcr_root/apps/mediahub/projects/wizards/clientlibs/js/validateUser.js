(function ($, Granite, undefined) {
    $(window).adaptTo("foundation-registry").register("foundation.validation.validator", {
        selector: '.project-email',
        validate: function(e) {
        var value = e.value;
        if(null!=value.match(/@[A-Za-z]*\.[A-Za-z]*$/g)){
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