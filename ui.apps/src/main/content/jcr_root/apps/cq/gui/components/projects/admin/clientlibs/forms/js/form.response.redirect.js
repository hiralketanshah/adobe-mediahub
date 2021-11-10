/*
 ADOBE CONFIDENTIAL

 Copyright 2016 Adobe Systems Incorporated
 All Rights Reserved.

 NOTICE:  All information contained herein is, and remains
 the property of Adobe Systems Incorporated and its suppliers,
 if any.  The intellectual and technical concepts contained
 herein are proprietary to Adobe Systems Incorporated and its
 suppliers and may be covered by U.S. and Foreign Patents,
 patents in process, and are protected by trade secret or copyright law.
 Dissemination of this information or reproduction of this material
 is strictly forbidden unless prior written permission is obtained
 from Adobe Systems Incorporated.
 */

(function(window, $, URITemplate) {
    "use strict";

    $(window).adaptTo("foundation-registry").register("foundation.form.response.ui.success", {
        name: "projects.redirect",
        handler: function(form, config, data, textStatus, xhr) {

            var href = URITemplate.expand(config.href, data);

            if(data.description.includes("External User Creation")){

				var ui = $(window).adaptTo("foundation-ui");
				ui.prompt(Granite.I18n.get("Success"), Granite.I18n.get("External user created"), "success",
                    [{
                        text: Granite.I18n.get("Ok"),
                        handler: function () {
							window.location = href;
                    	}
                    }]
                );

            }
               else{
					window.location = href;

               }
        }
    });
})(window, Granite.$, Granite.URITemplate);