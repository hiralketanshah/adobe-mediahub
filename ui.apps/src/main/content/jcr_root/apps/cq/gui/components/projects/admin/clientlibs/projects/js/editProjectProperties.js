/*
 * ADOBE CONFIDENTIAL
 * __________________
 *
 *  Copyright 2013 Adobe Systems Incorporated
 *  All Rights Reserved.
 *
 * NOTICE:  All information contained herein is, and remains
 * the property of Adobe Systems Incorporated and its suppliers,
 * if any.  The intellectual and technical concepts contained
 * herein are proprietary to Adobe Systems Incorporated and its
 * suppliers and are protected by trade secret or copyright law.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from Adobe Systems Incorporated.
 */

(function($, undefined) {
    "use strict";

    var ui = $(window).adaptTo("foundation-ui");

    var ns = ".cq-projects-admin-properties";

    var successTemplate = $(
        '<div class="modal"  data-type="success">' +
            '<div class="modal-header"><h2></h2></div>' +
            '<div class="modal-body"></div>' +
            '<div class="modal-footer"></div>' +
            '</div>');

    var errorTemplate = $(
        '<div class="modal" data-type="error">' +
            '<div class="modal-header">' +
            '<h2></h2>' +
            '<button class="close" type="button" data-dismiss="modal">&times;</button>' +
            '</div>' +
            '<div class="modal-body"></div>' +
            '</div>');


    var submit = function($form) {

        ui.wait();

        $(".team-table tr:hidden", $form).each(function(index) {
            $(this).remove();
        });

        var ajaxOptions = {
            url: $form.prop("action"),
            type: "post"
        };
        var jqxhr = CQ.projects.ajaxSubmitFileForm(ajaxOptions, $form);

        jqxhr.done(function(html) {
            ui.clearWait();
            var $html = $(html);

            var modal = successTemplate.clone();
            modal.find(".modal-header h2").html($html.find(".foundation-form-response-title").next().html());
            modal.find(".modal-body").html($html.find(".foundation-form-response-description").next().html());

            var footer = modal.find(".modal-footer");
            var redirect = $html.find("a.foundation-form-response-redirect");

            $('<a class="button primary" />')
                .prop("href", redirect.prop("href"))
                .text(redirect.text())
                .appendTo(footer);

            modal.on("click", ".button.primary", function() {
                window.location = redirect.prop("href");
            });

            modal.appendTo("body").modal("show");
        }).fail(function(xhr, error, errorThrown) {
                ui.clearWait();
                var modal = errorTemplate.clone();

                if (error === "error") {
                    var $html = $(xhr.responseText);

                    if ($html.find(".foundation-form-response-status-code").length > 0) {
                        modal.find(".modal-header h2").html($html.find(".foundation-form-response-title").next().html());
                        modal.find(".modal-body").html($html.find(".foundation-form-response-description").next().html());

                        modal.appendTo("body").modal("show");
                        return;
                    }
                }

                modal.find(".modal-header h2").text(error);
                modal.find(".modal-body").text(errorThrown);

                modal.appendTo("body").modal("show");
            });
    };

    $(document).on("foundation-contentloaded", function(e) {

        var $form = $(ns);

        if (!$form.length) {
            return;
        }


        /*$form.off("submit")
            .on("submit" + ns, function(e) {
                e.preventDefault();
                submit($form);
            });*/
    });




})(Granite.$);


(function ($, $document) {
    "use strict";
       $document.on("foundation-contentloaded", function() {
       var d = new Date();
	   document.querySelectorAll('coral-datepicker[name="project.dueDate"]')[0].setAttribute("max", (d.getFullYear()+1) + "-" + (d.getMonth() +1)+ "-" + d.getDate());
       });

})($, $(document));



