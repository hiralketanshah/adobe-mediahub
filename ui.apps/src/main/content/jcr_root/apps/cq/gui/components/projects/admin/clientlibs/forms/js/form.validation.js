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

(function ($, Granite, undefined) {
    "use strict";

    var requiredString;

    $(window).adaptTo("foundation-registry").register("foundation.validation.validator", {
        selector: '.project-duedate.coral-InputGroup',
        validate: validateDate
    });

    $(window).adaptTo("foundation-registry").register("foundation.validation.validator", {
        selector: '.project-startdate.coral-InputGroup',
        validate: validateDate
    });

    var registry = $(window).adaptTo("foundation-registry");
    registry.register("foundation.validation.validator", {
        selector: "input.cq-projects--required-nospaces",
        validate: function (element) {
            var el = $(element);

            var isRequired = element.required === true ||
                (element.required === undefined && el.attr("required") !== undefined) ||
                el.attr("aria-required") === "true";

            if (!isRequired) {
                return;
            }

            if (el.val().trim().length == 0) {
                errorDisableSubmit();
                return getRequiredString();
            } else {
                noErrorEnableSubmit();
            }
        }
    });


    function validateDate(el) {
        var setDateTime = el.valueAsDate;

        //date is not mandatory so return no error if it is null
        if(setDateTime == null)
            return "";

        var setDateObj = new Date(setDateTime);

        var currentDateObj = new Date();
        currentDateObj.setMinutes(currentDateObj.getMinutes() - 1);  // Subtract 1 minute to avoid slow javascript date comparisons

        if (el.name == 'project.startDate' || el.name == 'taskStartDate') {  // Do validation against End Date
            var setDueDate = getDateFromCollection($('.project-duedate.coral-InputGroup'));
            if (setDueDate == null) {
                // due date is not mandatory so return no error if it is null
                return "";
            }

            var setDueDateObj = new Date(setDueDate);
            if (setDateObj > setDueDateObj) {
                errorDisableSubmit();
                return Granite.I18n.get("Start Date must precede the Due Date");
            }
            // wait for the ui to remove error icons before checking if there are any still displaying
            setTimeout(startDateValidEnableSubmit, 250);
        }
        else { // it's the due date
            // Validate Date is greater than today
            if(setDateObj <= currentDateObj) {
                errorDisableSubmit();
                return Granite.I18n.get("Due Date cannot be in the past");
            }

            var setStartDate = getDateFromCollection($('.project-startdate.coral-InputGroup'));
            if (setStartDate == null) {
                // start date is not mandatory so return no error if it is null
                return "";
            }

            var setStartDateObj = new Date(setStartDate);
            if (setDateObj < setStartDateObj) {
                errorDisableSubmit();
                return Granite.I18n.get("Due Date must be newer than Start Date");
            }

            var plusone = new Date();
        	plusone.setFullYear(plusone.getFullYear() + 1);
            if(setDateObj > plusone) {
                errorDisableSubmit();
                return Granite.I18n.get("Due Date cannot be more than 1 year from Current Date");
            }

            // wait for the ui to remove error icons before checking if there are any still displaying
            setTimeout(dueDateValidEnableSubmit, 250);
        }
        return "";
    }

    function getDateFromCollection(collection) {
        if (collection.length > 0) {
            return collection[0].valueAsDate;
        } else {
            return null;
        }
    }

    function errorDisableSubmit() {
        $('#shell-propertiespage-saveactivator').prop('disabled', true);
        $('.betty-ActionBar-item.granite-ActionGroup').prop('disabled', true)
    }

    function noErrorEnableSubmit() {
        if ($('.coral-Icon--alert').length == 0) {
            $('#shell-propertiespage-saveactivator').prop('disabled', false);
            $('.betty-ActionBar-item.granite-ActionGroup').prop('disabled', false);
            return true;
        } else {
            return false;
        }
    }

    function startDateValidEnableSubmit() {
        if (!noErrorEnableSubmit()) {
            if ($('.project-duedate + .coral-Icon--alert').length > 0) {
                // there is currently an alert icon next to the due date input box, and the current
                // start date change might resolve that error, but we need to trigger another change
                // on the due date in order to invoke the validator that will remove the error status
                $('input[name="project.dueDate"]').change();
            }
        }
   }

    function dueDateValidEnableSubmit() {
        if (!noErrorEnableSubmit()) {
            if ($('.project-startdate + .coral-Icon--alert').length > 0) {
                // there is currently an alert icon next to the start date input box, and the current
                // due date change might resolve that error, but we need to trigger another change
                // on the start date in order to invoke the validator that will remove the error status
                $('input[name="project.startDate"]').change();
            }
        }
    }

    function getRequiredString() {
        if (!requiredString) {
            requiredString = Granite.I18n.get("Please fill out this field.");
        }
        return requiredString;
    }

})(Granite.$, Granite);