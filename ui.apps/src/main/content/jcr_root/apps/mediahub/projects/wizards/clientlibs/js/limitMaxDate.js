(function ($, $document) {
    "use strict";
    $(document).on("change", "coral-datepicker#externalUserDatepicker", function (e) {
        var calendar = e.target;
        var value = moment(calendar.value, calendar.valueFormat);
        if (value.isAfter(moment().add(1, 'y'))) {
            alert("Expiration date must be less than a year");
            calendar.valueAsDate = moment().add(364, 'd').toDate();
        }
    });
})($, $(document));