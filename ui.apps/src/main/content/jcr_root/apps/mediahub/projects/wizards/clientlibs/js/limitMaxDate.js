(function (Granite, $) {
    var ui = $(window).adaptTo("foundation-ui");
    $(document).on("change", "coral-datepicker#externalUserDatepicker", function (e) {
        var calendar = e.target;
        var value = moment(calendar.value, calendar.valueFormat);
        if (value.isAfter(moment().add(1, 'y'))) {
            ui.alert(Granite.I18n.get("Error"), Granite.I18n.get("Expiration date must be less than a year from today"), 'error');
            calendar.valueAsDate = moment().add(364, 'd').toDate();
        }
    });
})(Granite, Granite.$);