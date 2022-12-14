/*
 * ADOBE CONFIDENTIAL
 *
 * Copyright 2016 Adobe Systems Incorporated
 * All Rights Reserved.
 *
 * NOTICE:  All information contained herein is, and remains
 * the property of Adobe Systems Incorporated and its suppliers,
 * if any.  The intellectual and technical concepts contained
 * herein are proprietary to Adobe Systems Incorporated and its
 * suppliers and may be covered by U.S. and Foreign Patents,
 * patents in process, and are protected by trade secret or copyright law.
 * Dissemination of this information or reproduction of this material
 * is strictly forbidden unless prior written permission is obtained
 * from Adobe Systems Incorporated.
 */
(function(window, document, Granite, $, URITemplate) {
    "use strict";
    moment.locale(Granite.I18n.getLocale());

    window.CQ = window.CQ || {};
    window.CQ.Projects = window.CQ.Projects || {};
    window.CQ.Projects.UI = window.CQ.Projects.UI || {};

    window.CQ.Projects.UI.init = function() {
        var aeon = new Aeon.Agenda();

        initializeCalendar(aeon);
        setCalendarStatuses(aeon);
        setCalendarFields(aeon);
        setCalendarActions(aeon);
        setCalendarGroups(aeon);
        handleCookies();
        hideAeonButtons();
        setupDataReadyEvent(aeon);

        aeon.initialize();
    };

    //Create the calendar object and setup its basic configurations
    function initializeCalendar (aeon) {
        var projectMode = $.cookie("aeon-cal-project-view");
        if(projectMode == null) {
            projectMode = "list";
        }

        $("#CalendarWrapper").append(aeon);
        $('#CalendarWrapper .aeon-Agenda').addClass("cq-project-calendarview");

        var startDate = $.cookie("aeon-cal-project-start");
        if(startDate == null || !isDate(startDate)) {
            var interimDate = moment(new Date());
            interimDate.subtract(365, "days");  //display 365 days prior to today
            startDate = interimDate.format("YYYY-MM-DD");
        }

        var endDate = $.cookie("aeon-cal-project-end")
        if(endDate == null || !isDate(endDate)) {
            var currentDate = moment(new Date());
            currentDate.add(21, "days");
            var endDate = currentDate.format("YYYY-MM-DD");  //display 21 days after today
        }

         aeon.set({
            url: '/libs/cq/core/content/projects/calendarview/calendardatanode.json',
            cardSize: $('#projectCardSize').val(),
            mode: projectMode,
            groupBy: $('#projectGrouping').val(),
            cardDateSpecifier: 'YYYY-MM-DD',
            start: startDate,
            end: endDate,
            baseFilters: {
                calItemActive: $('#calState').val(),
                projectPath: '',
                type: 'post'
            },
            serviceTransformer: function(payload) {
                return payload;
            }
        });
    }

    //Setup the various statuses to be supported by the calendar
    //In Progress, Planned, Due Soon and Past Due
    function setCalendarStatuses (aeon) {
        aeon.statuses.add(
            'active',
            {
                icon: 'clock',
                color: "#87beff",
                label: Granite.I18n.get("In Progress"),
                dateLabel: Granite.I18n.get("Start Date: ")
            }
        );

        aeon.statuses.add(
            'activePlanned',
            {
                icon: 'clock',
                color: "#a5d273",
                label: Granite.I18n.get("Planned"),
                dateLabel: Granite.I18n.get("Start Date: ")
            }
        );
        aeon.statuses.add(
            'activeWarning',
            {
                icon: 'clock',
                color: "#fad269",
                label: Granite.I18n.get("Due Soon"),
                dateLabel: Granite.I18n.get("Start Date: ")
            }
        );
        aeon.statuses.add(
            'activeLate',
            {
                icon: 'clock',
                color: "#fa7d73",
                label: Granite.I18n.get("Past Due"),
                dateLabel: Granite.I18n.get("Start Date: ")
            }
        );
    }

    //Add fields to the calendar cards
    function setCalendarFields (aeon) {
        aeon.fields.add({}, {
            content: function (entry) {
                if(entry.status != undefined && entry.status != null) {

                    var displayStatus;
                    switch(entry.status) {
                        case "activeLate":
                            displayStatus = Granite.I18n.get("Past Due");
                            break;
                        case "activePlanned":
                            displayStatus = Granite.I18n.get("Planned");
                            break;
                        case "activeWarning":
                            displayStatus = Granite.I18n.get("Due Soon");
                            break;
                        default:
                            displayStatus = Granite.I18n.get("In Progress");
                    }
                    return "<b>" + Granite.I18n.get("Status:") + "</b> " + displayStatus;
                }
                return "<b>"+Granite.I18n.get("Status:")+"</b> "+Granite.I18n.get("NA");
            }
        });

        aeon.fields.add({}, {
            content: function (entry) {
                if(entry.endingAt != undefined && entry.endingAt != null)
                    return "<b>"+Granite.I18n.get("Due:")+"</b> " + moment(parseInt(entry.endingAt)*1000).format("YYYY-MM-DD");

                return "<b>"+Granite.I18n.get("Due:")+"</b> "+Granite.I18n.get("NA");
            }
        });

        aeon.fields.add({}, {
            content: function (entry) {
                if(entry.startingAt != undefined && entry.startingAt != null)
                    return "<b>"+Granite.I18n.get("Start:")+"</b> " + moment(parseInt(entry.startingAt)*1000).format("YYYY-MM-DD");

                return "<b>"+Granite.I18n.get("Start:")+"</b> "+Granite.I18n.get("NA");
            }
        });
    }

    //setup Aeon quicka actions and their handlers
    function setCalendarActions (aeon) {
        aeon.actions.add({},
            {
                name: Granite.I18n.get("View Properties"),
                icon: 'infoCircle',
                fn: function (payload) {
                    var href = Granite.HTTP.getContextPath() + payload.linkInfo;
                    window.location=href;
                }
            }
        );

        aeon.actions.add({},
            {
                name: Granite.I18n.get("Project"),
                icon: 'project',
                fn: function (payload) {
                    var href = Granite.HTTP.getContextPath() + payload.link;
                    window.location=href;
                }
            }
        );

        aeon.actions.add({},
            {
                name: Granite.I18n.get("Tasks"),
                icon: 'pasteList',
                fn: function (payload) {
                    var path = payload.deletePath;  //this item should be renamed to project path
                    window.location= Granite.HTTP.getContextPath() + "/aem/inbox.html" + path;
                }
            }
        );


        aeon.actions.add({},
            {
                name: Granite.I18n.get("Delete"),
                icon: 'delete',
                fn: function (payload) {
                    var dialog = document.querySelector('#calendar-delete-project-dialog');

                    dialog.onsubmit = function(){
                        window.location.reload(true);
                    }

                    var $dialog = $(dialog);

                    //check if the dialog exists.  Initial load won't have it yet

                    //var selectedItems = $(".foundation-collection .foundation-selections-item");
                    var $form = $dialog.find("#cq-project-delete-projects-form");
                    var formHTML = $form.html();

                    var inputList = [];
                    inputList.push('<input type="hidden" name="path" value="' + payload.deletePath + '" />');

                    $form.html(inputList.join("") + formHTML);

                    var $modal = $(dialog);
                    var intro = $modal.find(".delete-projects-intro");
                    var introText = intro.data("templateSingle");

                    var list = [];

                    list.push("<b>" + payload.title + "</b>");


                    intro.text(Granite.I18n.get(introText, "1"));
                    $modal.find(".projects").html("<br><br>" + list.join("<br>"));

                    dialog.show();
                }
            }
        );
    }

    //define groups.  2 groups are defined: 'Schedule' which contains
    //          statuses In Progress, Planned, Due Soon and Past Due
    //and 'Status' which contains 'Active' and 'In-Active'
    function setCalendarGroups (aeon) {
        aeon.groups.add(Granite.I18n.get("Schedule"),
            [
                {
                    label: Granite.I18n.get("Planned"),
                    criteria: {
                        metadata: {
                            platform: 'planned'
                        }
                    }
                },
                {
                    label: Granite.I18n.get("In Progress"),
                    criteria: {
                        metadata: {
                            platform: 'ontime'
                        }
                    }
                },
                {
                    label: Granite.I18n.get("Due Soon"),
                    criteria: {
                        metadata: {
                            platform: 'warning'
                        }
                    }
                },
                {
                    label: Granite.I18n.get("Past Due"),
                    criteria: {
                        metadata: {
                            platform: 'late'
                        }
                    }
                }
            ]
        );
    }

    // remove the specified cookie and replace it with a new value
    function setCalendarCookie(cookie, value) {
        $.removeCookie(cookie);
        // set the cookie with path '/' to ensure a unique value when read
        $.cookie(cookie, value, { path: '/'});
    }

    //setup events that persist user selections in the calendar for a given session
    function handleCookies () {
        //handle date filter selection
        $('.aeon-DatePopover-content').on("click", function(e) {
            if(e.target.getAttribute('handle') != 'reset') {
                // @coral usage of internals
                var startRange = moment($('.aeon-DateRangePicker--start span[handle="daterangedisplay"]').text());
                var endRange = moment($('.aeon-DateRangePicker--end span[handle="daterangedisplay"]').text());

                setCalendarCookie("aeon-cal-project-start", startRange.format("YYYY-MM-DD"));
                setCalendarCookie("aeon-cal-project-end", endRange.format("YYYY-MM-DD"));
            }
        });
  
        // @coral usage of internals
        $('[handle="timeline"]').on("click", function(e) {
            setCalendarCookie("aeon-cal-project-view", "timeline");
            //because the groupby does not have a unique id we need to match the selected
            //group with the last item in the list which will be scheduled. The group name changes
            //to the selected language so this is why we compare the selected string with the item
            //we know to be the 'Scheduled' group in the drop down.
            if ($('aeon-settings').attr('groupby') ==
                $($('aeon-settings coral-select[handle="group"] coral-select-item')[1]).text()) {
                $('#projectCalFilter').show();
            } else {
                $('#projectCalFilter').hide();
            }
        });
  
        // @coral usage of internals
        $('[handle="column"]').on("click", function(e) {
            setCalendarCookie("aeon-cal-project-view", "column");
            if ($('aeon-settings').attr('groupby') ==
                $($('aeon-settings coral-select[handle="group"] coral-select-item')[1]).text()) {
                $('#projectCalFilter').show();
            } else {
                $('#projectCalFilter').hide();
            }
        });
  
        // @coral usage of internals
        $('[handle="list"]').on("click", function(e) {
            setCalendarCookie("aeon-cal-project-view", "list");
            $('#projectCalFilter').hide();
        });
    }

    //hide Calendar preference buttons that we don't use
    function hideAeonButtons () {
        //hide Calendar preference buttons that we don't use
        $($('.aeon-Actions .aeon-PaneButton')[0]).hide();
        $($('.aeon-Actions .aeon-PaneButton')[1]).hide();
        $($('.aeon-Actions .aeon-PaneButton')[2]).hide();
        $($('.aeon-Actions .aeon-PaneButton')[3]).hide();
    }

    //When the client receives data from our dataprovider Aeon triggers an event
    //and returns an object with all the data
    //Currently we use this to setup our filter buttons and add group counts
    function setupDataReadyEvent (aeon) {
        aeon.on("aeon:data:loaded", function(e) {

            var countTot = e.detail.payload.meta.count;
            var countActive = e.detail.payload.meta.activeCount;
            var countLate = e.detail.payload.meta.activeLateCount;
            var countPlan = e.detail.payload.meta.activePlannedCount;
            var countWarn = e.detail.payload.meta.activeWarningCount;

            //check to see if button bar exists
            if ($("#projectCalFilter").length < 1) {
                $('<coral-buttongroup id="projectCalFilter" selectionmode="multiple" class="calendarStatusFilter" name="button-group">' +
                    '<button is="coral-button" icon="clock" id="aem-cal-all" class="calendar-filter-button">' + Granite.I18n.get("All ({0})", countTot.toString()) + '</button>' +
                    '<button is="coral-button" icon="clock" id="aem-cal-planned" class="calendar-filter-button">' + Granite.I18n.get("Planned ({0})", countPlan.toString()) + '</button>' +
                    '<button is="coral-button" icon="clock" id="aem-cal-progress" class="calendar-filter-button">' + Granite.I18n.get("In Progress ({0})", countActive.toString()) + '</button>' +
                    '<button is="coral-button" icon="clock" id="aem-cal-soon" class="calendar-filter-button">' + Granite.I18n.get("Due Soon ({0})", countWarn.toString()) + '</button>' +
                    '<button is="coral-button" icon="clock" id="aem-cal-due" class="calendar-filter-button">' + Granite.I18n.get("Past Due ({0})", countLate.toString()) + '</button></coral-buttongroup>')
                    .insertAfter($('.aeon-NavButtons'));

                //add click events on filter buttons
                setFilterButtons();

            } else {
                //update totals on button bar
                $("#aem-cal-all coral-button-label").text(Granite.I18n.get("All ({0})", countTot.toString()));
                $("#aem-cal-planned coral-button-label").text(Granite.I18n.get("Planned ({0})", countPlan.toString()));
                $("#aem-cal-progress coral-button-label").text(Granite.I18n.get("In Progress ({0})", countActive.toString()));
                $("#aem-cal-soon coral-button-label").text(Granite.I18n.get("Due Soon ({0})", countWarn.toString()));
                $("#aem-cal-due coral-button-label").text(Granite.I18n.get("Past Due ({0})", countLate.toString()));
            }

            if($.cookie("aeon-cal-project-view") == "list"){
                $('#projectCalFilter').hide();
            } else {
                if ($(".aeon-ResizeContainer").length > 0) {
                    $('#projectCalFilter').show();
                }
                else {
                    $('#projectCalFilter').hide();
                }
            }

        });
    }

    //click event on client side filter buttons
    function setFilterButtons () {
        // initial state is "All" selected
        $("#aem-cal-all").attr("selected", true);

        $("#aem-cal-planned").click(function () {
            $("#aem-cal-planned").toggleClass("calendar-filter-button-selected");
            updateFilterButtons();
        });
        $("#aem-cal-progress").click(function () {
            $("#aem-cal-progress").toggleClass("calendar-filter-button-selected");
            updateFilterButtons();
        });
        $("#aem-cal-soon").click(function () {
            $("#aem-cal-soon").toggleClass("calendar-filter-button-selected");
            updateFilterButtons();
        });
        $("#aem-cal-due").click(function () {
            $("#aem-cal-due").toggleClass("calendar-filter-button-selected");
            updateFilterButtons();
        });

        $("#aem-cal-all").click(function () {
            if ($(".aeon-ResizeContainer").length > 0) {
                // Show all containers
                $(".aeon-ResizeContainer").show();
            }

            // keep the individual filter button statuses unselected
            $("#aem-cal-planned").removeClass("calendar-filter-button-selected");
            $("#aem-cal-progress").removeClass("calendar-filter-button-selected");
            $("#aem-cal-soon").removeClass("calendar-filter-button-selected");
            $("#aem-cal-due").removeClass("calendar-filter-button-selected");

            $("#aem-cal-planned").attr("selected", false);
            $("#aem-cal-progress").attr("selected", false);
            $("#aem-cal-soon").attr("selected", false);
            $("#aem-cal-due").attr("selected", false);
        });
    };

    function updateFilterButtons () {
        // Clicking the "All" button will reset the individual selection status for all buttons,
        // so we need to check all of them whenever any one of them is individually selected
        $("#aem-cal-all").attr("selected", false);

      if ($(".aeon-ResizeContainer").length > 0) {
            // Tasks are grouped by schedule
            if ($("#aem-cal-planned").hasClass("calendar-filter-button-selected") == true)
                $($(".aeon-ResizeContainer")[0]).show();
            else
                $($(".aeon-ResizeContainer")[0]).hide();

            if ($("#aem-cal-progress").hasClass("calendar-filter-button-selected") == true)
                $($(".aeon-ResizeContainer")[1]).show();
            else
                $($(".aeon-ResizeContainer")[1]).hide();

            if ($("#aem-cal-soon").hasClass("calendar-filter-button-selected") == true)
                $($(".aeon-ResizeContainer")[2]).show();
            else
                $($(".aeon-ResizeContainer")[2]).hide();

            if ($("#aem-cal-due").hasClass("calendar-filter-button-selected") == true)
                $($(".aeon-ResizeContainer")[3]).show();
            else
                $($(".aeon-ResizeContainer")[3]).hide();
        }
    };

    function isDate(date) {
        return (new Date(date) !== "Invalid Date") && !isNaN(new Date(date));
    }

})(window, document, Granite, Granite.$, Granite.URITemplate);

