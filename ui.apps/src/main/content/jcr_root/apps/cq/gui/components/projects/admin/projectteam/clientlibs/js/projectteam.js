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
var CQ = CQ || {};
CQ.Projects = function() {
    return {

        /**
         * Add a team member to a parent
         * @param $parent the parent container.
         * @param data data representing the team member. This data is not HTML encoded and will be HTML encoded
         *             by this function prior to being processed by the team member template.
         */
        addTeamMember: function(target, data, readOnly) {
            data.name = data.name || data.userId;

            if (data.role === undefined || data.role === "") {
                data.role = data.roleId;
            }
            data.isnew = (readOnly) ? "existing" : "newentry";

            if (typeof data['allowRemove'] === 'undefined') {
                data.allowRemove = true;
            }

            if (data.avatar === undefined || data.avatar === null || data.avatar === "") {
                data.avatar = "/libs/granite/security/clientlib/themes/default/resources/sample-user-thumbnail.36.png";
            }

            if (data.email === undefined || data.email === null) {
                data.email = "";
            }

            var $teamTable = target;
            $teamTable.find("input[name='teamMemberUserId']").filter(function() {
                return $(this).val() === data.userId;
            }).closest("tr").hide().addClass("modified").find("input").attr("disabled", "disabled");

            // html encode the data
            $.each(data, function(key, val) {
                if ( "string" === typeof val ) {
                    data[key] = $("<div/>").text(val).html();
                }
            });

            // create the inner HTML for the new table row
            var td = "<td class=\"avatar\"><img src=\"" + data.avatar + "\"></td>";

            td += "<td class=\"name\"><span>" + data.name + "</span>";
            td += "<input type=\"hidden\" name=\"teamMemberUserId\" value=\"" + data.userId + "\"></td>";

            td += "<td class=\"email\"><span class=\"greyText\">" + data.email + "</span></td>";

            td += "<td class=\"role greyText\"><span>" + data.role + "</span>";
            td += "<input type=\"hidden\" name=\"teamMemberRoleId\" value=\"" + data.roleId + "\"></td>";

            td += "<td class=\"path\"/>";

            td += "<td class=\"remove\">";
            if (data.allowRemove) {
                td += "<button is=\"coral-button\" type=\"button\" variant=\"quiet\" ";
                td += "class=\"foundation-field-edit project-members-remove-user\" ";
                td += "title=\"" + Granite.I18n.get("Remove") + "\" data-dismiss=\"modal\">";
                td += "<i class=\"coral-Icon coral-Icon--sizeXS coral-Icon--delete\"></i>"
                td += "</button>";
            }

            // create the table row and append it to the team table
            var tr = document.createElement('tr');
            tr.setAttribute("class", data.isnew);
            tr.innerHTML = td;
            $teamTable.append(tr);
        }
    }
}();

(function(window, document, Granite, $, undefined) {
    var ns = ".cq-projects-admin-projectteam";
    var PROJECT_MEMBER_ADD_BTN =       ".project-members-add-user";
    var PROJECT_MEMBER_REMOVE_BTN =    ".project-members-remove-user";

    $(document).on("click", PROJECT_MEMBER_REMOVE_BTN, function(ev) {
        $(this).closest("tr")
                  .hide()
                  .find("input")
                  .attr("disabled", "disabled");

        $(".js-coral-Autocomplete-textfield").trigger("change");

        // if no members remain, disable the save button
        var visibleRowCount = $('.team-table tr:visible').length;
        if (visibleRowCount == 0) {
            $("#shell-propertiespage-saveactivator").attr("disabled", "disabled");
        }
    });

    // handle adding a new member
    $(document).on("click", PROJECT_MEMBER_ADD_BTN, function(ev) {
        var userId = $(".collection-settings-userpicker-id").data("autocomplete").getValue();
        if (userId) {
            var options = {
                url: Granite.HTTP.externalize("/libs/cq/core/content/projects/wizard/steps/defaultproject/items/column2/items/tabs/items/basic/items/projectmembers.userinfo.json?userid="+userId+"&_charset_=UTF-8"),
                type: "GET",
                success: function(data, textStatus, jqXHR) {
                    data.allowEdit = false;

                    var $member = $("#member-role");
                    if ($member.length>0) {
                        var member = $member[0];
                        if (member.selectedItem) {
                            data.roleId = member.value;
                            data.role = member.selectedItem.innerText;

                            // add new member to the table
                            CQ.Projects.addTeamMember($(".team-table tbody"), data, false);

                            // if this is the only member, enable the save button
                            var visibleRowCount = $('.team-table tr:visible').length;
                            if (visibleRowCount == 1) {
                                $("#shell-propertiespage-saveactivator").attr("disabled", null);
                            }

                            // clear out the user picker text
                            //$(".collection-settings-userpicker-id input[type='text']").val("");
                            $(".collection-settings-userpicker-id").find("input").val("");
                            $(".collection-settings-userpicker-id").data("autocomplete")._lastSelected = "";
                        }
                    }
                }
            };
            $.ajax(options);
        }
    });

    // handler for resetting the form
    $(document).on("reset", "form", function(e, mode, group) {

        $(".team-table tr.newentry").each(function(index) {
            $(this).remove();
        });

        $(".team-table tr.modified").each(function(index) {
            $(this).remove();
        });

        $(".team-table tr:hidden").each(function(index) {
            $(this).show()
                .find("input")
                .attr("disabled", null);
        });

        $(".collection-settings-userpicker-id input[type='text']").val("");

        // make sure we turn off our content loaded listener for project otherwise
        // we will get duplicate results
        $(document).off("foundation-contentloaded.cq-projects-admin-properties");
    });

})(window, document, Granite, Granite.$);