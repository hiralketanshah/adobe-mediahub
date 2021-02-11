(function(window, document, Granite, $, undefined) {
    $(document).off("click", '.project-members-add-user');
    $(document).on("click", '.project-members-add-user', function(ev) {
    	var userId = $(".collection-settings-userpicker-id").data("autocomplete").getValue();
        if (userId) {
            var options = {
                url: Granite.HTTP.externalize("/apps/mediahub/projects/teammembers/datasource.userinfo.json?userid="+userId+"&_charset_=UTF-8"),
                type: "GET",
                success: function(data, textStatus, jqXHR) {
                    data.allowEdit = false;

                    var $member = $("#member-role");
                    if ($member.length>0) {
                        var member = $member[0];
                        if (member.selectedItem) {
                            data.roleId = member.value;
                            data.role = member.selectedItem.innerText;

                            if(data.roleId === 'external-contributor'){
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
                            }else{
                                alert('External Users cannot be assigned to another project role');
                            }
                        }
                    }
                }
            };
            $.ajax(options);
        }
    });
})(window, document, Granite, Granite.$);