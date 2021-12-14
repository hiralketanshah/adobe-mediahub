/*
 * ADOBE CONFIDENTIAL
 *
 * Copyright 2015 Adobe Systems Incorporated
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
(function(document, $) {
    "use strict";

    var actSuccessModal;


    var actFailureModal;

    $(document).on("foundation-contentloaded", init);

    function init() {
        var userID = getCurrentUserId();
        if($("#internalUserContact") && $("#internalUserContact").find("input[type=text]") && ($("#internalUserContact").find("input[type=text]").length > 0) ){
            if($("#internalUserContact").find("input[type=text]")[0].value === "tobereplaced"){
                $("#internalUserContact").find("input[type=text]")[0].value = userID;
                $("#internalUserContact").find("input[name=user]")[0].value = userID;
            }
        }
    }

    function getCurrentUserId(){
        var userID;
        $.ajax( {
            url: "/libs/cq/security/userinfo.json",
            async: false
        } ).done(handler);

        function handler(data){
            if(!data || !data.userID){
                return;
            }
            userID = data.userID;
        }
        return userID;
    }

    function downloadAssets(e) {
        var container = $(e.target).closest("#downloadasset");
        var path = window.sessionStorage.damAssetDownloads.replace(/path=/g, "").split("&");
        // decode path before using in this phase
        var i;
        var assetPaths;

        for (i = 0; i < path.length; i++) {
            path[i] = decodeURIComponent(path[i]);
        }
        if (window.sessionStorage.acceptedLicenseAssetPaths) {
            assetPaths = window.sessionStorage.acceptedLicenseAssetPaths.replace(/path=/g, "").split("&");
            _sendRequestForAcceptedOrRejectedLicenseEvent(assetPaths, "ACCEPTED");
        }
        if (window.sessionStorage.rejectedLicenseAssetPaths) {
            assetPaths = window.sessionStorage.rejectedLicenseAssetPaths.replace(/path=/g, "").split("&");
            _sendRequestForAcceptedOrRejectedLicenseEvent(assetPaths, "REJECTED");
        }

        var filename = $("#dam-asset-download-title").val();
        var patharr;
        patharr = [];
        for (i = 0; i < path.length; i++) {
            patharr.push(encodeURIComponent(path[i]));
        }
        var url = path[0] + ".assetdownload.zip/";
        // When email option is selected, we do not need any encoding of filename as it is only
        // used in email sent to the recipient with all the special characters, including spaces, truncated.
        var downloadViaEmail = false;
        if ($(".email [type='checkbox']", container).prop("checked")) {
            downloadViaEmail = true;
            url = url + encodeURIComponent(filename.replace(/ /g, "").replace(/\(|\)/g, ""));
        } else {
            url = url + encodeURIComponent(filename).replace(/%2F/g, "/");
        }
        url = Granite.HTTP.externalize(url);



        var params = {
            "path": patharr,
            "_charset_": encodeURIComponent("utf-8"),
            "downloadAssets": encodeURIComponent($(".asset-select [type='checkbox']", container).prop("checked")),
            "downloadRenditions": encodeURIComponent($(".rendition-select [type='checkbox']", container)
                .prop("checked")),
            "downloadSubassets": encodeURIComponent($(".subasset-select [type='checkbox']", container).prop("checked")),
            "flatStructure": encodeURIComponent(!($(".flatstructure-select [type='checkbox']", container)
                .prop("checked"))),
            "licenseCheck": encodeURIComponent("true")
        };

        // s7dam export with dymanic settings
        var s7ExportSettings = $("#downloadasset").data("s7exportsettings");
        if (typeof s7ExportSettings !== "undefined" && s7ExportSettings !== null) {
            // s7ExportSettings is encoded in the addParameter() method
            params["s7exportsettings"] = encodeURIComponent(s7ExportSettings);
        }
        var users = $("#downloadasset").data("emails");
        if (!downloadViaEmail || users === "" || users === undefined) {
            window.sessionStorage.removeItem("nonLicensedAssetPaths");
            post(url, params, function() {
                if (checkIfAssetLicenseSelectionPage()) {
                    window.location.href = $("#licenseselection .foundation-wizard-control").attr("href");
                }
            });
        } else {
            // email parameter actually contains the list of userIDs selected to receive download link in their email.
            // parameter name email is used to ensure backwards compatibility.
            // parameter decodedIDs is used to ensure no decoding is done for new code on backend.
            params["email"] = users;
            params["decodedIDs"] = true;
            $.ajax({
                type: "POST",
                url: url,
                data: params,
                success: function(data) {
                    if (!actSuccessModal) {
                        actSuccessModal = new Coral.Dialog().set({
                            id: "downloadasset-processed",
                            variant: "success",
                            closable: "on",
                            header: {
                                innerHTML: Granite.I18n.get("Download")
                            },
                            content: {
                                innerHTML: Granite.I18n.get("Your download job is starting and you will be notified when it is complete.") // eslint-disable-line max-len
                            },
                            footer: {
                                innerHTML: '<button is="coral-button" class="closeExport" variant="default" ' +
                                           "coral-close>" + Granite.I18n.get("Close") + "</button>"
                            }
                        }).on("coral-overlay:close", function(event) {
                            if (checkIfAssetLicenseSelectionPage()) {
                                window.location.href = $("#licenseselection .foundation-wizard-control").attr("href");
                            }
                        });
                    }
                    actSuccessModal.show();
                },
                error: function(data) {
                    if (!actFailureModal) {
                        actFailureModal = new Coral.Dialog().set({
                            id: "downloadasset-failed",
                            variant: "error",
                            closable: "on",
                            header: {
                                innerHTML: Granite.I18n.get("Download")
                            },
                            content: {
                                innerHTML: Granite.I18n.get("Download job couldn't be triggered. Contact administrator for more details.") // eslint-disable-line max-len
                            },
                            footer: {
                                innerHTML: '<button is="coral-button" class="closeExport" variant="default" ' +
                                           "coral-close>" + Granite.I18n.get("Close") + "</button>"
                            }
                        });
                    }
                    if (data && data.responseText) {
                        var noEmailUsers = JSON.parse(data.responseText).noEmailUsers;
                        if (noEmailUsers) {
                            var dialogContent = actFailureModal.content;
                            dialogContent.innerHTML = Granite.I18n.get("Download job couldn't be triggered. No email configured for following {0} user(s):", noEmailUsers.length); // eslint-disable-line max-len
                            dialogContent.appendChild(function() {
                                var para = document.createElement("p");
                                $.each(noEmailUsers, function(i, user) {
                                    para.appendChild(document.createTextNode(user));
                                    para.appendChild(document.createElement("br"));
                                });
                                para.appendChild(document.createElement("br"));
                                return para;
                            }());
                        }
                    }
                    actFailureModal.show();
                }
            });
        }

        // MED-347
        if($("#internalUserContact") || $("#useTextArea") || $("#dateOfUse") || $("#geographicalarea")){
            if($("#internalUserContact") && $("#internalUserContact").find("input[type=text]") && ($("#internalUserContact").find("input[type=text]").length > 0) ){
              params["internalUserContact"] = $("#internalUserContact").find("input[name=user]")[0].value;
            }

            if($("#useTextArea")){
              params["useTextArea"] = $("#useTextArea").val();
            }

            if($("#dateOfUse")){
              params["dateOfUse"] = $("#dateOfUse").find("input[is=coral-textfield]").val();
            }

            if($("#geographicalarea") && $("#geographicalarea").find("coral-tag")){
                if($("#geographicalarea").find("coral-tag").length > 0){
                    var elements = $("#geographicalarea").find("coral-tag");
                    var values = [];
                    for (var i=0; i<elements.length; i++) {
                      values[i] = elements[i].value;
                    }
                    params["geographicalarea"] = values;
                }
            }

            $.ajax({
                type: "POST",
                url: "/bin/mediahub/downloadmetadata",
                data: params
            });
        }
    }

    function _sendRequestForAcceptedOrRejectedLicenseEvent(acceptedOrRejectedLicenseAssetPaths, eventName) {
        $.ajax({
            type: "POST",
            url: Granite.HTTP.externalize("/libs/dam/drm"),
            data: {
                "_charset_": "utf-8",
                "assetPaths": acceptedOrRejectedLicenseAssetPaths,
                ":agreement": eventName
            }
        });
    }

    $("#exportBtn").on("click", function(e) {
        downloadAssets(e);
    });

    $("#downloadasset").on("change", function(event) {
        var totalChecked = $(".row").find(":first[checked='checked']").size();
        if ($(".flatstructure-select:first[checked='checked']").size() === 1) {
            totalChecked -= 1;
        }
        var emailListOptionsCount = $(".downloadOptionValidationRequired")[0].values.length;
        var emailChecked = $(".emailCheckBox:first[checked='checked']").size();
        if (totalChecked === 0 || (emailChecked > 0 && emailListOptionsCount === 0)) {
            $("#exportBtn").attr("disabled", "disabled");
        } else {
            $("#exportBtn").removeAttr("disabled");
        }

        if($("#internalUserContact") && $("#useTextArea") && $("#dateOfUse") && $("#geographicalarea")){
            if( ($("#geographicalarea").find("coral-tag") && $("#geographicalarea").find("coral-tag").length > 0) && ($("#dateOfUse").find("input[is=coral-textfield]").val() !== "") && ($("#useTextArea").val() !== "") && ($("#internalUserContact").find("input[name=user]").val() !== "") ){
                $("#exportBtn").removeAttr("disabled");
            } else {
                $("#exportBtn").attr("disabled", "disabled");
            }
        }

    });

    function checkIfAssetLicenseSelectionPage() {
        var loc = window.location.pathname;
        return loc.match("/licensecheck.external.html");
    }

    // Post to the provided URL with the specified parameters.
    function post(path, parameters, callback) {
        var form = $("<form></form>");
        form.attr("method", "post");
        form.attr("action", path);
        form.attr("target", "_blank");
        $.each(parameters, function(key, value) {
            if ($.isArray(value)) {
                $.each(value, function(keyArray, valueArray) {
                    var field = $("<input></input>");
                    field.attr("type", "hidden");
                    field.attr("name", key);
                    field.attr("value", valueArray);
                    form.append(field);
                }
                );
            } else {
                var field = $("<input></input>");
                field.attr("type", "hidden");
                field.attr("name", key);
                field.attr("value", value);
                form.append(field);
            }
        });
        // The form needs to be a part of the document in
        // order for us to be able to submit it.
        $(document.body).append(form);
        form.submit();
        setTimeout(callback, 100);
    }
})(document, Granite.$);
