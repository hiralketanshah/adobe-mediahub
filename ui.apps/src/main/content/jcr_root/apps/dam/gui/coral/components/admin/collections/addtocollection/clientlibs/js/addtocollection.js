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
(function(window, document, Granite, $) {
    "use strict";

    var ns = ".cq-damadmin-admin-addcollection";
    var destinationPath;
    var destinationTitle;
    var addNewCollectionDialog = null;
    var addToCollectionSuccessDialogId = "addToCollectionSuccessDialog";
    var addToCollectionSuccessDialogCloseClass = "closeAddToCollectionSuccessDialog";

    // todo: response of the servlet should follow Granite's response format
    function _submit() {
        if (!destinationPath) {
            return;
        }
        var selectedItems = $(".destination-container").data("assetpath");
        var to = decodeURIComponent(destinationPath);
        var from = [];
        $.each(selectedItems, function(i) {
            // todo: check if it is needed
            from.push(selectedItems[i]);
        });

        $.ajax({
            type: "post",
            async: false,
            url: Granite.HTTP.externalize(to + ".collection.html"),
            data: {
                _charset_: "utf-8",
                ":operation": "add",
                "path": from
            }
        }).done(function(html) {
            $("#" + addToCollectionSuccessDialogId).remove();
            _showAddToCollectionSuccessDialog(selectedItems);
            _goToAppropriatePageOnClosingOfAddToCollectionSuccessDialog();
        }).fail(function(xhr, error, errorThrown) {
            var ui = $(window).adaptTo("foundation-ui");
            var responseText = xhr.responseText;
            var cyclicReferenceSpan = $(responseText).find("span:contains('cyclic-reference-loop-candidate')");
            var errorMessage;
            if (cyclicReferenceSpan.length !== 0) {
                var cyclicReferenceLoopCandidate = cyclicReferenceSpan.next().html();
                errorMessage = Granite.I18n.get("Adding collection {0} will result in a cyclic reference.",
                    [ cyclicReferenceLoopCandidate ], "Eg. Adding collection c1 will result in a cyclic reference.");
            } else {
                errorMessage = Granite.I18n.get("An error occurred while attempting to add to collection, you may not have permission to add to this collection.");// eslint-disable-line max-len
            }
            ui.alert(Granite.I18n.get("Error"), errorMessage, "error");
        });
    }

    function _getAddToCollectionSuccessMsg(selectedItems, title) {
        if (selectedItems.length > 1) {
            return Granite.I18n.get(
                "{0} assets were added to {1} collection",
                [ selectedItems.length, "<b>" + title + "</b>" ],
                "Eg. 4 assets were added to Retro Prints Collection."
            );
        } else {
            return Granite.I18n.get(
                "1 asset was added to {0} collection",
                [ title ],
                "Eg. 1 asset was added to Retro Prints Collection."
            );
        }
    }

    function _showAddToCollectionSuccessDialog(selectedItems) {
        var successModal = new Coral.Dialog().set({
            id: addToCollectionSuccessDialogId,
            variant: "success",
            closable: "off",
            header: {
                innerHTML: Granite.I18n.get("Add To Collection")
            },
            content: {
                innerHTML: _getAddToCollectionSuccessMsg(selectedItems, $("<span>").text(destinationTitle).html())
            },
            footer: {
                innerHTML: '<button is="coral-button" class="' + addToCollectionSuccessDialogCloseClass +
                           '" variant="default" coral-close>' + Granite.I18n.get("Close") + "</button>"
            }
        });
        $("body").append(successModal);
        successModal.show();
    }

    function _goToAppropriatePageOnClosingOfAddToCollectionSuccessDialog() {
        $("." + addToCollectionSuccessDialogCloseClass).on("click", function() {
            if ($("#shell-propertiespage-closeactivator")) {
                window.location =
                    $(".foundation-wizard-control[data-foundation-wizard-control-action='cancel']")[0].href;
            } else {
                redirectToPreviousPage();
            }
        });
    }

    $(document).on("click", "#shell-propertiespage-saveactivator", function(e) {
        _submit();
    });

    $(document).on("foundation-selections-change", function(event) {
        if (!event.target.selectedItems.length) {
            return;
        }
        var $selectedItems = $(event.target.selectedItems[0]);
        var selectedItemMeta = $selectedItems.find(".foundation-collection-assets-meta")[0];
        if (selectedItemMeta) {
            destinationPath = selectedItemMeta.dataset.foundationCollectionMetaPath;
            destinationTitle = selectedItemMeta.dataset.foundationCollectionMetaTitle;
        } else {
            destinationPath = $selectedItems.data("foundationCollectionItemId");
            destinationTitle = $selectedItems.data("itemTitle");
        }
    });

    $(document).on("foundation-contentloaded" + ns, function(e) {
        var selectedItems = $(".destination-container").data("assetpath");
        if (selectedItems === undefined || selectedItems.length <= 0) {
            $(".add-to-collection-wizard").html(
                "<p style='text-align: center;'>" +
                Granite.I18n.get("There is no content to display.") + "<br>" +
                Granite.I18n.get("If you have manually refreshed the page,") + "<br>" +
                Granite.I18n.get("please go back and select asset/s to be added to a collection.") + "</p>"
            );
            return;
        }
        if ($(".cq-damadmin-admin-childcollections.foundation-collection .foundation-collection-item").length) {
            // FixMe: No need of this, it should be removed once contextual actions are in place
            window.setTimeout(_selectFirstItem, 100);
        }
    });

    $(document).on("click", ".cq-damadmin-admin-childcollections .foundation-collection-item", function(e) {
        // disable going inside the collection
        e.preventDefault();
    });




    $(document).on("click", ".cq-damadmin-admin-addnewcollection-activator", function(e) {
        if (!addNewCollectionDialog) {
            addNewCollectionDialog = new Coral.Dialog().set({
                header: {
                    innerHTML: Granite.I18n.get("Create New Collection")
                },
                content: {
                    innerHTML: '<div class="coral-Form-fieldwrapper">' +
                               '<input is="coral-textfield" id="collectionname" class="coral-Form-field" ' +
                               'placeholder="' + Granite.I18n.get("Enter Collection title") +
                               '" name="field" value="">' +
                               "</div>" +
                               '<div class="coral-Form-fieldwrapper">' +
                               '<coral-checkbox id="collectionpublic" class="coral-Form-field" name="public">' +
                               Granite.I18n.get("Public Collection") +
                               "</coral-checkbox>" +
                               "</div>"
                },
                footer: {
                    innerHTML: '<button id="cancelButton" is="coral-button" variant="default" coral-close>' +
                                Granite.I18n.get("Cancel") +
                                '</button><button class="continue" is="coral-button" variant="primary" disabled>' +
                                Granite.I18n.get("Continue") + "</button>"
                }
            });

            addNewCollectionDialog.classList.add("cq-damadmin-admin-actions-addnewcollection");
            document.body.appendChild(addNewCollectionDialog);
            if(isAllowedPublicCollection() == "false"){
                document.getElementById("collectionpublic").style.display = "none";
            }
            addNewCollectionDialog.show();
        } else {
            var $addNewCollectionDialog = $(addNewCollectionDialog);
            $addNewCollectionDialog.find("#collectionname").val("");
            if ($addNewCollectionDialog.find("#collectionpublic").length > 0) {
                $addNewCollectionDialog.find("#collectionpublic")[0].checked = false;
            }
            $addNewCollectionDialog.find(".continue")[0].disabled = true;
            addNewCollectionDialog.show();
        }
    });

    $(document).on("click", ".cq-damadmin-admin-actions-addnewcollection .continue", function(e) {
        var principal = "";
        var collectionHome = "";

        if ($(".lightbox-principal").html() !== "undefined") {
            principal = $(".lightbox-principal").html();
        }
        if ($(".lightbox-collection-home").html() !== "undefined") {
            collectionHome = $(".lightbox-collection-home").html();
        }
        var title = $("#collectionname").val();
        var collectionPublic = $("#collectionpublic")[0].checked;

        addNewCollectionDialog.hide();

        $.ajax({
            type: "POST",
            url: Granite.HTTP.externalize(collectionsRoot(collectionHome) + ".collection.html"),
            data: {
                "_charset_": "utf-8",
                ":operation": "create",
                "collectionPath": collectionHome,
                "teamMemberPrincipalName": principal,
                "teamMemberRole": "owner",
                "title": title,
                "public": collectionPublic,
                "description": ""
            }
        }).done(function(html) {
            var selectedItems = $(".destination-container").data("assetpath");
            if (selectedItems.length) {
                // CQ-4201137 : Use addtocollectionwizard.external.html for POST requests instead of location.href
                var url = Granite.HTTP.externalize("/mnt/overlay/dam/gui/content/collections/addtocollectionwizard.external.html");// eslint-disable-line max-len
                var cancelhref =
                    $(".foundation-wizard-control[data-foundation-wizard-control-action='cancel']")[0].href;
                var data = {
                    "item": selectedItems,
                    "_charset_": "utf-8",
                    "cancelhref": cancelhref
                };
            }
            // Post to the provided URL with the specified parameters.
            var post = function post(path, parameters) {
                var form = $("<form></form>");
                form.attr("method", "post");
                form.attr("action", path);
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
            };
            if (url) {
                post(url, data);
            }
        }).fail(function(xhr, error, errorThrown) {
            var ui = $(window).adaptTo("foundation-ui");
            var message = Granite.I18n.get("An error occurred while creating collection, you may not have permission to create a collection.");// eslint-disable-line max-len
            ui.alert(Granite.I18n.get("Error"), message, "error");
        });
    });

    $(document).on("input", "#collectionname", function(e) {
        if ($(this).val()) {
            $(".cq-damadmin-admin-actions-addnewcollection .continue")[0].disabled = false;
        } else {
            $(".cq-damadmin-admin-actions-addnewcollection .continue")[0].disabled = true;
        }
    });

    function _selectFirstItem() {
        if ($(".cq-damadmin-admin-childcollections.foundation-collection .foundation-collection-item").length) {
            // FixMe: No need of this, it should be removed once contextual actions are in place
            var selectionAPI =
                $(".cq-damadmin-admin-childcollections.foundation-collection").adaptTo("foundation-selections");
            selectionAPI.select($(".cq-damadmin-admin-childcollections.foundation-collection" +
                " .foundation-collection-item")[0]);
        }
    }

    function redirectToPreviousPage() {
        if ($(".referer").data("refererPath")) {
            window.location = $(".referer").data("refererPath");
        } else if (window.referrer ||
            (document.referrer && document.referrer.indexOf(window.location.pathname) === -1)) {
            window.location = window.referrer || document.referrer;
        } else {
            history.back();
        }
    }

    function collectionsRoot(lightBoxCollectionHome) {
        var collectionsRootIndex = lightBoxCollectionHome.indexOf("collections") + "collections".length;
        return lightBoxCollectionHome.substr(0, collectionsRootIndex);
    }

    function isAllowedPublicCollection(){
            var res = $.ajax({
                url: "/bin/mediahub/allowPublicCollection",
                type: "get",
                success: function (data) {},
                async: false,
                error: function (err) {
                    console.log(err);
                }
            }).responseText;
            return res;
        }


})(window, document, Granite, Granite.$);
