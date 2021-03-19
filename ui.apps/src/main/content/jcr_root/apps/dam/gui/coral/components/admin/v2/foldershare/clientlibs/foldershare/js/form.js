/*
 * ADOBE CONFIDENTIAL
 *
 * Copyright 2013 Adobe Systems Incorporated
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
 *
 */

(function(document, $, _g) {
    "use strict";

    // For cross browser support - CQ-37914
    if (!String.prototype.endsWith) {
        String.prototype.endsWith = function(suffix) {
            return this.indexOf(suffix, this.length - suffix.length) !== -1;
        };
    }

    var collectionItemRel = ".cq-damadmin-admin-childpages .foundation-collection-item";

    $(document).on("keypress", ".data-fields input[type=text]", function(e) {
        if (e.keyCode === 13) {
            return false;
        }
    });


    function validateBnppStatus() {
            var ariaRequired = $('.cq-damadmin-admin-folder-settings-form [aria-required="true"]');
            // AS TODO: Do FMS support data-required fields
            // var dataRequired = $('.data-fields.active [data-required="true"]');
            if(document.getElementsByName("./jcr:content/metadata/bnpp-status")) {
              if(document.getElementsByName("./jcr:content/metadata/bnpp-status")[0]){
                if(document.getElementsByName("./jcr:content/metadata/bnpp-status")[0].value !== 'validated'){
                  return false;
                }
              }
            }

            return true;
        }

    function validateRequiredFields() {
        var ariaRequired = $('.cq-damadmin-admin-folder-settings-form [aria-required="true"]');
        // AS TODO: Do FMS support data-required fields
        // var dataRequired = $('.data-fields.active [data-required="true"]');
        var isValid = true;

        ariaRequired.each(function(index, item) {
            if ($(item).is("coral-multifield")) {
                var child = $(item).children("coral-multifield-item");
                var hasValue = false;

                $(child).each(function(i, value) {

                    if ($(value).find('input[is="coral-textfield"]').val() !== "") {
                        hasValue = true;
                    }
                });
                if (hasValue === false) {
                    isValid = hasValue;
                }
            } else {
                var field = $(item).closest(".coral-Form-field");
                var validation = field.adaptTo("foundation-validation");

                if (!validation.checkValidity()) {
                    isValid = false;
                }
                validation.updateUI();
            }
            $(item).updateErrorUI();
        });

        return isValid;
    }

    function saveMetadataChanges(e) {
        // @see CQ-29669 Don't validate for bulkeditor
        if (!validateRequiredFields()) {
            showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                Granite.I18n.get("One or more required field(s) is/are empty."),
                '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
            return;
        }

        var wizard = $("form#folder-settings-form")[0];
        var folderPath = $(".cq-damadmin-admin-folder-settings-form").attr("action");
        var hintFields = createHintFields(false, false);
        $.DAM.FolderShare.updateCugToFolder(folderPath, function() {
            // submit the form after cug policy is applied to folder
            $.DAM.FolderShare.submit(wizard, hintFields);
        });

        if ($("#collection-modifieddate").length) {
            $("#collection-modifieddate").attr("value", (new Date()).toISOString());
        }

        createNewTags($("form.data-fields.active")).done(function() {
            addRating();
        }).fail(function(response) {
            showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                Granite.I18n.get("Unable to create new tags. Check for access privileges to create tags."), "");
        });
    }

    function saveMediaMetadataChanges(e) {

            if (document.getElementById("shell-propertiespage-save-publish").getAttribute("ismediavalidated") === "emptyMedia") {
                showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Cannot publish an empty media."),
                    '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
                return false;
            }

            if(!validateBnppStatus() && (document.getElementById("shell-propertiespage-save-publish").getAttribute("isChildrenDeactivated") === null) ){
                showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("BNPP Status is not Validated."),
                    '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
                return false;
            }

            // @see CQ-29669 Don't validate for bulkeditor
            if (!validateRequiredFields()) {
                showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("One or more required field(s) is/are empty."),
                    '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
                return false;
            }

            if (document.getElementById("shell-propertiespage-save-publish").getAttribute("isValidated") === "false") {
                showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("One or more assets has required field(s) empty."),
                    '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
                return false;
            }

            var wizard = $("form#folder-settings-form")[0];
            var folderPath = $(".cq-damadmin-admin-folder-settings-form").attr("action");
            var hintFields = createHintFields(false, false);
            $.DAM.FolderShare.updateCugToFolder(folderPath, function() {
                // submit the form after cug policy is applied to folder
                $.DAM.FolderShare.submitMedia(wizard, hintFields);
            });

            if ($("#collection-modifieddate").length) {
                $("#collection-modifieddate").attr("value", (new Date()).toISOString());
            }

            createNewTags($("form.data-fields.active")).done(function() {
                addRating();
            }).fail(function(response) {
                showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Unable to create new tags. Check for access privileges to create tags."), "");
            });

            return true;
        }

    $(document).on("click", "#shell-propertiespage-save-publish", function(e) {
        if(!validateBnppStatus() && (document.getElementById("shell-propertiespage-save-publish").getAttribute("isChildrenDeactivated") !== null) && (document.getElementById("shell-propertiespage-save-publish").getAttribute("isChildrenDeactivated") !== "true") ){
          deactivateChildren();
        } else if(saveMediaMetadataChanges(e)){
          internalPublish(document.getElementById("shell-propertiespage-save-publish").getAttribute("isValidated"), e , document.getElementById("shell-propertiespage-save-publish").getAttribute("isFolderMetadataMissing"), document.getElementById("shell-propertiespage-save-publish").getAttribute("isMediaValidated"));
        }
        return false;
    });

    $(document).on("click", "#shell-propertiespage-saveactivator, #shell-propertiespage-doneactivator", function(e) {
        saveMetadataChanges(e);
        return false;
    });

    $(document).on("click", "#soft-submit-popover .aem-assets-metadataeditor-bulk-submit", function(e) {
        saveMetadataChanges(e);
    });

    function addRating() {
        var rating = $(".rating.edit-mode coral-icon[icon='starFill'].current-rating").data("rate");
        if (rating) {
            var contentPath = $(".rating.edit-mode coral-icon[icon='starFill'].current-rating")
                .closest("form").data("formid");
            if (!contentPath) {
                contentPath = $(collectionItemRel).data("path");
            }
            if (!contentPath) {
                return;
            }
            var url = Granite.HTTP.getContextPath() + contentPath + "/ratings.social.json";
            $.ajax({
                type: "POST",
                url: url,
                async: false,
                data: {
                    tallyGenerator: "assets",
                    response: rating,
                    tallyType: "Rating",
                    ":operation": "social:postTallyResponse"
                },
                error: function(e) {
                    showDialog("aem-assets-rating-error", "error", Granite.I18n.get("Rating Failure"),
                        Granite.I18n.get("Error in rating the asset."),
                        '<button is="coral-button" class="aem-assets-rating-error" variant="default" coral-close>' +
                        Granite.I18n.get("OK") + "</button>");
                }
            });
        }
    }

    function showDialog(id, variant, header, content, footer) {
        var $dialog = $("#" + id);
        var dialog;
        if ($dialog.length === 0) {
            dialog = new Coral.Dialog().set({
                id: id,
                variant: variant,
                closable: "on",
                header: {
                    innerHTML: header
                },
                content: {
                    innerHTML: content
                },
                footer: {
                    innerHTML: footer
                }
            });
            document.body.appendChild(dialog);
        } else {
            dialog = $dialog[0];
            dialog.header.innerHTML = header;
            dialog.content.innerHTML = content;
            dialog.footer.innerHTML = footer;
        }
        dialog.show();
    }

    // showing spinner after bulk metadata edit success and reloading the window
    $(document).on("coral-overlay:close", "#aem-assets-metadataedit-success", function() {
        var ui = $(window).adaptTo("foundation-ui");
        if (ui !== undefined) {
            ui.wait();
        }
        if (location.search === undefined || location.search === "") {
            // Send user back to last page if its a POST request
            window.location = $("#shell-propertiespage-closeactivator")[0].href;
        } else {
            window.location.reload();
        }
    });

    function createHintFields(multiAssets, isCollection) {
        var hintFields = [];
        var $form = $("form#folder-settings-form");
        var allTags = $("[data-metatype=tags]", $form);
        allTags.each(function(index, tag) {
            var $tag = $(tag);
            var name = $("coral-taglist", $tag).data("fieldname");
            if (!name) {
                name = "./jcr:content/metadata/cq:tags";
            }
            if (!multiAssets) {
                hintFields.push({
                    "name": name + "@Delete",
                    "value": "delete-empty"
                });
            }
        });


        var allNumbers = $("[data-metatype=number]", $form);
        allNumbers.each(function(index, number) {
            var $number = $(number);
            var typeHint = $number.data("typehint");
            if (!typeHint) {
                typeHint = "Long";
            }
            var name = $number.attr("name");
            // fallback to textfield wrapped in form field
            if (!name) {
                name = $('input[is="coral-textfield"]', $number).attr("name");
            }
            hintFields.push({
                "name": name + "@TypeHint",
                "value": typeHint
            });
        });

        var allMVText = $("[data-metatype=mvtext]", $form);
        allMVText.each(function(index, mvtext) {
            var $mvtext = $(mvtext);
            var typeHint = $mvtext.data("typehint");
            if (!typeHint) {
                typeHint = "String[]";
            }
            var name = $mvtext.data("granite-coral-multifield-name");
            hintFields.push({
                "name": name + "@TypeHint",
                "value": typeHint
            });
        });


        var allCheckbox = $("[data-metatype=checkbox]", $form);
        allCheckbox.each(function(index, checkbox) {
            var $checkbox = $(checkbox);
            if ($checkbox.is(":checked")) {
                $checkbox.attr("value", "true");
            } else if (!($(".cq-damadmin-admin-childpages.foundation-collection")
                .data("foundation-selections-mode") === "multiple")) {
                // Add false to checkbox if asset is not opened in bulk metadata editor
                // see https://jira.corp.adobe.com/browse/CQ-98699 for details .
                // in bulk metadata editor only checked state is considered.
                $checkbox.attr("value", "false");
            }
            var typeHint = $checkbox.data("typehint");
            if (!typeHint) {
                typeHint = "Boolean";
            }
            var name = $checkbox.attr("name");
            hintFields.push({
                "name": name + "@TypeHint",
                "value": typeHint
            });
        });

        var allMVSelects = $("[data-metatype=dropdown]", $form);
        allMVSelects.each(function(index, mvSelect) {
            var $mvSelect = $(mvSelect);
            var typeHint = $mvSelect.data("typehint");
            if (!typeHint) {
                typeHint = mvSelect.hasAttribute("multiple") ? "String[]" : "String";
            }
            var name = $mvSelect.attr("name");
            hintFields.push({
                "name": name + "@TypeHint",
                "value": typeHint
            });
        });

        return hintFields;
    }

    function createNewTags(form) {
        return $.when.apply(null, form.find('.cq-ui-tagfield coral-taglist input[type="hidden"][name]').map(function() {
            var el = this;

            if (el.value.indexOf(":") >= 0) {
                return;
            }

            var tenantId = $(".foundation-form.mode-edit").attr("tenant-id");
            el.value = tenantId ? ("mac:" + tenantId + "/default/" + el.previousElementSibling.textContent)
                : el.previousElementSibling.textContent;
            return createSingleTag(el.value).then(function(tag) {
                // Fix tag name in select element
                var tenantId = $(".foundation-form.mode-edit").attr("tenant-id");
                if (!tenantId) {
                    // Fix tag name in select element
                    el.value = tag;
                }
            });
        }));
    }

    function createSingleTag(name) {
        var param = {
            cmd: "createTagByTitle",
            tag: name,
            locale: "en", // This is fixed to "en" in old siteadmin also
            "_charset_": "utf-8"
        };
        return $.ajax({
            url: Granite.HTTP.externalize("/bin/tagcommand"),
            type: "POST",
            data: param,
            async: false
        })
            .then(function(html) {
                return $(html).find("#Path").text();
            });
    }
})(document, Granite.$, _g);
