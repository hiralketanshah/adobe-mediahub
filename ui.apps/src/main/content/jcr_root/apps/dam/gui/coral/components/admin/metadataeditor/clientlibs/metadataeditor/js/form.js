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

(function(document, $, _g, Dam) {
    "use strict";

    // For cross browser support - CQ-37914
    if (!String.prototype.endsWith) {
        String.prototype.endsWith = function (suffix) {
            return this.indexOf(suffix, this.length - suffix.length) !== -1;
        };
    }

    var collectionItemRel = ".cq-damadmin-admin-childpages .foundation-collection-item";
    var selectionItemRel = ".cq-damadmin-admin-childpages .foundation-selections-item";
    var simpleSave = true;
    var formId = "";

    $(document).on("keypress", ".data-fields input[type=text]", function (e) {
        if (e.keyCode === 13) {
            return false;
        }
    });

    function validateRequiredFields() {
        var ariaRequired = $('.data-fields.active [aria-required="true"]');
        var dataRequired = $('.data-fields.active [data-required="true"]');
        var isValid = true;
        ariaRequired.each(function (index, item) {
            if ($(item).is("coral-multifield")) {
                var child = $(item).children("coral-multifield-item");
                var hasValue = false;

                $(child).each(function (i, value) {
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
        if (isValid) {
            dataRequired.each(function (index, item) {
                if (!$('input[is="coral-textfield"]', $(item)).val()) {
                    isValid = false;
                    if ($(item).data("metatype") === "number") {
                        $(item).attr("invalid", "");
                    }
                } else {
                    if ($(item).data("metatype") === "number") {
                        $(item).removeAttr("invalid");
                    }
                }
            });
        }

        return isValid;
    }

    function validateAssets() {
        var selectionItems = $(selectionItemRel);
        var filePath;
        var isAllAssetsValid = true;
        selectionItems.each(function (index, value) {
            filePath = $(value).data("path");
            var jsonPath = encodeURIComponent(filePath).replace(/%2F/g, "/") + ".1.json?ch_ck = " + Date.now();
            jsonPath = Granite.HTTP.externalize(jsonPath);
            // check if Asset is present or not
            var result = Granite.$.ajax({
                type: "GET",
                async: false,
                url: jsonPath
            });

            if (result.status !== 200) {
                isAllAssetsValid = false;
                return false;
            }
        });

        return isAllAssetsValid;
    }

    function saveMetadataChanges(e) {
        var selectedArticles = $(selectionItemRel).length;

        // @see CQ-29669 Don't validate for bulkeditor
        if (selectedArticles === 1 && !validateRequiredFields()) {
            // Invalid class sometimes doesn't get added to input element
            // $('.data-fields.active .coral-DatePicker.is-invalid').each(function(index, field){
            // $('input[type="text"]', field).addClass('is-invalid');
            // });
            showDialog("aem-assets-metadataedit-validationerror", "error", Granite.I18n.get("Error"),
                Granite.I18n.get("One or more required field(s) is/are empty."),
                '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
            return;
        }
        var assetsAreValid = validateAssets();
        if (assetsAreValid === true) {
            // Assets is present
            var cur = $(e.currentTarget);
            var beforesubmit = $.Event("beforesubmit", {
                originalEvent: e
            });

            cur.trigger(beforesubmit);
            if (beforesubmit.isDefaultPrevented()) {
                return false;
            }

            if ($("#collection-modifieddate").length) {
                $("#collection-modifieddate").attr("value", (new Date()).toISOString());
            }

            createNewTags($("form.data-fields.active")).done(function () {
                var form = $("form.data-fields.active");
                handleResponse(form, submitForm(form));
            }).fail(function (response) {
                showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Unable to create new tags. Check for access privileges to create tags."), "");
            });
        } else {
            // assets is not present
            showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                Granite.I18n.get("Some assets are either removed or not accessible. Please refresh assets and try again."), ""); // eslint-disable-line max-len
        }

        return true;
    }

    function saveMediaMetadataChanges(e) {
        var selectedArticles = $(selectionItemRel).length;

        var assetsAreValid = validateAssets();
        if (assetsAreValid === true) {
            // Assets is present
            var cur = $(e.currentTarget);
            var beforesubmit = $.Event("beforesubmit", {
                originalEvent: e
            });

            cur.trigger(beforesubmit);
            if (beforesubmit.isDefaultPrevented()) {
                return false;
            }

            if ($("#collection-modifieddate").length) {
                $("#collection-modifieddate").attr("value", (new Date()).toISOString());
            }

            createNewTags($("form.data-fields.active")).done(function () {
                var form = $("form.data-fields.active");
                handleResponse(form, submitForm(form));
            }).fail(function (response) {
                showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Unable to create new tags. Check for access privileges to create tags."), "");
            });
        } else {
            // assets is not present
            showDialog("aem-assets-metadataedit-tags-error", "error", Granite.I18n.get("Error"),
                Granite.I18n.get("Some assets are either removed or not accessible. Please refresh assets and try again."), ""); // eslint-disable-line max-len
        }
    }


    $(document).on("click", "#shell-propertiespage-saveactivator, #shell-propertiespage-doneactivator", function (e) {
        if (e.currentTarget.id === "shell-propertiespage-doneactivator") {
            simpleSave = false;
        } else {
            simpleSave = true;
        }
        var appendModeEnabled = $(".foundation-content-path").data("appendModeEnabled");
        if (appendModeEnabled === undefined) {
            appendModeEnabled = true;
        }
        if (!$(".foundation-content-path").data("is-bulk-mode") || !appendModeEnabled) {
            saveMetadataChanges(e);
        }
        return false;
    });

    $(document).on("click", "#shell-propertiespage-saveactivator-media", function (e) {
        if (e.currentTarget.id === "shell-propertiespage-doneactivator") {
            simpleSave = false;
        } else {
            simpleSave = true;
        }
        var appendModeEnabled = $(".foundation-content-path").data("appendModeEnabled");
        if (appendModeEnabled === undefined) {
            appendModeEnabled = true;
        }
        if (!$(".foundation-content-path").data("is-bulk-mode") || !appendModeEnabled) {
            saveMediaMetadataChanges(e);
        }
		saveMediaMetadataChanges(e);
        return false;
    });

    $(document).on("click", "#shell-propertiespage-activate-bulk-asset, #shell-propertiespage-deactivate-bulk-asset", function (e) {
        if (e.currentTarget.id === "shell-propertiespage-activate-bulk-asset") {
            formId = "shell-propertiespage-activate-bulk-asset";
            saveMediaMetadataChanges(e);
            var isValidated = document.getElementById("shell-propertiespage-activate-bulk-asset").getAttribute("isValidated");
            if (isValidated && isValidated === 'true') {
                assetBulkActivation(e);
            } else {
                internalPublishErrorMessage(document.getElementById("shell-propertiespage-activate-bulk-asset").getAttribute("isValidated"), e, document.getElementById("shell-propertiespage-activate-bulk-asset").getAttribute("isFolderMetadataMissing"), document.getElementById("shell-propertiespage-activate-bulk-asset").getAttribute("isMediaValidated"));
            }
        } else {
            deactivateBulkAsset(e);
        }
        return false;
    });

    function assetBulkActivation(e){
        var data = {};
        var selectedItems = $(selectionItemRel);
        var bulkAssets = new Array();
        selectedItems.each(function(index, value) {
            bulkAssets[index] = $(value).data("path");
        });
        data["path"] = bulkAssets;
        $.ajax({
            type: "GET",
            url: "/bin/mediahub/assetpublish",
            data: data
        }).done(function(json) {
          var ui = $(window).adaptTo("foundation-ui");
          var successMessage = Granite.I18n.get("The Assets will be published soon");
          ui.prompt(Granite.I18n.get("The Assets wil be published soon"), successMessage, "success", [{
              text: Granite.I18n.get("OK"),
              primary: true,
              handler: function () {
                  location.href = $(".foundation-backanchor").attr("href");
              }
          }]);
        }).fail(function(json) {
            ui.prompt(Granite.I18n.get("Error"), "Error while publishing assets :" + json.responseJSON.message, "Error while publishing assets :" + json.responseJSON.message, [{
                text: Granite.I18n.get("Close"),
                primary: true,
                handler: function() {
                    // do nothing in case of error
                }
            }]);
        });
    }

    function deactivateBulkAsset(e){
        var data = {};
        var selectedItems = $(selectionItemRel);
        var bulkAssets = new Array();
        selectedItems.each(function(index, value) {
            bulkAssets[index] = $(value).data("path");
        });
        data["path"] = bulkAssets;
        $.ajax({
            type: "GET",
            url: "/bin/asset/bulkunpublish",
            data: data
        }).done(function(json) {
          var ui = $(window).adaptTo("foundation-ui");
          var successMessage = Granite.I18n.get("The Assets will be unpublished soon");
          ui.prompt(Granite.I18n.get("The Assets wil be unpublished soon"), successMessage, "success", [{
              text: Granite.I18n.get("OK"),
              primary: true,
              handler: function () {
                  location.href = $(".foundation-backanchor").attr("href");
              }
          }]);
        }).fail(function(json) {
            ui.prompt(Granite.I18n.get("Error"), "Error while unpublishing assets :" + json.responseJSON.message, "Error while unpublishing assets :" + json.responseJSON.message, [{
                text: Granite.I18n.get("Close"),
                primary: true,
                handler: function() {
                    // do nothing in case of error
                }
            }]);
        });
    }

    $(document).on("click", "#shell-propertiespage-save-publish", function (e) {
        var selectionItems = $(selectionItemRel);
        var assets = new Array();
        selectionItems.each(function (index, value) {
            assets[index] = $(value).data("path");
        });
        $.ajax({
           async: false,
           url: Granite.HTTP.externalize("/bin/mediahub/asset/processed"),
           type: "GET",
           data: {
               "paths": assets
           },
           success: function(resp) {
               if(resp.isInRunningWorkflow === false){
                  if (e.currentTarget.id === "shell-propertiespage-save-publish") {
                      simpleSave = false;
                  } else {
                      simpleSave = true;
                  }
                  var appendModeEnabled = $(".foundation-content-path").data("appendModeEnabled");
                  if (appendModeEnabled === undefined) {
                      appendModeEnabled = true;
                  }
                  if (!$(".foundation-content-path").data("is-bulk-mode") || !appendModeEnabled) {
                      var isValidated = document.getElementById("shell-propertiespage-save-publish").getAttribute("isValidated");
                      if (isValidated && isValidated === 'true') {
                          var ui = $(window).adaptTo("foundation-ui");
                          var successMessage = Granite.I18n.get("Properties are saved and The Asset has been triggered to Publish");
                          ui.prompt(Granite.I18n.get("The Asset has been triggered to Publish"), successMessage, "success", [{
                              text: Granite.I18n.get("OK"),
                              primary: true,
                              handler: function () {
                                  if (saveMetadataChanges(e)) {
                                      internalPublish(document.getElementById("shell-propertiespage-save-publish").getAttribute("isValidated"), e, document.getElementById("shell-propertiespage-save-publish").getAttribute("isFolderMetadataMissing"), document.getElementById("shell-propertiespage-save-publish").getAttribute("isMediaValidated"));
                                  }
                              }
                          }]);

                      } else {
                          internalPublishErrorMessage(document.getElementById("shell-propertiespage-save-publish").getAttribute("isValidated"), e, document.getElementById("shell-propertiespage-save-publish").getAttribute("isFolderMetadataMissing"), document.getElementById("shell-propertiespage-save-publish").getAttribute("isMediaValidated"));
                      }
                  }
               } else {
                  var ui = $(window).adaptTo("foundation-ui");
                  ui.prompt(Granite.I18n.get("Error"), Granite.I18n.get("This asset cannot be published now"), "warning", [{
                      text: Granite.I18n.get("Close"),
                      primary: true,
                      handler: function() {
                          // do nothing in case of error
                      }
                  }]);
               }
           }
        });


        return false;
    });

    $(document).on("click", "#shell-propertiespage-mediaactivator", function (e) {
        if (e.currentTarget.id === "shell-propertiespage-mediaactivator") {
            simpleSave = false;
        } else {
            simpleSave = true;
        }
        var appendModeEnabled = $(".foundation-content-path").data("appendModeEnabled");
        if (appendModeEnabled === undefined) {
            appendModeEnabled = true;
        }
        if (!$(".foundation-content-path").data("is-bulk-mode") || !appendModeEnabled) {
            saveMediaMetadataChanges(e);
        }
		    saveMediaMetadataChanges(e);
        return false;
    });


    $(document).on("click", "#soft-submit-popover .aem-assets-metadataeditor-bulk-submit", function (e) {
        saveMetadataChanges(e);
    });

    $(document).on("click", "#shell-propertiespage-deactivate-asset", function (e) {
        deactivateAsset(e);
    });

    function handleResponse(form, xhr) {
        var ui = $(window).adaptTo("foundation-ui");
        xhr.done(function () {
            if (ui !== undefined) {
                ui.clearWait();
            }
            addRating();
            createSuccessHandler(form, xhr);
        }).fail(function () {
            if (ui !== undefined) {
                ui.clearWait();
            }
            if (xhr.status === 500) {
                showDialog("aem-assets-metadataedit-error", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Unable to edit properties. Insufficient permissions."), "");
            } else {
                showDialog("aem-assets-metadataedit-error", "error", Granite.I18n.get("Error"),
                    Granite.I18n.get("Unable to edit properties."), "");
            }
        });
    }

    function createSuccessHandler(form, xhr) {
        var $articles = $(collectionItemRel);
        var length = $articles.length;
        if (length > 1) {
            if (simpleSave) {
                successModalForBulkEdit();
            } else {
                form.trigger("foundation-form-submitted", [ true, xhr ]);
            }
        } else {
            form.trigger("foundation-form-submitted", [ true, xhr ]);
        }
    }

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
                error: function (e) {
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

    function successModalForBulkEdit() {
        var selectedArticles = $(selectionItemRel);
        var assets = new Array();
        var limit = 10;
        selectedArticles.each(function (item, value) {
            assets[item] = $(value).data("title");
        });
        var resp = "";
        if (assets.length > 1) {
            if (selectedArticles.hasClass("card-collection")) {
                resp = "<p>" + Granite.I18n.get("The following {0} collections have been modified:",
                    assets.length) + "</p>";
            } else {
                resp = "<p>" + Granite.I18n.get("The following {0} assets have been modified:",
                    assets.length) + "</p>";
            }
        } else if (assets.length === 1) {
            if (selectedArticles.hasClass("card-collection")) {
                resp = "<p>" + Granite.I18n.get("The following collection have been modified:") + "</p>";
            } else {
                resp = "<p>" + Granite.I18n.get("The following asset have been modified:") + "</p>";
            }
        }

        resp += "<p class=\"item-list\">";
        var iterLim = assets.length;
        if (assets.length > limit) {
            iterLim = limit - 1;
        }
        for (var i = 0; i < iterLim; i++) {
            resp += _g.XSS.getXSSValue(assets[i]) + "<br>";
        }
        if (assets.length > limit) {
            resp += "...<br>";
        }
        resp += "</p>";

        if(formId !== "shell-propertiespage-activate-bulk-asset"){
            showDialog("aem-assets-metadataedit-success", "success", Granite.I18n.get("Asset(s) modified"), resp,
                        '<button is="coral-button" variant="default" coral-close>' + Granite.I18n.get("OK") + "</button>");
        }
    }

    // showing spinner after bulk metadata edit success and reloading the window
    $(document).on("coral-overlay:close", "#aem-assets-metadataedit-success", function() {
        var ui = $(window).adaptTo("foundation-ui");
        if (ui !== undefined) {
            ui.wait();
        }
        if (simpleSave) {
            if (Dam.Util.openWithFormPost !== undefined) {
                var patharr = [];
                var items = $(".foundation-selections-item");
                var viewPropertiesWizardURL =
                    Granite.HTTP.externalize("/mnt/overlay/dam/gui/content/assets/metadataeditor.external.html");
                if (items.length) {
                    items.each(function(i) {
                        var item = $(this);
                        var itemPath = item.data("foundation-collection-item-id");
                        patharr.push(itemPath);
                    });
                    // POST Request
                    var data = {
                        "item": patharr,
                        "_charset_": "utf-8"
                    };
                    Dam.Util.openWithFormPost(viewPropertiesWizardURL, data);
                }
            } else {
                // for backwards compatibility
                window.location.reload();
            }
        } else if (location.search === undefined || location.search === "") {
            // Send user back to last page if its a POST request
            window.location = $("#shell-propertiespage-closeactivator")[0].href;
        } else {
            window.location.reload();
        }
    });

    function submitForm(form) {
        var data = formDataForMultiEdit(form);
        var ui = $(window).adaptTo("foundation-ui");
        if (ui !== undefined) {
            ui.wait();
        }

        var url = "";
        if (data.multiAssets) {
            url = Granite.HTTP.externalize("/content/dam.html");
        } else {
            url = Granite.HTTP.externalize(data.contentPath);
        }

        return $.ajax({
            type: "post",
            url: url,
            data: data.formData,
            cache: false,
            async: false
        });
    }

    function formDataForMultiEdit(form) {
        var assets = new Array();
        var articleMarkup = new Array();

        var selectionItems = $(selectionItemRel);

        var multiAssets = true;
        if (selectionItems.length === 1) {
            multiAssets = false;
        }
        selectionItems.each(function(index, value) {
            articleMarkup[index] = $(value);
            assets[index] = articleMarkup[index].data("path");
        });


        var basePath = "/content/dam";
        var charset = form.data("charset");
        if (!charset) {
            charset = "utf-8";
        }

        var contentPath = selectionItems.data("path");

        if (!multiAssets) {
            basePath = contentPath;
        }

        var contentType = selectionItems.data("type");
        var isCollection = contentType ? contentType.toLowerCase() === "collection" : false;
        var hintFields = createHintFields(multiAssets, isCollection);

        var data = [];
        data.push({ "name": "_charset_", "value": charset });
        data.push({ "name": "dam:bulkUpdate", "value": "true" });

        if (isCollection) {
            selectionItems.each(function(index, value) {
                data.push({ "name": $(value).data("path") + "/jcr:lastModified", "value": "" });
                data.push({ "name": $(value).data("path") + "/jcr:lastModifiedBy", "value": "" });
            });
        }

        var checked = $("#soft-submit-popover input:checkbox").prop("checked");
        data.push(checked ? { "name": "mode", "value": "soft" } : { "name": "mode", "value": "hard" });
        if (checked) {
            selectionItems.each(function(index, value) {
                var cvm = $(value).data("contentvm");
                var mdvm = $(value).data("metadatavm");
                var collvm = $(value).data("collectionvm");
                var p = {};
                p["path"] = $(value).data("path");
                p["cvm"] = cvm;
                p["mdvm"] = mdvm;
                p["collvm"] = collvm;

                data.push({ "name": "asset", "value": JSON.stringify(p) });
            });
        }

        var arrayEligibleFormData = $(".data-fields.active").serializeArray();

        for (var i = 0; i < assets.length; i++) {
            var pdfAsset = $("#aem-assets-metadataeditor-formid[data-mime-type='application/pdf']").length > 0;
            var pdfKeywords = [];
            var j;
            var name;

            if (!pdfAsset) {
                var asset = $(".cq-damadmin-admin-childpages .foundation-collection-item" +
                    "[data-foundation-collection-item-id='" + assets[i].replace(/(')/g, "\\$1") + "']");
                pdfAsset = asset.length === 1 && asset.attr("data-mime-type") === "application/pdf";
            }
            for (j = 0; j < arrayEligibleFormData.length; j++) {
                name = arrayEligibleFormData[j]["name"];
                if (name.indexOf("./") !== 0) {
                    if (i !== 0) {
                        // Add to form data only once
                        continue;
                    }
                } else {
                    name = "." + assets[i].substring(basePath.length) + name.substring(1);
                }
                var value = arrayEligibleFormData[j]["value"];
                if (pdfAsset && name === "./jcr:content/metadata/dc:subject") {
                    (value.indexOf(",") !== -1 || value.indexOf(";") !== -1) ? pdfKeywords.push('"' + value + '"')
                        : pdfKeywords.push(value);
                }
                // publish all subassets if it is a s7 set
                if (name === "./jcr:content/onTime" &&
                    articleMarkup[i].data("is-s7set") === true) {
                    articleMarkup[i].data("s7set-subassets-path-list").split(":").forEach(function(val) {
                        data.push({
                            "name": "." + val.substring(basePath.length) + name.substring(1),
                            "value": value
                        });
                    });
                }
                if (value || assets.length === 1) {
                    data.push({ "name": name, "value": value });
                }
            }

            // If asset mimetype is application/pdf then pdf:Keywords and dc:subject has to be in sync
            if (pdfAsset) {
                data.push({ "name": "." + assets[i].substring(basePath.length) +
                        "/jcr:content/metadata/pdf:Keywords@Delete" });
                data.push({
                    "name": "." + assets[i].substring(basePath.length) + "/jcr:content/metadata/pdf:Keywords",
                    "value": pdfKeywords.join(",")
                });
            }

            for (j = 0; j < hintFields.length; j++) {
                name = "." + assets[i].substring(basePath.length) + hintFields[j].name.substring(1);
                data.push({
                    "name": name,
                    "value": hintFields[j].value
                });
            }
        }

        return {
            contentPath: contentPath,
            multiAssets: multiAssets,
            formData: data
        };
    }

    function createHintFields(multiAssets, isCollection) {
        var hintFields = [];
        var $form = $("form.data-fields.active");
        var allTags = $("[data-metatype=tags]", $form);
        allTags.each(function (index, tag) {
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
        allNumbers.each(function (index, number) {
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
        allMVText.each(function (index, mvtext) {
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
        allCheckbox.each(function (index, checkbox) {
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
        allMVSelects.each(function (index, mvSelect) {
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
        return $.when.apply(null, form.find('.cq-ui-tagfield coral-taglist input[type="hidden"][name]').map(function () {
            var el = this;
            var tagName = "";

            if (el.value.indexOf(":") >= 0) {
                return;
            }

            if (el.name.indexOf("/") >= 0) {
                var pieces = el.name.split("/");
                tagName = pieces[pieces.length - 1];
            }
            var tenantId = $(".foundation-form.mode-edit").attr("tenant-id");

            if (el.previousElementSibling.textContent.indexOf(tagName) < 0) {
                el.value = tenantId ? ("mac:" + tenantId + "/default/" + tagName + "/" + el.previousElementSibling.textContent)
                    : tagName + "/" + el.previousElementSibling.textContent;
            } else {
                el.value = tenantId ? ("mac:" + tenantId + "/default/" + el.previousElementSibling.textContent)
                    : el.previousElementSibling.textContent;
            }


            return createSingleTag(el.value).then(function (tag) {
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
            .then(function (html) {
                return $(html).find("#Path").text();
            });
    }

    /**
     * Method to check if current delete operation should be processed async
     * @param selectedItems items to delete
     * @param folderDelete is any of the items to delete a folder
     * @returns {boolean} {@code true} if operation should be async, {@code false} otherwise
     */
    function isAssetProcessed(e) {
         var selectedArticles = $(selectionItemRel);
         var assets = new Array();

         selectionItems.each(function (index, value) {
              assets[index] = $(value).data("path");
         });

        $.ajax({
            async: false,
            url: Granite.HTTP.externalize("/bin/mediahub/asset/processed"),
            type: "GET",
            data: {
                "paths": assets
            },
            success: function(resp) {
                alert(resp);
            }
        });
        return false;
    }
})(document, Granite.$, _g, Dam);
