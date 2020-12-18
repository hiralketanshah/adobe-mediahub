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
(function($) {
    "use strict";

    var thumbnailRemoveButtonClass = "thumbnail-remove-button-active";
    var thumbnailImg = ".cq-dam-assetthumbnail coral-card coral-card-asset img";
    var thumbnailIcon = ".cq-dam-assetthumbnail coral-card coral-card-asset coral-icon";
    var wizardApi = $(".foundation-wizard").adaptTo("foundation-wizard");
    var uploadActionRel = ".thumbnail-upload-action";
    var previousImgElement = null;

    function setThumbnail(name, src, hasChanged) {
        var hasImage = name || src;
        name = name || "";
        var date = new Date();
        if ((src !== undefined) && !hasChanged) {
            src += "?ck=" + date.getTime();
        }
        var img = document.querySelector(thumbnailImg);
        if (!img) {
            img = document.querySelector(thumbnailIcon);
        }
        if (previousImgElement === null || img === document.querySelector(thumbnailIcon)) {
            previousImgElement = img;
        }
        var newImg = $("<img class=' show-grid'>").attr({
            src: src,
            title: name,
            width: img.width,
            height: img.height
        });
        $(img).replaceWith(newImg);
        var $thumbnail = $("#thumbnail-upload-button input");
        if (!hasImage) {
            $thumbnail.val("");
        }
        // If the user has not changed the file, it will not be uploaded nor the existing file removed on the server
        if (hasChanged) {
            $thumbnail.addClass("file-changed");
        }
        var fileUploadElm = $("#thumbnail-upload-button");
        var uploadThumbnailText = hasImage ? Granite.I18n.get("Default Thumbnail")
            : Granite.I18n.get("Change Thumbnail");
        changeText(fileUploadElm, uploadThumbnailText);
        if (hasImage) {
            $thumbnail.attr("title", Granite.I18n.get("Default Thumbnail"));
        } else {
            $thumbnail.attr("title", Granite.I18n.get("Change Thumbnail"));
        }
        fileUploadElm.addClass(thumbnailRemoveButtonClass);
        $("#removemanualthumbnail").val(false);
    }

    $(document).on("foundation-contentloaded change-thumbnail", function(e) {
        var HTML5enabled = window.FormData !== undefined;
        if (!HTML5enabled) {
            $("#thumbnail-upload-button").hide();
        }

        var isManualThumbnail = $(".cq-dam-assetthumbnail").data("manualThumbnail");
        if (isManualThumbnail === true) {
            var fileUploadElm = $("#thumbnail-upload-button");
            changeText(fileUploadElm, Granite.I18n.get("Default Thumbnail"));
            fileUploadElm.addClass(thumbnailRemoveButtonClass);
        }
        // Thumbnail upload
        $("coral-fileupload#thumbnail-upload-button").change(function(e) {
            e.preventDefault();
            var fileName = getFileName(e.target.uploadQueue[0]);
            if (fileName) {
                if (document.querySelector(uploadActionRel) &&
                  document.querySelector(uploadActionRel).dataset.ignorefield &&
                  JSON.parse(document.querySelector(uploadActionRel).dataset.ignorefield)) {
                    document.querySelector(".thumbnail-upload-action>input").disabled = true;
                } else {
                    if (typeof wizardApi !== "undefined") {
                        wizardApi.toggleNext(true);
                    }
                }

                if (fileName.match(/\.(jpg|jpeg|png|gif)/i)) {
                    if (e.target.uploadQueue.length && window.FileReader) {
                        var file = e.target.uploadQueue[0]._originalFile;
                        var reader = new FileReader();
                        reader.onload = function(e) {
                            setThumbnail(fileName, e.target.result, true);
                        };
                        reader.readAsDataURL(file);
                    } else {
                        setThumbnail(fileName, null, true);
                    }
                }
            }
        });
    });

    $(document).on("click", ".thumbnail-remove-button-active", function(e) {
        e.preventDefault();
        if (document.querySelector(uploadActionRel) &&
            document.querySelector(uploadActionRel).dataset.ignorefield &&
            JSON.parse(document.querySelector(uploadActionRel).dataset.ignorefield)) {
            document.querySelector(".thumbnail-upload-action>input").disabled = false;
        }
        var img = document.querySelector(thumbnailImg);
        if (!img) {
            img = document.querySelector(thumbnailIcon);
        }
        var newImg;
        if (previousImgElement !== null) {
            newImg = previousImgElement;
        } else {
            newImg = $("<img class=' show-grid'>").attr({
                width: img.width,
                height: img.height
            });
        }
        $(img).replaceWith(newImg);
        var fileUploadElm = $("#thumbnail-upload-button");
        changeText(fileUploadElm, Granite.I18n.get("Change Thumbnail"));
        fileUploadElm.removeClass(thumbnailRemoveButtonClass);
        $("#removemanualthumbnail").val(true);
        $("#thumbnail-upload-button input").val("");
        $("#thumbnail-upload-button input").attr("title", Granite.I18n.get("Change Thumbnail"));
        $(document).trigger("change-thumbnail");
    });

    function getFileName(input) {
        var name;
        if (input.file) {
            name = input.file.name;
        } else {
            name = input.value.split(/[\/\\]/).slice(-1)[0];// eslint-disable-line no-useless-escape
        }
        return name;
    }

    function changeText(element, text) {
        var label = element.find("coral-button-label");
        label.text(text);
        return element;
    }
})(jQuery);
