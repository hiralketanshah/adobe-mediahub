/*
 * ADOBE CONFIDENTIAL
 *
 * Copyright 2014 Adobe Systems Incorporated
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
;(function(Granite, $, undefined) {
    "use strict";

    //var modalActivator = ".asset-pod-configuration-activator";
    var ui = $(window).adaptTo("foundation-ui");

    var openButton = ".asset-pod-configuration-activator";
    var cuiModal;
    var rel = ".cq-projects-admin-assetpod-dialog";

    /**
     * load model will attempt to load the collection dialog node
     * and create a CUI.Modal when loaded.  The result is set
     * into the cuiModal variable when the load is complete.
     */
    function loadModel(path, assetPath) {
        var p = $.Deferred();


        var ck = Date.now();
        // need to pass the suffix to the dialog to allow it to resolve the resource
        var suffix = getSuffix(window.location.pathname);

        var src = '/mnt/overlay/cq/core/content/projects/details/assetpodconfig.html' + suffix + '?ch_ck=' + ck + "&_charset_=utf-8&path=" + path + "&assetPath=" + assetPath;

        var xhr = $.get(Granite.HTTP.externalize(src))
            .done(function(html) {
                var result = Granite.UI.Foundation.Utils.processHtml(html);

                var el = $(result);
                $(document.body).append(el);

                cuiModal = new CUI.Modal({
                    element: el,
                    visible: false
                });

                // need to trigger the modal to initialize the components in the modal
                $(document).trigger('cui-contentloaded');

                p.resolve();
            })
            .fail(function() {
                p.reject(Granite.I18n.get("Unable to load the collections modal"));
            });

        return p;
    }

    /**
     * Upon click of the asset pod's configuration, load the modal dialog
     * then wire up listeners for the submit and reset.
     */
    $(document).on("click" + openButton, openButton, function(e) {
        e.preventDefault();
        var path = $(e.target).closest('.cq-projects-Pod-assetPod').attr('data-path');
        var assetPath = $(e.target).closest('.cq-projects-Pod-assetPod').find('.cq-projects-Pod-footer-text').attr('data-path');
        loadModel(path, assetPath)
            .done(function() {
                cuiModal.show();
                cuiModal.on('reset', function() {
                    removeForm(false);
                });

                var $form = cuiModal.$element;


                    $form.off('foundation-form-submitted' + rel)
                        .on('foundation-form-submitted' + rel, function(e) {
                            $form.data('modal').hide();


                                // TODO: Figure out why this 'refresh' doesn't work.  Articles get 'display:none' style for
                                // some reason.
                                var contentApi = $('.foundation-collection').adaptTo('foundation-collection');
                                contentApi.reload();

                                //location.reload();

                        });


                // $form.on('submit.assetpod', function(e) {
                //     e.preventDefault();
                //
                //     ui.wait();
                //     var ajaxOptions = {
                //         url: ,
                //         type: "post",
                //         success: function(data, status, request) {
                //             ui.clearWait();
                //             removeForm(true);
                //         },
                //         error: function(jqXHR, error, message) {
                //             ui.clearWait();
                //             ui.alert(Granite.I18n.get("Error"), message, error);
                //             removeForm(true);
                //         }
                //     };
                //     CQ.projects.ajaxSubmitFileForm(ajaxOptions, $form);
                // });
            })
            .fail(function(error) {
                ui.alert(Granite.I18n.get("Error"), error, "error");
            });
    });

    /**
     * Remove the modal/form element from the dom, pass in true if you
     * want the content to be refreshed
     * @param refresh true to refresh
     */
    function removeForm(refresh) {
        cuiModal.hide(); // removes the black background
        cuiModal.$element.remove(); // removes the element from the dom
        if (refresh) {
            window.location.reload();
        }
    }

    /**
     * Return the suffix from the current url
     * @param href
     * @returns {string} the suffix
     */
    function getSuffix(href) {
        var lastDot = href.lastIndexOf('.'),
            suffixStart = href.indexOf('/', lastDot);
        if (suffixStart >= 0) {
            return href.substr(suffixStart)
        } else {
            return '';
        }
    };

})(Granite, Granite.$);