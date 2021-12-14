/*************************************************************************
* ADOBE CONFIDENTIAL
* ___________________
*
* Copyright 2016 Adobe
* All Rights Reserved.
*
* NOTICE: All information contained herein is, and remains
* the property of Adobe and its suppliers, if any. The intellectual
* and technical concepts contained herein are proprietary to Adobe
* and its suppliers and are protected by all applicable intellectual
* property laws, including trade secret and copyright laws.
* Dissemination of this information or reproduction of this material
* is strictly forbidden unless prior written permission is obtained
* from Adobe.
**************************************************************************/
(function(document, $, Coral) {
    "use strict";

    /**
     * @typedef {Object} SortConfig
     * @property {String} sortBy
     * @property {String} SortOrder
     */

    var sortContainerMap = new WeakMap();

    /**
     * Get the sortByItem whose value attribute is equal to passed value
     *
     * @param {HTMLElement} sortContainerEl container/parent of sort element
     * @param {String} value whose sortByItem needs to be retrieved
     * @returns {HTMLElement} sortByItem with provided value attribute
     */
    function getSortByItemFromValue(sortContainerEl, value) {
        var sortByEl = sortContainerEl.querySelector(".shell-collection-sortby");
        return sortByEl.items.getAll().find(function(sortByItem) {
            return sortByItem.getAttribute("value") === value;
        });
    }

    /**
     * Get the sortOrderItem whose value attribute is equal to passed value
     *
     * @param {HTMLElement} sortContainerEl container/parent of sort element
     * @param {String} value whose sortOrderItem needs to be retrieved
     * @returns {HTMLElement} sortOrderItem with provided value attribute
     */
    function getSortOrderItemFromValue(sortContainerEl, value) {
        var sortOrderEl = sortContainerEl.querySelector(".shell-collection-sortorder");
        return sortOrderEl.items.getAll().find(function(sortOrderItem) {
            return sortOrderItem.getAttribute("value") === value;
        });
    }

    /**
     * Get the selected sortByItem columnName
     *
     * @param {HTMLElement} sortContainerEl container/parent of sort element
     * @returns {Object} set of API related to shell sort Container.
     */
    function getSortContainerAPI(sortContainerEl) {
        var sortByEl = sortContainerEl.querySelector(".shell-collection-sortby");
        var sortOrderEl = sortContainerEl.querySelector(".shell-collection-sortorder");
        var sortOrderButtonEl = sortOrderEl.querySelector(".shell-collection-sortorder > button");
        /**
         * Get the selected sortByItem value
         *
         * @returns {String} value of selected sortByItem
         */
        function getSelectedSortByValue() {
            return sortByEl.selectedItem.getAttribute("value");
        }

        /**
         * Get the selected sortOrderItem value
         *
         * @returns {String} value of selected sortOrderItem
         */
        function getSelectedSortOrderValue() {
            return sortOrderEl.selectedItem.getAttribute("value");
        }

        return {
            /*
             * Get the sort values of shell sort container.
             * @returns {Config} containing sort values. keys-> sortBy, sortOrder, columnName.
             */
            getValues: function() {
                return {
                    sortBy: getSelectedSortByValue() || null,
                    sortOrder: getSelectedSortOrderValue() || null
                };
            },

            /*
             * This method sets the sort values of shell sort container.
             *
             * @param sortBy value .
             * @param sortOrder value.
             * @param silent if true only sync values and not trigger sorting
             */
            setValues: function(sortBy, sortOrder, silent) {
                var sortByItem = getSortByItemFromValue(sortContainerEl, sortBy);
                var sortOrderItem = getSortOrderItemFromValue(sortContainerEl, sortOrder);

                if (!sortBy || !sortOrder) {
                    // select none option and disable order
                    sortByItem = getSortByItemFromValue(sortContainerEl, null);
                    sortOrderItem = getSortOrderItemFromValue(sortContainerEl, "asc");
                    sortOrderButtonEl.disabled = true;
                } else if (sortByItem && sortOrderItem) {
                    sortOrderButtonEl.disabled = false;
                } else {
                    // do nothing
                    sortOrderButtonEl.disabled = false;
                    return;
                }

                if (!sortByItem.selected) {
                    sortByItem.set("selected", true, silent);
                }
                if (!sortOrderItem.selected) {
                    sortOrderItem.set("selected", true, silent);
                }
            }
        };
    }

    /**
     * Sync the sortContainer values based on collection sort values.
     *
     * @param {HTMLElement} containerEl target collection element
     * @param {HTMLElement} sortContainerEl shell sortContainer element
     */
    function syncSortContainerValues(collectionEl, sortContainerEl) {
        if (!collectionEl || !sortContainerEl) {
            return;
        }

        var sortAPI = $(collectionEl).adaptTo("foundation-collection-sort");
        var sortContainerAPI = getSortContainerAPI(sortContainerEl);

        var sortValues = sortAPI.getSortValues();

        sortContainerAPI.setValues(sortValues["sortBy"], sortValues["sortOrder"]);
    }

    /**
     * Sync the cookie value with the collection element sort values. Never sync with sortContainer values.
     *
     * @param {HTMLElement} containerEl target collection element
     * @param {HTMLElement} sortContainerEl container/parent of sort element
     */
    function syncCookie(collectionEl, sortContainerEl) {
        if (!collectionEl || !sortContainerEl) {
            return;
        }

        var consoleId = sortContainerEl.dataset.shellCollectionpageConsoleid;
        var sortAPI = $(collectionEl).adaptTo("foundation-collection-sort");
        var sortValues = sortAPI.getSortValues();

        if (!consoleId) {
            return;
        }

        var sortBykey = consoleId + "-sortName";
        var sortOrderkey = consoleId + "-sortDir";

        if (sortValues["sortBy"] && sortValues["sortOrder"]) {
            $.cookie(sortBykey, sortValues["sortBy"], { expires: 7, path: "/" });
            $.cookie(sortOrderkey, sortValues["sortOrder"], { expires: 7, path: "/" });
        } else {
            $.removeCookie(sortBykey, { path: "/" });
            $.removeCookie(sortOrderkey, { path: "/" });
        }
    }

    /**
     * returns the related sort container for collection
     *
     * @param {HTMLElement} containerEl target collection element
     * @returns {HTMLElement} sortContainerEl container/parent of sort element
     */
    function getRelatedSortContainer(collectionEl) {
        if (sortContainerMap.has(collectionEl)) {
            return sortContainerMap.get(collectionEl);
        } else {
            var matchedSortContainerEl = Array.from(document.querySelectorAll(".shell-collection-sortcontainer"))
                .find(function(sortContainerEl) {
                    return collectionEl.matches(sortContainerEl.dataset.shellCollectionTarget);
                });

            if (matchedSortContainerEl) {
                sortContainerMap.set(collectionEl, matchedSortContainerEl);
                return matchedSortContainerEl;
            }
            return null;
        }
    }

    /**
     * Removes the saved sort cookie
     *
     * @param {HTMLElement} containerEl target collection element
     * @param {HTMLElement} sortContainerEl container/parent of sort element
     */
    function clearCookie(collectionEl, sortContainerEl) {
        if (!collectionEl || !sortContainerEl) {
            return;
        }

        var consoleId = sortContainerEl.dataset.shellCollectionpageConsoleid;

        if (!consoleId) {
            return;
        }

        var sortBykey = consoleId + "-sortName";
        var sortOrderkey = consoleId + "-sortDir";

        $.removeCookie(sortBykey, { path: "/" });
        $.removeCookie(sortOrderkey, { path: "/" });
    }

    /**
     * Used to track the event performed by user.
     * This function tracks interactions with breadcrumbs
     * OMEGA Implementation
     *
     * @param {HTMLElement} sortContainerEl container/parent of sort element.
     * @param {HTMLElement} sortContainerItem sortContainerEl child element which user interacted with.
     * @param {Object} attributes optional extra information that needs to be sent
     */
    function trackEvent(sortContainerEl, sortContainerItem, attributes) {
        var trackElement = sortContainerItem.matches(".shell-collection-sortby") ? "sortBy" : "sortOrder";

        var trackData = {
            element: trackElement,
            feature: "No feature defined",
            type: "sort",
            action: "click",
            widget: {
                name: trackElement,
                type: "shell-sort"
            },
            attributes: attributes || {}
        };

        $(window).adaptTo("foundation-tracker").trackEvent(trackData);
    }


    /**
     * This will be triggered when sort component value has been changed from UI/code i.e
     * the sort container values are correct and other values needs to be updated.
     */
    $(document).on("coral-cyclebutton:change",
        ".shell-collection-sortby, .shell-collection-sortorder", function(event) {
            var self = this;
            var sortContainerEl = self.closest(".shell-collection-sortcontainer");
            var targetSelector = sortContainerEl.dataset.shellCollectionTarget;
            var collectionEl = document.querySelector(targetSelector);

            if (!targetSelector || !collectionEl) {
                return;
            }

            var collection = $(collectionEl);

            var sortContainerAPI = getSortContainerAPI(sortContainerEl);
            var sortContainerValues = sortContainerAPI.getValues();
            var sortBy = sortContainerValues["sortBy"];
            var sortOrder = sortContainerValues["sortOrder"];
            var sortAPI = collection.adaptTo("foundation-collection-sort");

            sortContainerEl.sortTimeoutId && clearTimeout(sortContainerEl.sortTimeoutId);

            // void twice execution of sorting if event occur quickly
            sortContainerEl.sortTimeoutId = setTimeout(function() {
                delete sortContainerEl.sortTimeoutId;
                sortAPI.doSort(sortBy, sortOrder) &&
                trackEvent(sortContainerEl, self, { sortBy: sortBy, sortOrder: sortOrder });
            }, 50);
        });

    /**
     * This will be triggered when sorting has been changed for collection.
     * the collection/sortAPI values are correct values and other values needs to be synced.
     */
    $(document).on("foundation-collection-sort:changed", ".foundation-collection", function() {
        var collectionEl = this;

        var sortContainerEl = getRelatedSortContainer(collectionEl);

        if (!sortContainerEl) {
            return;
        }
        // Even when sortContainer are hidden, the values/cookies should get sync.
        Coral.commons.ready(function() {
            syncSortContainerValues(collectionEl, sortContainerEl);
            syncCookie(collectionEl, sortContainerEl);
        });

        // GRANITE-31732
        sortContainerEl[collectionEl.matches(".foundation-layout-table.foundation-collection")
            ? "setAttribute" : "removeAttribute"]("hidden", "");
    });

    /**
     * This will be executed before sorting starts.
     */
    $(document).on("foundation-collection-sort:beforeChange", ".foundation-collection", function() {
        var sortAPI = $(this).adaptTo("shell-collection-sort");
        if (sortAPI) {
            sortAPI.clearCookie();
        }
    });

    /**
     * Change the sort order icon to size 'XS'
     * coral-cyclebutton does not support iconSize, this is a workaround
     */
    $(document).on("foundation-contentloaded", function(e) {
        e.target.querySelectorAll(".shell-collection-sortcontainer").forEach(function(sortContainerEl) {
            Coral.commons.ready(sortContainerEl, function() {

                if(null==$.cookie("cq-assets-files-files-sortName")){
					$.cookie("cq-assets-files-files-sortName", "name", { expires: 7, path: "/" });
                    $.cookie("cq-assets-files-files-sortDir", "asc", { expires: 7, path: "/" });
                    location.reload();

                }
                var sortOrder = sortContainerEl.querySelector(".shell-collection-sortorder");
                var sortOrderButton = sortOrder.querySelector(".shell-collection-sortorder > button");
                if (sortOrderButton && sortOrderButton.iconSize !== Coral.Icon.size.EXTRA_SMALL) {
                    sortOrderButton.iconSize = Coral.Icon.size.EXTRA_SMALL;
                }
            });

            var collectionEl = document.querySelector(sortContainerEl.dataset.shellCollectionTarget);

            if (collectionEl) {
                sortContainerMap.set(collectionEl, sortContainerEl);

                // GRANITE-31732
                sortContainerEl[collectionEl.matches(".foundation-layout-table.foundation-collection")
                    ? "setAttribute" : "removeAttribute"]("hidden", "");
            }
        });
    });


    /**
     * adaptTo implementation
     */
    $(window).adaptTo("foundation-registry").register("foundation.adapters", {
        type: "shell-collection-sort",
        selector: ".foundation-collection",
        adapter: function(collectionEl) {
            var sortContainerEl = getRelatedSortContainer(collectionEl);

            if (!sortContainerEl) {
                return null;
            }

            return {
                clearCookie: function() {
                    clearCookie(collectionEl, sortContainerEl);
                }
            };
        }
    });
})(document, Granite.$, Coral);
