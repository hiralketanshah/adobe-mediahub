/*************************************************************************
 * ADOBE CONFIDENTIAL
 * ___________________
 *
 * Copyright 2018 Adobe
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
package apps.granite.ui.components.shell.collectionpage.sort;

import com.adobe.granite.ui.components.AttrBuilder;
import org.apache.sling.api.resource.Resource;
import org.apache.sling.api.resource.ValueMap;
import javax.servlet.http.Cookie;
import com.adobe.granite.ui.components.htl.ComponentHelper;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Sort extends ComponentHelper {
    private final String DEFAULT_CONFIG_PATH = "granite/ui/content/shell/collectionpage/sort";
    private boolean ordered;
    private boolean override;
    private String customConfigPath;

    private ArrayList<SortByItem> sortByItems = new ArrayList<SortByItem>();
    private ArrayList<SortOrderItem> sortOrderItems = new ArrayList<SortOrderItem>();
    private static final Logger LOGGER = LoggerFactory.getLogger(Sort.class);

    @Override
    public void activate() {
        ValueMap vm  = this.getResource().getValueMap();
        ordered = vm.get("ordered", false);
        override = vm.get("override", false);
        customConfigPath = vm.get("configPath", String.class);

        Resource customResource = this.getResourceResolver().getResource(customConfigPath);
        Resource defaultResource = this.getResourceResolver().getResource(DEFAULT_CONFIG_PATH);

        addSortByItems(defaultResource.getChild("sortBy"), override);
        addSortOrderItems(defaultResource);
        if(customResource != null){
            addSortByItems(customResource, false);
        }
    }

    public ArrayList<SortByItem> getSortByItems() {
        if(ordered) {
            Collections.sort(sortByItems);
        }
        return new ArrayList<>(sortByItems);
    }

    public ArrayList<SortOrderItem> getSortOrderItems() {
        return new ArrayList<>(sortOrderItems);
    }

    public Map<String, String> getAttributes() {
        AttrBuilder attrs = getInheritedAttrs();
        populateCommonAttrs(attrs);
        return attrs.getData();
    }

    public String getSelectedSortByValue() {
        SortByItem selectedItem = null;
        for(SortByItem item : sortByItems){
            if(item != null && item.getSelected()){
                selectedItem = item;
                break;
            }
        }
        if(null == selectedItem){
            return getCookie(getConsoleId() + "-sortName");
        } else {
            return selectedItem.getValue();
        }
    }

    public String getSelectedSortOrderValue() {
        SortOrderItem selectedItem = null;
        for(SortOrderItem item : sortOrderItems){
            if(item != null && item.getSelected()){
                selectedItem = item;
                break;
            }
        }
        if(null == selectedItem){
            return getCookie(getConsoleId() + "-sortDir");
        } else {
            return selectedItem.getValue();
        }
    }

    public boolean getOrdered(){
        return ordered;
    }

    public boolean getOverride(){
        return override;
    }

    public String getLabel(){
        return "Sort by";
    }

    public String getCustomConfigPath(){
        return customConfigPath != null ? customConfigPath : "";
    }

    private void addSortByItems(Resource resource, boolean override) {
        // todo add getItemDataSource in htl componenthelper
        final Resource items = resource.getChild("items");
        if(items != null) {
            Iterator<Resource> it = items.listChildren();
            for (; it.hasNext();) {
                Resource item = it.next();
                ValueMap vm = item.getValueMap();
                String text = vm.get("text", String.class);
                String value = vm.get("value", String.class);
                String columnName = vm.get("columnName", String.class);

                if(!override || vm.get("compulsory", false)){
                    sortByItems.add(new SortByItem(text, value, vm.get("selected", "false")));
                }
            }
        }
    }

    private void addSortOrderItems(Resource resource) {
        // todo add getItemDataSource in htl componenthelper
        final Resource sortOrder = resource.getChild("sortOrder");
        Iterator<Resource> it = sortOrder.getChild("items").listChildren();
        for (; it.hasNext();) {
            Resource item = it.next();
            ValueMap vm = item.getValueMap();
            String text = vm.get("text", String.class);
            String value = vm.get("value", String.class);
            String icon = vm.get("icon", String.class);
            sortOrderItems.add(new SortOrderItem(text, value, icon));
        }
    }

    public class SortByItem implements Comparable<SortByItem> {
        private final String label;
        private final String value;
        private boolean selected;

        SortByItem(String label, String value, String columnName) {
            this.label = label;
            this.value = value;
            this.selected = false;
            if(columnName.equalsIgnoreCase("true")){
				this.selected = true;
            }
        }

        public String getLabel() {
            return label;
        }

        public String getValue() {
            return value;
        }

        public boolean getSelected() {
            return selected;
        }

        public int compareTo(SortByItem item) {
            return this.getLabel().compareTo(item.getLabel());
        }
    }

    public class SortOrderItem {
        private final String label;
        private final String value;
        private final String icon;
        private boolean selected;

        SortOrderItem(String label, String value, String icon) {
            this.label = label;
            this.value = value;
            this.icon = icon;
            this.selected = value.equals(getSelectedSortOrderValue());
        }

        public String getLabel() {
            return label;
        }

        public String getValue() {
            return value;
        }

        public boolean getSelected() {
            return selected;
        }

        public String getIcon() {
            return icon;
        }
    }

    private String getConsoleId() {
        AttrBuilder attrs = getInheritedAttrs();
        return attrs.getData().get("data-shell-collectionpage-consoleid");
    }

    private String getCookie(String key) {
        try {
            Cookie cookie = this.getRequest().getCookie(key);

            if (cookie == null) {
                return null;
            }

            return URLDecoder.decode(cookie.getValue(), "utf-8");
        } catch (UnsupportedEncodingException impossible) {
            throw new RuntimeException(impossible);
        }
    }

}
