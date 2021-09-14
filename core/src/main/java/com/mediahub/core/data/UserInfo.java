package com.mediahub.core.data;

public class UserInfo {
    private String uoId;
    private String business;
    private String organizationUnit;

    public UserInfo() {
		super();
	}

	public UserInfo(String uoId,String business, String organizationUnit) {
        this.uoId = uoId;
        this.business=business;
        this.organizationUnit = organizationUnit;
    }

	public String getUoId() {
		return uoId;
	}

	public void setUoId(String uoId) {
		this.uoId = uoId;
	}

	public String getBusiness() {
		return business;
	}

	public void setBusiness(String business) {
		this.business = business;
	}

	public String getOrganizationUnit() {
		return organizationUnit;
	}

	public void setOrganizationUnit(String organizationUnit) {
		this.organizationUnit = organizationUnit;
	}
	
}

