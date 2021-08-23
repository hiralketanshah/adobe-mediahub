package com.mediahub.core.data;

public class UserInfo {
    private String uoId;
    private String business;

    public UserInfo() {
		super();
	}

	public UserInfo(String uoId,String business) {
        this.uoId = uoId;
        this.business=business;
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
}

