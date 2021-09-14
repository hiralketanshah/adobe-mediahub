package com.mediahub.core.data;

public class User {
    private String id;
    private String givenName;
    private String familyName;
    private String emailId;
    private String jobTitle;
    private String city;
    private String country;    
    private String uoId;
    private String business;
    private String statusId;
    private String status;
    private String organizationUnit;

    public User() {
		super();
	}

	public User(String id, String familyName, String givenName, String emailId, String jobTitle,String uoId,String business, String statusId, String status, String organizationUnit) {
        this.id = id;
        this.givenName = givenName;
        this.familyName = familyName;
        this.emailId = emailId;
        this.jobTitle = jobTitle;
        this.uoId = uoId;
        this.business=business;
        this.organizationUnit = organizationUnit;
    }

    public String getId() {
        return id;
    }

    public String getGivenName() {
        return givenName;
    }

    public String getFamilyName() {
        return familyName;
    }

    public String getEmailId() {
        return emailId;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setGivenName(String givenName) {
        this.givenName = givenName;
    }

    public void setFamilyName(String familyName) {
        this.familyName = familyName;
    }

    public void setEmailId(String emailId) {
        this.emailId = emailId;
    }

	public String getJobTitle() {
		return jobTitle;
	}

	public void setJobTitle(String jobTitle) {
		this.jobTitle = jobTitle;
	}
	
	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
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

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getStatusId() {
		return statusId;
	}

	public void setStatusId(String statusId) {
		this.statusId = statusId;
	}
	
	public String getOrganizationUnit() {
		return organizationUnit;
	}

	public void setOrganizationUnit(String organizationUnit) {
		this.organizationUnit = organizationUnit;
	}

}

