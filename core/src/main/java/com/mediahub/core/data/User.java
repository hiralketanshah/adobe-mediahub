package com.mediahub.core.data;

public class User {
    private String id;
    private String givenName;
    private String familyName;
    private String emailId;
    private String jobTitle;
    private String uoId;
    private String business;

    public User() {
		super();
	}

	public User(String id, String familyName, String givenName, String emailId, String jobTitle,String uoId,String business) {
        this.id = id;
        this.givenName = givenName;
        this.familyName = familyName;
        this.emailId = emailId;
        this.jobTitle = jobTitle;
        this.uoId = uoId;
        this.business=business;
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

