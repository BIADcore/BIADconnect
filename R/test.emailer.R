
#-----------------------------------------------------------------------------------------	
# Test emailer
#-----------------------------------------------------------------------------------------	
source('functions.R')
email <- gmailr::gm_mime(
    	To = "kittymede@gmail.com",
    	From = "BIAD.committee@gmail.com",
    	Subject = "pears",
    	body = "are you there?"
    	)

options(httr_oob_default=TRUE) 
gmailr::gm_auth_configure(path='../tools/email/gmailr.json')
gmailr::gm_auth(email = TRUE, cache = "../tools/email/.secret")
gmailr::gm_send_message(email)		
#-----------------------------------------------------------------------------------------		
