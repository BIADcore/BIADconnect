#-----------------------------------------------------------------------------------------
# Check for missing user info.
# This script is reliant on files that are not stored on the public github:
# gmailr.json and .secret stored in tools/email
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# check for users that are missing from the zprivate_users table
#-----------------------------------------------------------------------------------------
d1 <- sql.wrapper(sql.command = "SELECT User FROM mysql.db WHERE Db='corex'",user,password)
d2 <- sql.wrapper(sql.command = "SELECT * FROM COREX.zprivate_users",user,password)

d <- merge(d1,d2,by.x='User',by.y='user',all=T)
d <- subset(d, !User%in%c('Rscripts','root','user'))
missing <- subset(d, is.na(email))$User
#-----------------------------------------------------------------------------------------
# construct emails
#-----------------------------------------------------------------------------------------

if(length(missing)>0){
	
	body1 <- 'The following users are missing from the zprivate_users table. Please add them to this table, or remove them as users: '
	body2 <- paste(missing,collapse=', ')
	body3 <- 'Many thanks,'
	body4 <- 'BIAD'	
	
	email <- gmailr::gm_mime(
    	To = subset(d2,administrator=='YES')$email,
    	From = "BIAD.committee@gmail.com",
    	Subject = 'Missing users information',
    	body = paste(body1,body2,body3,body4,sep="\n")
    	)
	}
#-----------------------------------------------------------------------------------------
# Send emails
#-----------------------------------------------------------------------------------------
if(length(missing)>0){
	gmailr::gm_auth_configure(path='../tools/email/gmailr.json')
	gmailr::gm_auth(email = TRUE, cache = "../tools/email/.secret")

	gmailr::gm_send_message(email)
	}
#-----------------------------------------------------------------------------------------