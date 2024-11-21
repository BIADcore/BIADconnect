#-----------------------------------------------------------------------------------------
# Check for missing user info.
# This script is reliant on files that are not stored on the public github:
# gmailr.json and .secret stored in tools/email
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# check for users that are missing from the zprivate_users table
#-----------------------------------------------------------------------------------------
conn <- init.conn()
d1 <- query.database(conn = conn, sql.command = "SELECT * FROM `BIAD`.`zprivate_users`")
d2 <- query.database(conn = conn, sql.command = "SELECT User FROM `mysql`.`user`")

missing.from.db <- d1$user[!d1$user%in%d2$User]
missing.from.zprivate <- d2$User[!d2$User%in%d1$user]
missing.from.zprivate <- missing.from.zprivate[!missing.from.zprivate%in%c('Rscripts','user','root')]
#-----------------------------------------------------------------------------------------
# construct emails
#-----------------------------------------------------------------------------------------
body1 <- 'The following users are missing from the zprivate_users table. Please add them to this table, or remove them as users: '
body2 <- paste(missing.from.zprivate,collapse=', ')
body3 <- 'The following users are extra in the zprivate_users table. Please remove them from this table, or add them as users: '
body4 <- paste(missing.from.db,collapse=', ')	
body5 <- 'Please do not reply. Many thanks,'
body6 <- 'BIAD'	
	
email <- gmailr::gm_mime(
   	To = subset(d1,administrator=='YES')$email,
   	From = 'BIAD.committee@gmail.com',
   	Subject = 'Missing users information',
   	body = paste(body1,body2,body3,body4,body5,body6,sep="\n\n")
   	)
#-----------------------------------------------------------------------------------------
# Send emails
#-----------------------------------------------------------------------------------------
if(length(missing.from.db)>0 | length(missing.from.zprivate)>0 ){
	gmailr::gm_auth_configure(path='../tools/email/gmailr.json')
	gmailr::gm_auth(email = TRUE, cache = "../tools/email/.secret copy")
	gmailr::gm_send_message(email)
	}
#-----------------------------------------------------------------------------------------
disconnect()
