#-----------------------------------------------------------------------------------------
# Check for missing citations
# This script is reliant on files that are not stored on the public github:
# gmailr.json and .secret stored in tools/email
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# check for users that are missing from the zprivate_users table
#-----------------------------------------------------------------------------------------
sources <- '/Users/admin/../BIAD/BIAD/SOURCES/primary sources/'
d1 <- query.database(user, password, sql.command = "SELECT * FROM BIAD.zprivate_users")
d2 <- query.database(user, password, sql.command = "SELECT CitationID, user_added FROM BIAD.citations")
#-----------------------------------------------------------------------------------------
x <- Sys.glob(paths=paste(sources,'*',sep=''))
x <- gsub(sources,'',x)
x <- tolower(x)
x <- gsub('.pdf','',x)
x <- utf8::utf8_normalize(x) 

sub <- subset(d2, !is.na(user_added))
sub$user <- matrix(unlist(strsplit(sub$user_added,split='@')),2,nrow(sub))[1,]
sub <- subset(sub, user %in% d1$user)
sub <- sub[!sub$CitationID%in%x,]

names <- names(table(sub$user))
options(httr_oob_default=TRUE) 
for(n in 1:length(names)){
	
	missing <- sub$CitationID[sub$user==names[n]]
	email.address <- d1$email[d1$user==names[n]]
	
	body1 <- 'The following citations are missing from the shared drive, but are in the Citations table. Please put them in the shared drive: '
	body2 <- paste(missing,collapse=', ')
	body3 <- 'Many thanks,'
	body4 <- 'BIAD'	
	
	email <- gmailr::gm_mime(
    	To = email.address,
    	From = biad.address,
    	Subject = 'Missing citations',
    	body = paste(body1,body2,body3,body4,sep="\n")
    	)
  	
    if(length(missing)!=0){
		gmailr::gm_auth_configure(path='../tools/email/gmailr.json')
		gmailr::gm_auth(email = TRUE, cache = "../tools/email/.secret copy")
		gmailr::gm_send_message(email)	
		}
	}
#-----------------------------------------------------------------------------------------	
	
#-----------------------------------------------------------------------------------------		
