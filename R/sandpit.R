#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# Convert skeleton parts to a zoptions look up
#-----------------------------------------------------------------------------------------
map <- read.csv('map.csv')

d <- sql.wrapper(sql.command = "SELECT Element FROM BIAD.FaunalBones",user,password,hostname,hostuser,keypath,ssh)

sql <- c()
N <- nrow(map)
for(n in 1:N){
	sql[n] <- paste("UPDATE BIAD.FaunalBones SET Element='",map$new[n],"' WHERE Element='",map$old[n],"';",sep='')
	}

sql.wrapper(sql,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
d1 <- sql.wrapper(sql.command = "SELECT Element FROM BIAD.zoptions_SkeletalElements",user,password,hostname,hostuser,keypath,ssh)

a <- d1[,1]
b <- map$new
b[!b%in%a]
#-----------------------------------------------------------------------------------------	
# Test emailer
#-----------------------------------------------------------------------------------------	
source('functions.R')
email <- gmailr::gm_mime(
    	To = "???@gmail.com",
    	From = biad.address,
    	Subject = "pears",
    	body = "are you there?"
    	)

options(httr_oob_default=TRUE) 
gmailr::gm_auth_configure(path='../tools/email/gmailr.json')
gmailr::gm_auth(email = TRUE, cache = "../tools/email/.secret")
gmailr::gm_send_message(email)		
#-----------------------------------------------------------------------------------------		

df <- data.frame(element=b[!b%in%a])
write.csv(df,file='new.csv', row.names=F)
#-----------------------------------------------------------------------------------------	
get.plink.pid <- function(){
	require(installr)
	all.processes <- get_tasklist()
	plink.processes <- all.processes$PID[all.processes$`Image Name`=='plink.exe']
return(plink.processes)}
