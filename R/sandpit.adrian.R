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
	tasklist <- shell(cmd='tasklist /nh /fo "csv" /fi "imagename eq plink.exe"',intern=T)
	pid <- c()
	for(n in 1:length(tasklist)){
		pid[n] <- as.numeric(strsplit(tasklist[n],split='\",\"')[[1]][2])
		}
return(pid)}
#-----------------------------------------------------------------------------------------	
open.ssh.tunnel <- "plink -ssh BIAD@macelab-server.biochem.ucl.ac.uk -i C:/Users/adrian/.ssh/BIAD.ppk -N -L 3306:macelab-server.biochem.ucl.ac.uk:3306"
shell(open.ssh.tunnel, wait=FALSE)

drv <- RMySQL::MySQL()	
require(odbc)
con <- dbConnect(drv, user='sam', password='sam', dbname='BIAD', host = "127.0.0.1", port=3306)
dbDisconnect(con)

pid.remove <- get.plink.pid()

close.ssh.tunnel <- paste('taskkill /f /fi "pid eq ',pid.remove,'"',sep='')
shell(close.ssh.tunnel)

#-----------------------------------------------------------------------------------------	
require(ssh)
session <- ssh_connect(host='biad@macelab-server.biochem.ucl.ac.uk', keyfile="C:/Users/adrian/.ssh/id_rsa", verbose=1)

drv <- RMySQL::MySQL()	
require(odbc)
con <- dbConnect(drv, user='sam', password='sam', dbname='BIAD', host = "127.0.0.1", port=3306)
dbDisconnect(con)

ssh_disconnect(session)

#-----------------------------------------------------------------------------------------	
session <- ssh_connect(host='biad@macelab-server.biochem.ucl.ac.uk', keyfile="C:/Users/adrian/.ssh/id_rsa", verbose=0)

out <- ssh_exec_wait(session, "R CMD inner.R")
cat(rawToChar(out$stdout))

ssh_disconnect(session)


ssh -L 8888:127.0.0.1:3306 BIAD@macelab-server.biochem.ucl.ac.uk -i C:/Users/adrian/.ssh/BIAD.ppk
ssh BIAD@macelab-server.biochem.ucl.ac.uk -i C:/Users/adrian/.ssh/BIAD.ppk -N -L 3306:macelab-server.biochem.ucl.ac.uk:3306

