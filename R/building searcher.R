#----------------------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
library(ssh)
#----------------------------------------------------------------------------------------------------
commands <- c(
	"cd BIAD/R",
	"/Library/Frameworks/R.framework/Resources/bin/R CMD BATCH --no-save server.run.tree.maker.R tmp/tmp.Rout"
	)
#----------------------------------------------------------------------------------------------------
tmp.path <- "BIAD/R/tmp"
session <- ssh_connect(host=paste(hostuser,"@",hostname,sep=''), keyfile='C:/Users/adrian/.ssh/BIAD.pem')
ssh_exec_wait(session, command = paste("mkdir",tmp.path))
scp_upload(session, files = "functions.R" , to = tmp.path)
scp_upload(session, files = ".Rprofile" , to = tmp.path)
ssh_exec_wait(session, command = commands)
scp_download(session, files = "tmp/tmp.Rout", to = ".")
ssh_disconnect(session)

#----------------------------------------------------------------------------------------------------


library(data.tree)
tree <- FromListSimple(x)
plot(tree)
print(tree)
#----------------------------------------------------------------------------------------------------
d <- read.csv('test.csv', fileEncoding='Unicode', encoding='Unicode')
d[,2]


#----------------------------------------------------------------------------------------------------



