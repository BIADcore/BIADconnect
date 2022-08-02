#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# import problems
#-----------------------------------------------------------------------------------------
pha <- sql.wrapper("SELECT * FROM BIAD.Phases",user,password,hostname,hostuser,keypath,ssh)
new <- read.csv('C14Import.csv', encoding='UTF8')
both <- merge(pha[,c('PhaseID','SiteID','Period')],new[,c('PhaseID','SiteID','Period')],by='PhaseID')

bad.i <- (both$SiteID.x!=both$SiteID.y) | (both$Period.x!=both$Period.y)
both[bad.i,]

new <- read.csv('Isot.csv', encoding='UTF8')
#-----------------------------------------------------------------------------------------
# merge FaunalBones and FaunalBiometrics ino a new FaunalBones
#-----------------------------------------------------------------------------------------
bo <- sql.wrapper("SELECT * FROM BIAD.FaunalBones",user,password,hostname,hostuser,keypath,ssh)
bi <- sql.wrapper("SELECT * FROM BIAD.FaunalBiometrics",user,password,hostname,hostuser,keypath,ssh)
both <- merge(bo,bi,by='BoneID')
newID <- both$MetricID+9000000
newID <- as.character(newID)
newID <- paste('M',substring(newID,2),sep='')
stamps <- data.frame(time_added=rep(NA,nrow(both)), user_added=rep(NA,nrow(both)), time_last_update=both$timestamp.x, user_last_update=both$userstamp.x)
castrate <- data.frame(Castrate=NA)
new <- cbind(data.frame(MetricID=newID),both[,c('BoneID','PhaseID','TaxonCode','Element','Sex')],castrate,both[,c('Measurement','Value')],stamps)

i <- new$Sex=='Undet'
new$Sex[i] <- 0.5; new$Castrate[i] <- NA
i <- new$Sex=='Castrate'
new$Sex[i] <- 0; new$Castrate[i] <- 1
i <- new$Sex=='Castrate?'
new$Sex[i] <- NA; new$Castrate[i] <- 0.5
i <- new$Sex=='Male'
new$Sex[i] <- 0; new$Castrate[i] <- 0
i <- new$Sex=='Male?'
new$Sex[i] <- 0.25; new$Castrate[i] <- NA
i <- new$Sex=='Female'
new$Sex[i] <- 1; new$Castrate[i] <- 0
i <- new$Sex=='Female?'
new$Sex[i] <- 0.75; new$Castrate[i] <- NA

sql <- c()
for(n in 1:nrow(new)){
	i <- !is.na(new[n,])
	names <- paste(names(new)[i],collapse="`, `")
	values <- paste(new[n,i], collapse="', '")
	sql[n] <- paste("INSERT INTO `BIAD`.`FaunalAdrian` (`",names, "`) VALUES ('", values, "')",sep="")
	}

sql.wrapper(sql,user,password,hostname,hostuser,keypath,ssh)

#-----------------------------------------------------------------------------------------
# quick look at interacting with google scholar
#-----------------------------------------------------------------------------------------
library(rvest)
url <- "https://scholar.google.com/scholar?q=apples+pears"
url <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=apples+pears&btnG="

page <- url %>% read_html() %>% html_elements

page <- read_html(url)
all.links <- page%>% html_nodes("a") %>% html_attr( "href")
#-----------------------------------------------------------------------------------------
