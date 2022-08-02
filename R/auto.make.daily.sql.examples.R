#-----------------------------------------------------------------------------------------
# Autogenerate some useful mysql snippets stored on the shared drive.
# Requires autogeneration as tables may occasionally change
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
file.1 <- '/Users/admin/../BIAD/BIAD/SQLcode/summary of all types of data by phase.sql'
#-----------------------------------------------------------------------------------------
# Pull all foreign keys
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#-----------------------------------------------------------------------------------------
# Fixed sections
#-----------------------------------------------------------------------------------------
section.1 <- c(
"SELECT Phases.PhaseID,
CONCAT_WS('; ', Phases.Culture1, Phases.Culture2, Phases.Culture3) AS Cultures,
Phases.Period,
Sites.SiteID,
Sites.SiteName,
Sites.Country,
Citations.CitationID,
Citations.Authors,
Citations.Title,
Citations.Publication,
Citations.Volume,
Citations.Pages,")

section.3 <- c(
"FROM BIAD.Phases
LEFT JOIN Sites ON Phases.SiteID = Sites.SiteID
LEFT JOIN PhaseCitation ON Phases.PhaseID = PhaseCitation.PhaseID
LEFT JOIN Citations ON PhaseCitation.CitationID = Citations.CitationID")

section.5 <- ";"
#-----------------------------------------------------------------------------------------
# Variable sections
#-----------------------------------------------------------------------------------------
x <- subset(d, REFERENCED_COLUMN_NAME=='PhaseID')
N <- nrow(x)
section.2 <- section.4 <- c()
for(n in 1:N){
	var <- x$TABLE_NAME[n]
	letter <- letters[n]
	section.2[n] <- paste(letter,".PhaseID AS '",var,"',",sep="")
	section.4[n] <- paste("LEFT JOIN (SELECT DISTINCT PhaseID FROM BIAD.",var,") ",letter," ON Phases.PhaseID = ",letter,".PhaseID",sep="")
	}
section.2[N] <- substr(section.2[N],1,nchar(section.2[N])-1)
#-----------------------------------------------------------------------------------------
# Bolt together and save
#-----------------------------------------------------------------------------------------
sql <- c(section.1, section.2, section.3, section.4, section.5)
writeLines(sql, con=file.1)
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------		
