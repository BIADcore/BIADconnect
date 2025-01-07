#------------------------------------------------------------------
# Private stuff, this script is not pushed to git, so can be lost if recloned
#------------------------------------------------------------------
source('functions.R')
source('functions.database.connect.R')
#------------------------------------------------------------------
# A single phaseCitations table for all citations
#------------------------------------------------------------------
# 1. Might need a better name Not intuitive
# 2. Needs a trigger that autopopulates, provided triplet values are unique. Probably use an update
conn  <- init.conn()

# clean up triplet duplicates
# Automatically update 
# Need to ensure no duplicate triplets, and if there is a dup, use the one with greater info

for(t in c(2,5,6)){

options(warn=2)
	table <- tables$Tablename[t]
	x <- query.database(sql.command = paste("SELECT `PhaseID`, `CitationID` ,`time_added`,`user_added`,`time_last_update`, `user_last_update` FROM `BIAD`.`",table,"`;",sep=''),conn = conn)
	x <- subset(x, !is.na(CitationID))
	x <- unique(x)
	each <- unique(x[,c('PhaseID', 'CitationID')])
	each <- subset(each, !is.na(PhaseID))
	keep <- matrix(, nrow(each), ncol(x))
	for(n in 1:nrow(each)){
		tmp <- subset(x, PhaseID==each$PhaseID[n] & CitationID==each$CitationID[n])
		filled <- rowSums(!is.na(tmp))
		tmp <- tmp[which(filled==max(filled)),]
		tmp <- tmp[1,]
		keep[n,] <- as.character(tmp)
		}



# apply the table here, and compare to what is already in DB, 


	x <- keep

	sql.command <- c()
	for(n in 1:nrow(x)){
		tmp <- x[n,]
		tmp <- gsub("'","\\'",tmp,fixed=TRUE)
		values <- paste(c(table,tmp),collapse="','")
		txt <- paste("INSERT INTO `BIAD`.`PhaseCitation` (`DataCited`, `PhaseID`, `CitationID`,`time_added`,`user_added`,`time_last_update`, `user_last_update`) VALUES ('",values,"');",sep='')
		sql.command[n] <- txt
		}
	sql.command <- gsub("'NA'","NULL", sql.command)
	
	query.database(sql.command = sql.command,conn = conn)
	}