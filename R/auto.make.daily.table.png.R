#-------------------------------------------------------------------------------
# Generate a bunch of examples of tables as .png, for embedding in wiki
#-------------------------------------------------------------------------------
library(gridExtra)
tables <- c('citations','phasecitation','phases','phasetypes','sites','zoptions_types')

theme <- ttheme_minimal(
	core = list(fg_params = list(hjust = 0, x = 0.1, fontsize = 18, col='blue')),
	colhead = list(fg_params = list(hjust = 0, x = 0.1, fontsize = 12, col='black'))
	)
#-------------------------------------------------------------------------------
for(table in tables){

	sql.command <- paste("SELECT * FROM BIAD.",table,sep='')
	d <- query.database(user, password, 'biad', sql.command)[1:5,]
	d[is.na(d)] <- '\\N'
	d$time_added <- '\\N'
	d$user_added <- '\\N'
	d$time_last_update <- '\\N'
	d$user_last_update <- '\\N'

	for(n in 1:ncol(d)){
		i <- nchar(d[,n])>20
		d[i,n] <- paste(substr(d[i,n],1,20),'...',sep='')
		}

	path <- paste('../tools/plots/',table,'.png',sep='')
	png(path,width=2200, height=180)
	grid.table(d, theme=theme,rows=rep('',5))
	dev.off()
	}
#-------------------------------------------------------------------------------