#------------------------------------------------------------------
# Runs all R scripts beginning with 'auto.make.'
# This script is invoked by a regular CRON schedule (Admin),
# However, some scripts need not be run so frequently.
# Rather than specifying a CRON schedule for each script, 
# this script decides when to run which script, based on the filename.
#------------------------------------------------------------------
source('functions.R')
source('.Rprofile')
#------------------------------------------------------------------
run.day <- c('Tue','Fri')
today <- strsplit(date(),split=' ')[[1]][1]

files <- list.files()
daily.files <- files[grepl('auto.make.daily',files)]
weekly.files <- files[grepl('auto.make.weekly',files)]

ND <- length(daily.files)
NW <- length(weekly.files)

if(ND>0)for(n in 1:ND){
	file <- daily.files[n]
	tryCatch({
        cat(paste0("#-----------------------------------\n"));
        cat(paste('Starting to run script:',file,'at',date(),"\n"))
        source(file)
        cat(paste0("#------------ run ",file,", success ✅\n"));
    },
    error=function(err){
        cat(paste0("#------------ run ",file,", failed ❌\n"));
        print(err);
	})
}

if(NW>0 & today%in%run.day)for(n in 1:NW){
	file <- weekly.files[n]
	tryCatch({
        cat(paste0("#-----------------------------------\n"));
        cat(paste('Starting to run script:',file,'at',date(),"\n"))
        source(file)
        cat(paste0("#------------ run ",file,", success ✅\n"));
    },
    error=function(err){
        cat(paste0("#------------ run ",file,", failed ❌\n"));
        print(err);
	})
	}

#------------------------------------------------------------------
	
	
	
	
