#------------------------------------------------------------------
# Runs all R scripts beginning with 'auto.make.'
#------------------------------------------------------------------
source('functions.R')
#------------------------------------------------------------------
files <- list.files()
files_to_run <- files[substr(files,1,10)=='auto.make.']
N <- length(files_to_run)
for(n in 1:N){
	print(date())
	source(files_to_run[n])
	}
date()
#------------------------------------------------------------------
	
	
	
	