
#------------------------------------------------------------------
sql.command <- "SELECT table_comment FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='COREX' AND table_name='Phases';"
d <- sql.wrapper(sql.command,user,password)
#------------------------------------------------------------------
# Write to the Gist
writeLines(d[1,1], con='../../Gists/Phases/Phases.md')
rm('../../Gists/Phases/.DS_Store')
#------------------------------------------------------------------

	
	
	