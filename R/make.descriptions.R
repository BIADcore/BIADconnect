
#------------------------------------------------------------------
# Pull table summaries from the dtabase, and update to Gists
# Should be able to loop it
#------------------------------------------------------------------
# Sites
sql.command <- "SELECT table_comment FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='COREX' AND table_name='Sites';"
d <- sql.wrapper(sql.command,user,password)
writeLines(d[1,1], con='../../Gists/Sites/Sites.md')
#------------------------------------------------------------------
# Phases
sql.command <- "SELECT table_comment FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='COREX' AND table_name='Phases';"
d <- sql.wrapper(sql.command,user,password)
writeLines(d[1,1], con='../../Gists/Phases/Phases.md')
#------------------------------------------------------------------

	
	
	