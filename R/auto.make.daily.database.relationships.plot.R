#----------------------------------------------------------------------------------
# script to plot database relationships
#----------------------------------------------------------------------------------
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
#----------------------------------------------------------------------------------
# Pull all foreign keys
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD';"	
d.tables <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)$TABLE_NAME	

zprivate <- d.tables[grepl('zprivate', d.tables)]
zoptions <- d.tables[grepl('zoptions', d.tables)]
copy <- d.tables[grepl('copy', d.tables)]
standard <- d.tables[!d.tables%in%c(zoptions,zprivate,copy)]
#------------------------------------------------------------------
# all relationships
d.tables <- paste(standard, collapse='; ')
image <- database.relationship.plotter(d.tables, TRUE)
export_svg(image) %>% charToRaw %>% rsvg_png("../tools/plots/database.relationships.plot.png")
#------------------------------------------------------------------
# set 1
d.tables <- paste(c('Sites','Phases','C14Samples','Graves','FaunalIsotopes','ABotPhases','StrontiumEnvironment'), collapse='; ')
image <- database.relationship.plotter(d.tables, FALSE)
export_svg(image) %>% charToRaw %>% rsvg_png("../tools/plots/database.relationships.plot.set.1.png", height=400)
#------------------------------------------------------------------
