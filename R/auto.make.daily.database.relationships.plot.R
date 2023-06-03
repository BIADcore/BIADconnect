#----------------------------------------------------------------------------------
# script to plot database relationships
#----------------------------------------------------------------------------------
library(rsvg)
library(DiagrammeRsvg)
#----------------------------------------------------------------------------------
# Pull all foreign keys
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD';"	
d.tables <- query.database(user, password, sql.command)$TABLE_NAME	

zprivate <- d.tables[grepl('zprivate', d.tables)]
zoptions <- d.tables[grepl('zoptions', d.tables)]
copy <- d.tables[grepl('copy', d.tables)]
standard <- d.tables[!d.tables%in%c(zoptions,zprivate,copy)]
#------------------------------------------------------------------
print('here 1')
# all relationships
d.tables <- paste(standard, collapse='; ')
image <- database.relationship.plotter(d.tables, TRUE, user, password)
export_svg(image) %>% charToRaw %>% rsvg_png("../tools/plots/database.relationships.plot.png")
#------------------------------------------------------------------
# set 1
d.tables <- paste(c('Sites','Phases','C14Samples','Graves','FaunalIsotopes','ABotPhases','StrontiumEnvironment'), collapse='; ')
image <- database.relationship.plotter(d.tables, FALSE, user, password)
export_svg(image) %>% charToRaw %>% rsvg_png("../tools/plots/database.relationships.plot.sub.1.png", height=350)
#------------------------------------------------------------------
