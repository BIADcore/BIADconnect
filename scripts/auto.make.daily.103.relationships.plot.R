#----------------------------------------------------------------------------------
# script to plot database relationships
#----------------------------------------------------------------------------------
library(rsvg)
library(DiagrammeRsvg)
#----------------------------------------------------------------------------------
# Pull all foreign keys
conn <- init.conn()
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD';"	
d.tables <- query.database(sql.command = sql.command, conn=conn)$TABLE_NAME

zprivate <- d.tables[grepl('zprivate', d.tables)]
zoptions <- d.tables[grepl('zoptions', d.tables)]
copy <- d.tables[grepl('copy', d.tables)]
standard <- d.tables[!d.tables%in%c(zoptions,zprivate,copy)]
#------------------------------------------------------------------
# all relationships
d.tables <- paste(standard, collapse='; ')
image <- database.relationship.plotter(d.tables, include.look.ups=TRUE, conn=conn)
svg <- export_svg(image)
writeLines(svg, '../tools/plots/database.relationships.plot.svg')
#------------------------------------------------------------------
# set 1
d.tables <- paste(c('Sites','Phases','C14Samples','Graves','FaunalIsotopes','ABotPhases','StrontiumEnvironment'), collapse='; ')
image <- database.relationship.plotter(d.tables, include.look.ups=FALSE, conn = conn)
svg <- export_svg(image)
writeLines(svg, '../tools/plots/database.relationships.plot.sub.1.svg')
#--------------------------------------------------------------------------------------
disconnect()
#--------------------------------------------------------------------------------------

