#----------------------------------------------------------------------------------
# script to plot database relationships
#----------------------------------------------------------------------------------
# Pull all foreign keys
sql.command <- "SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='BIAD';"	
d.tables <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)$TABLE_NAME	

zprivate <- d.tables[grepl('zprivate', d.tables)]
zoptions <- d.tables[grepl('zoptions', d.tables)]
standard <- d.tables[!d.tables%in%c(zoptions,zprivate)]

sql.command <- "SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE REFERENCED_TABLE_SCHEMA = 'BIAD'"
d <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)
#------------------------------------------------------------------
# get data tables
tables <- paste(standard, collapse='; ')

data.tables <- paste("
  node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 2.2,
  fontsize = 15]
"
, tables, sep='\n ')
#------------------------------------------------------------------
# get lookup tables
tables <- zoptions

look.ups <- paste("
  node [shape = box
  style = filled,
  fillcolor = lightblue,
  fixedsize = true,
  width = 3.0
  fontsize = 15]
"
, tables, sep='\n ')
#------------------------------------------------------------------
# convert foreign keys into a suitable format for DiagrammeR
edges <- paste(d$REFERENCED_TABLE_NAME, d$TABLE_NAME, sep=' -> ')
edges <- paste('edge [color = dimgray]', edges, collapse='\n ')
#------------------------------------------------------------------
subgraph <- "
subgraph cluster {
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 1,
  fontsize = 10]
  DataTable
  node [shape = box
  style = filled,
  fillcolor = lightblue
  fixedsize = true,
  width = 1
  fontsize = 10]
  LookUpTable
}
"
#------------------------------------------------------------------
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
diagram <- paste("digraph {", data.tables, look.ups, edges, subgraph, "}")
image <- DiagrammeR::grViz(diagram)

export_svg(image) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.relationships.plot.png")
#------------------------------------------------------------------
