#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
library(data.tree)
#----------------------------------------------------------------------------------------------------
#query the selected table for the dependenancies of a SiteID or PhaseID
x <- run.server.searcher(table.name = 'Sites', primary.value = 'S03277') 

#use the queried information to prepare the list of relationships up and down the hierarchical tree
tree.down <- FromListSimple(x$down)
tree.up <- FromListSimple(x$up)

#print the results as a list
print(tree.down)
print(tree.up)

#show the results as a plot
plot(tree.down)
plot(tree.up)
#----------------------------------------------------------------------------------------------------
# query to access information displayed in the list or graph as 'data'
x$down$Phases$VEDRO1$C14Samples$data
#----------------------------------------------------------------------------------------------------
