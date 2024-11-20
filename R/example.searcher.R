#--------------------------------------------------------------------------------------
# Requirements
# You must have previously added the .Rprofile to your R_USER folder, here -> path.expand('~/') 
# See the BIADwiki readme or BIADwiki for details about using the .Rprofile

# Load some required functions
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
library(data.tree)
#----------------------------------------------------------------------------------------------------
# query any table with a primary key value, to get all direct relationships 
x <- run.server.searcher(table.name = 'GraveIndividuals', primary.value = 'C03440') 
x <- run.server.searcher(table.name = 'Sites', primary.value = 'S10050') 
x <- run.searcher(table.name = 'Sites', primary.value = 'S10050') 
# look at the data
x

# look at direct relationships (tree) above or below
tree.down <- FromListSimple(x$down)
tree.up <- FromListSimple(x$up)

# print the results as a list
print(tree.down)
print(tree.up)

# show the results as a plot
plot(tree.down)
plot(tree.up)
#----------------------------------------------------------------------------------------------------
open.tunnel()
query <- query.database("SELECT * FROM `Sites`")
dim(query)
close.tunnel()
#----------------------------------------------------------------------------------------------------

