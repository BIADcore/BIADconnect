#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# # query any table with a primary key value, to get all direct relationships 
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Requirements first read:
# https://biadwiki.org/en/connectR
#--------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/BIADwiki/BIADwiki/main/R/functions.R")
#--------------------------------------------------------------------------------------
x <- run.searcher(table.name = 'Sites', primary.value = 'S10050') 

x # look at the data

# look at direct relationships (tree) above or below
library(data.tree)
tree.down <- FromListSimple(x$down)
tree.up <- FromListSimple(x$up)

# print the results as a list
print(tree.down)
print(tree.up)

# show the results as a plot
plot(tree.down)
plot(tree.up)

#--------------------------------------------------------------------------------------
# more advance testing
#--------------------------------------------------------------------------------------
open.tunnel()
x <- {a=Sys.time();res <- get.relatives(table.name = 'Sites', primary.value = 'S10050', directions = "down") ; print(Sys.time()-a); res}
x <- {a=Sys.time();res <- run.searcher(table.name = 'Sites', primary.value = 'S10050', direction = "down") ; print(Sys.time()-a); res}
close.tunnel()
#--------------------------------------------------------------------------------------