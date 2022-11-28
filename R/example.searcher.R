#----------------------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
library(ssh)
library(data.tree)
#----------------------------------------------------------------------------------------------------
x <- run.server.searcher(table.name = 'Phases', primary.value = 'TEG31')
x <- run.server.searcher(table.name = 'Sites', primary.value = 'S01239')
x <- run.server.searcher(table.name = 'Sites', primary.value = 'S09209')

tree.down <- FromListSimple(x$down)
tree.up <- FromListSimple(x$up)

print(tree.down)
print(tree.up)

plot(tree.down)
plot(tree.up)
#----------------------------------------------------------------------------------------------------
x$down$Phases$TEG31$C14Samples$data
#----------------------------------------------------------------------------------------------------
