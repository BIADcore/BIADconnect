#----------------------------------------------------------------------------------------------------
source('.Rprofile')
source('functions.R')
library(data.tree)
#----------------------------------------------------------------------------------------------------

table.name <- 'Phases'
primary.value <-  'TEG31eee'

table.name <- 'Sites'
primary.value <-  'S09209'

table.name <- 'Phases'
primary.value <-  'NITR2'

table.name <- 'Phases'
primary.value <-  'SCHEV4'

table.name <- 'Sites'
primary.value <- 'S10386'

table.name <- 'Sites'
primary.value <- 'S10191'

x <- get.related.data(table.name, primary.value, fnc = decendants)
x <- get.related.data(table.name, primary.value, fnc = ancestors)


tree <- FromListSimple(x)
plot(tree)
print(tree)
#----------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------



