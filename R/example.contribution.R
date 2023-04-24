#--------------------------------------------------------------------------------------
# Determining the % of contribution towards data collection in BIAD
# Example: radiocarbon dates
#--------------------------------------------------------------------------------------
source('.Rprofile') # should already have loaded if you open R from this script.
source('functions.R')

# query the requested dataset
sql.command <- "SELECT *
FROM C14Samples
INNER JOIN `Sites` ON `C14Samples`.`SiteID` = `Sites`.`SiteID`
INNER JOIN `Phases` ON `C14Samples`.`PhaseID` = `Phases`.`PhaseID`
WHERE `Phases`.`Culture1` IS NOT NULL AND `Sites`.`Country` = 'Sweden' OR `Phases`.`Culture1` IS NOT NULL AND `Sites`.`Country` = 'Denmark'"
query <- sql.wrapper(sql.command,user,password,hostname,hostuser,keypath,ssh)

# generate a dataframe with the added and updated information
added <- data.frame(query$user_added, "added")
colnames(added)[1] = "author"
colnames(added)[2] = "type"
updated <- data.frame(query$user_last_update, "updated")
colnames(updated)[1] = "author"
colnames(updated)[2] = "type"
contribution <- rbind(added, updated)

# generate the proportions for each of the columns
prop.table(table(contribution$author)) # contribution by authors
prop.table(table(contribution)) # comparison of both values

# barplot code
counts <- table(contribution$type, contribution$author)
barplot(counts, col = c("darkblue","red"), legend = rownames(counts))