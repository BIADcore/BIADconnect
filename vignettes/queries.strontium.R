#--------------------------------------------------------------------------------------
conn  <-  init.conn()
strcol <- query.database("DESCRIBE Strontium", conn=conn)
sql.command <- paste0("SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Sites`.`Latitude`, `Sites`.`Longitude`", paste0(paste0("`Strontium`.`",strcol$Field,"`"),collapse=","),"
FROM `Sites`
LEFT JOIN `Phases` ON `Sites`.`SiteID` = `Phases`.`SiteID`
LEFT JOIN `Graves` ON `Phases`.`PhaseID` = `Graves`.`PhaseID`
LEFT JOIN `GraveIndividuals` ON `Graves`.`GraveID` = `GraveIndividuals`.`GraveID`
LEFT JOIN `Strontium` ON `GraveIndividuals`.`IndividualID` = `Strontium`.`IndividualID`")
query <- query.database(sql.command, conn=conn)

strcol <- query.database("DESCRIBE HumanIsotopes", conn=conn)
sql.command <- paste0("SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Sites`.`Latitude`, `Sites`.`Longitude`,", paste0(paste0("`HumanIsotopes`.`",strcol$Field,"`"),collapse=","),"
FROM `Sites`
LEFT JOIN `Phases` ON `Sites`.`SiteID` = `Phases`.`SiteID`
LEFT JOIN `Graves` ON `Phases`.`PhaseID` = `Graves`.`PhaseID`
LEFT JOIN `GraveIndividuals` ON `Graves`.`GraveID` = `GraveIndividuals`.`GraveID`
LEFT JOIN `HumanIsotopes` ON `GraveIndividuals`.`IndividualID` = `HumanIsotopes`.`IndividualID`")
query <- query.database(sql.command, conn=conn)

strcol <- query.database("DESCRIBE StrontiumEnvironment", conn=conn)
sql.command <- paste0("SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Sites`.`Latitude`, `Sites`.`Longitude`,", paste0(paste0("`StrontiumEnvironment`.`",strcol$Field,"`"),collapse=","),"
FROM `StrontiumEnvironment`
LEFT JOIN `Sites` ON `StrontiumEnvironment`.`SiteID` = `Sites`.`SiteID` ")
query <- query.database(sql.command, conn=conn)

strcol <- query.database("DESCRIBE C14Samples", conn=conn)
sql.command <- paste0("SELECT `Sites`.`SiteName`, `Sites`.`SiteID`, `Sites`.`Latitude`, `Sites`.`Longitude`,", paste0(paste0("`C14Samples`.`",strcol$Field,"`"),collapse=","),"
FROM `Sites`
LEFT JOIN `C14Samples` ON `Sites`.`SiteID` = `C14Samples`.`SiteID` ")
query <- query.database(sql.command, conn=conn)

