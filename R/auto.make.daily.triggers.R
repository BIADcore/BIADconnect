#--------------------------------------------------------------------------------------------------------------
# Create MySQL triggers to ensure all leading and trailing whitespace is removed for any insert or update.
# Needs recreating regulary, as table structures change over time.
#--------------------------------------------------------------------------------------------------------------
tables <- query.database(user, password, "SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';")
all <- query.database(user, password, "SELECT `TABLE_NAME`, `COLUMN_NAME` FROM `information_schema`.`COLUMNS` WHERE TABLE_SCHEMA='biad';")
trig <- query.database(user, password, "SHOW TRIGGERS;")$Trigger

all <- subset(all, TABLE_NAME%in%tables$TABLE_NAME)
old.triggers <- trig[substr(trig, 1, 20) =='auto_trigger_padding']
drop.triggers <- paste('DROP TRIGGER `',old.triggers,'`;',sep='')
all.triggers <- make.all.triggers(all)

query.database(user, password, drop.triggers)
query.database(user, password, all.triggers)
#--------------------------------------------------------------------------------------------------------------