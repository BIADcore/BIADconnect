#--------------------------------------------------------------------------------------------------------------
# Create MySQL triggers
# Needs recreating regulary, as table structures change over time.
#--------------------------------------------------------------------------------------------------------------
tables <- query.database(user, password, 'biad', "SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';")
all <- query.database(user, password, 'biad', "SELECT `TABLE_NAME`, `COLUMN_NAME` FROM `information_schema`.`COLUMNS` WHERE TABLE_SCHEMA='biad';")
trig <- query.database(user, password, 'biad', "SHOW TRIGGERS;")$Trigger
all <- subset(all, TABLE_NAME%in%tables$TABLE_NAME)

# autopad triggers to ensure all leading and trailing whitespace is removed for any insert or update.
prefix <- 'auto_pad_'
old.triggers <- trig[substr(trig, 1, nchar(prefix)) ==prefix]
drop.triggers <- paste('DROP TRIGGER `',old.triggers,'`;',sep='')
new.triggers <- make.all.triggers(all, prefix, trigger = make.autopad.trigger)
query.database(user, password, 'biad', drop.triggers)
query.database(user, password, 'biad', new.triggers)

# time/user stamp triggers
prefix <- 'trigger_stamp_'
old.triggers <- trig[substr(trig, 1, nchar(prefix)) ==prefix]
drop.triggers <- paste('DROP TRIGGER `',old.triggers,'`;',sep='')
sub <- subset(all, COLUMN_NAME=='time_added')
new.triggers <- make.all.triggers(sub, prefix, trigger = make.stamp.trigger)
query.database(user, password, 'biad', drop.triggers)
query.database(user, password, 'biad', new.triggers)
#--------------------------------------------------------------------------------------------------------------



