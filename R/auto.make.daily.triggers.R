#--------------------------------------------------------------------------------------------------------------
# Create MySQL triggers
# Needs recreating regulary, as table structures change over time.
#--------------------------------------------------------------------------------------------------------------
conn  <- init.conn()
tables <- query.database(sql.command = "SELECT `TABLE_NAME` FROM `information_schema`.`TABLES` WHERE TABLE_SCHEMA='biad' AND `TABLE_TYPE`='BASE TABLE';",conn = conn)
all <- query.database(sql.command = "SELECT `TABLE_NAME`, `COLUMN_NAME` FROM `information_schema`.`COLUMNS` WHERE TABLE_SCHEMA='biad';",conn = conn)
trig <- query.database(sql.command =  "SHOW TRIGGERS;")$Trigger
all <- subset(all, TABLE_NAME%in%tables$TABLE_NAME)

# autopad triggers to ensure all leading and trailing whitespace is removed for any insert or update.
prefix <- 'auto_pad_'
old.triggers <- trig[substr(trig, 1, nchar(prefix)) ==prefix]
drop.triggers <- paste('DROP TRIGGER `',old.triggers,'`;',sep='')
new.triggers <- make.all.triggers(all, prefix, trigger = make.autopad.trigger)
if(length(old.triggers)>0)query.database(sql.command = drop.triggers, conn = conn)
query.database(sql.command = new.triggers, conn = conn)

# time/user stamp triggers
prefix <- 'trigger_stamp_'
old.triggers <- trig[substr(trig, 1, nchar(prefix)) ==prefix]
drop.triggers <- paste('DROP TRIGGER `',old.triggers,'`;',sep='')
sub <- subset(all, COLUMN_NAME=='time_added')
new.triggers <- make.all.triggers(sub, prefix, trigger = make.stamp.trigger)
if(length(old.triggers)>0)query.database(sql.command = drop.triggers,conn=conn)
query.database(sql.command = new.triggers,conn=conn)
disconnect()
#--------------------------------------------------------------------------------------------------------------



