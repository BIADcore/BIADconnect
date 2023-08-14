#---------------------------------------------------------------------------------------------
# Looks for suspicious stuff, which may not actually be a problem, 
# but its nice to have a look anyway, if you're a suspicious sort of person
#---------------------------------------------------------------------------------------------
# Different sites that might actually be the same
#---------------------------------------------------------------------------------------------
# Compares all pairs of sites. 
# 1. The closer they are, the greater the chance they are actually the same (consider lat and long independently)
# 2. The more similar the name, the greater the chance they are the same.
# 3. If there is no associated data, or the only associated data is radiocarbon, there is a greater chance of error
#---------------------------------------------------------------------------------------------
# work in progress!