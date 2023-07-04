#------------------------------------------------------------------
# Example R script for converting local coordinates to WGS 84
#------------------------------------------------------------------
# First obtain a set of points with coordinates.
# Put them in a .csv file, in the same folder that this script is in.
#------------------------------------------------------------------
library(sf)
# read the .csv file into R
___ <- read.csv("___.csv") 
head(___) # inspect the file

# convert the data.frame to an sf object
# pay attention to the re-arranged coordinate columns
# input the crs value for the original porjection
___ <- st_as_sf(___, coords = c("E", "N"), crs = ___)  

# inspect the file
head(___) 

# transform the sf object from the local system to WGS 84
___ <- st_transform(___, crs = 4326)

# inspect the file
head(___) 

#save the file as .csv ready for import
st_write(___, "___.csv", layer_options = "GEOMETRY=AS_YX", delete_dsn = TRUE)