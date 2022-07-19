#------------------------------------------------------------------
# Example R script for converting local Swedish coordinate system to WGS 84
#------------------------------------------------------------------
# First obtain a set of points with coordinates.
# Put them in a .csv file, in the same folder that this script is in.
#------------------------------------------------------------------
library(sf)
___ <- read.csv("___.csv") #read the .csv file into R
head(___) # inspect the file
___ <- st_as_sf(___, coords = c("E", "N"), crs = 3006)  #convert the data.frame to an sf object, pay attention to the re-arranged coordinate columns
head(___) #inspect the file
___ <- st_transform(___, crs = 4326) #transform the sf object from the local system to WGS 84
head(___) #inspect the file
st_write(___, "___.csv", layer_options = "GEOMETRY=AS_YX", delete_dsn = TRUE) #save the file as .csv ready for import