
#----------------------------------------------------------------------------------------------------------------------------------------
# plotly is possible: https://datatricks.co.uk/3d-world-map-with-plotly
# but leaflet may be preferable
#----------------------------------------------------------------------------------------------------------------------------------------
# library(leaflet)
library(htmlwidgets)
source('functions.R')
user <- 'Rscripts'
password <- 'delightfuldata'
server <- 'macelab-server.biochem.ucl.ac.uk'
port <- 3306
#----------------------------------------------------------------------------------------------------------------------------------------
sql.command <- "SELECT * FROM COREX.Sites"
d <- sql.wrapper(sql.command,user,password,server,port)

rand_lng = function(n = 10) rnorm(n, -93.65, .01)
rand_lat = function(n = 10) rnorm(n, 42.0285, .01)
m = leaflet() %>% addTiles() %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 10, 200))
m

saveWidget(m, file="m.html")
m <- leaflet()
m <- m %>% addTiles()
m <- m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 10, 200))