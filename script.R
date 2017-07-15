library(rgdal)
library(sp)
dirr <- getwd()
subzone_SPDF <- readOGR(dsn = paste0(dirr,"/data"),layer="MP14_SUBZONE_NO_SEA_PL")
