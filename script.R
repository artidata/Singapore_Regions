library(data.table)
library(sf)
library(ggplot2)
dirr <- getwd()
subzone_SF <- st_read(paste0(dirr,"/shp/MP14_SUBZONE_NO_SEA_PL.shp"))
foo_DT <- data.table(subzone_SF)
foo_DT <- foo_DT[,.(id =OBJECTID,SUBZONE_C,PLN_AREA_C,REGION_C)]
post_DT <-fread(paste0(dirr,"/locfile 20151126.csv"))
post_SF <- st_as_sf(post_DT[,.(postcode,x,y)],coords = c("x","y"),agr="constant",crs=4267)
post_SF <- st_transform(post_SF,st_crs(subzone_SF))

within_LS <- st_within(post_SF,subzone_SF)
#handle non unique intersection
within_LS <- lapply(within_LS,function(x){ifelse(length(x)==1,x,NA)})

regionConversion_DT <- data.table(postcode=post_DT[,postcode], id = unlist(within_LS))
regionConversion_DT <- merge(regionConversion_DT,foo_DT,by="id",all.x=T)

fwrite(regionConversion_DT,"regionConversion_DT 20170715.csv")