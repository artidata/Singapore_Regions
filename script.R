library(data.table)
library(sf)
library(ggplot2)
library(gganimate)
library(stringr)
library
dirr <- getwd()
subzone_SF <- read_sf(paste0(dirr,"/shp/MP14_SUBZONE_NO_SEA_PL.shp"))
subzone_SF_DT <- data.table(subzone_SF) 


#######################################
#post code conversion to planning area#
#######################################

foo_DT <- data.table(subzone_SF)
foo_DT <- foo_DT[,.(id =OBJECTID,SUBZONE_C,PLN_AREA_C,REGION_C)]

post_DT <- fread(paste0(dirr,"/locfile 20151126.csv"))
post_SF <- st_as_sf(post_DT,coords = c("x","y"),agr="constant",crs=4267)
post_SF <- st_transform(post_SF,st_crs(subzone_SF))
post_SF_DT <- data.table(post_SF)

within_LS <- st_within(post_SF_DT[,geometry],subzone_SF_DT[,geometry])
#handle non unique intersection
within_LS <- lapply(within_LS,function(x){ifelse(length(x)==1,x,NA)})

regionConversion_DT <- data.table(postcode=post_DT[,postcode], id = unlist(within_LS))
regionConversion_DT <- merge(regionConversion_DT,foo_DT,by="id",all.x=T)

fwrite(regionConversion_DT,"regionConversion_DT 20170715.csv")

##########################
#Aging population example#
##########################

library(stringr)
pop_DT <- fread(paste0(dirr,"/respopagsex2000to2016.csv"))
pop_DT[,PA:= toupper(PA),]
pop_DT[,start_AG := as.integer(str_extract(AG,"\\d*"))]
graph_DT <- pop_DT[start_AG>=65]
graph_DT <- graph_DT[,.(Pop = sum(Pop)),by=.(Time,PA)] 


plnAreaN_SF_DT <- subzone_SF_DT[,data.table(st_union(geometry)),by=PLN_AREA_N] # very important data.table
setnames(plnAreaN_SF_DT,"V1","geometry")
plnAreaN_SF_DT[,geometry:=st_cast(geometry),] #very important st_cast
st_write(st_transform(plnAreaN_SF_DT[,geometry],crs=4267),"sg plan area 20170719.geojson")

graph3_DT <- graph_DT[Time>2011]
graph3_DT <- merge(graph3_DT,plnAreaN_SF_DT,by.x="PA",by.y ="PLN_AREA_N",all.x=T)





ggplot()+
  geom_sf(data=graph3_DT,aes(geometry = geometry, fill=Pop)) + 
  facet_wrap(~Time)

gganimate(gif)


ggplot()+
  geom_sf(data = plnAreaC_SF_DT,aes(geometry=geometry))+
  theme_void()
          
ggplot()+
  geom_sf(data= subzone_SF_DT[PLN_AREA_C=="WI"],aes(geometry=geometry))
