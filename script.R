library(data.table)
library(sf)
library(ggplot2)
library(stringr)
library(rprojroot)
###############
#Cleaning Data#
###############

root <- find_root(is_rstudio_project)

subzone_1998_SF <- read_sf(str_c(root,"/shp/1998/MP98_SUBZONE_NO_SEA_PL.shp"))
subzone_1998_SF$year <- 1998
subzone_2008_SF <- read_sf(str_c(root,"/shp/2008/MP08_SUBZONE_NO_SEA_PL.shp"))
subzone_2008_SF$year <- 2008
subzone_2014_SF <- read_sf(str_c(root,"/shp/2014/MP14_SUBZONE_NO_SEA_PL.shp"))
subzone_2014_SF$year <- 2014

# Proof that the projection is the same
identical(st_crs(subzone_2008_SF),st_crs(subzone_1998_SF))
identical(st_crs(subzone_2008_SF),st_crs(subzone_2014_SF))
all_crs <- st_crs(subzone_1998_SF)


subzone_1998_SF_DT <- data.table(subzone_1998_SF)
subzone_2008_SF_DT <- data.table(subzone_2008_SF)
#some error discovery, and making it more consistent
subzone_2008_SF_DT[PLN_AREA_N=="NORTH EASTERN ISLANDS", ":="(PLN_AREA_N="NORTH-EASTERN ISLANDS",
                                                             SUBZONE_N = "NORTH-EASTERN ISLANDS")]
subzone_2008_SF_DT[SUBZONE_N=="SZ1",SUBZONE_N:= "SUBZONE 1"]
subzone_2008_SF_DT[SUBZONE_N=="SZ2",SUBZONE_N:= "SUBZONE 2"]
subzone_2008_SF_DT[SUBZONE_N=="SZ3",SUBZONE_N:= "SUBZONE 3"]
subzone_2008_SF_DT[SUBZONE_N=="SZ4",SUBZONE_N:= "SUBZONE 4"]
subzone_2008_SF_DT[SUBZONE_N=="SZ5",SUBZONE_N:= "SUBZONE 5"]
subzone_2008_SF_DT[SUBZONE_N=="SZ6",SUBZONE_N:= "SUBZONE 6"]

subzone_2014_SF_DT <- data.table(subzone_2014_SF)

# Cleaning 1998 data
## adding Planning Area Name for 1998 data
foo_SF <- read_sf(paste0(root,"/shp/1998 Planning Area/MP98_PLNG_AREA_NO_SEA_PL.shp"))
foo_DT <- data.table(PLN_AREA_C = foo_SF$PLN_AREA_C,
                     PLN_AREA_N = foo_SF$PLN_AREA_N)
subzone_1998_SF_DT <- merge(subzone_1998_SF_DT,foo_DT,by="PLN_AREA_C",all.x=T)

## creating the SUBZONE_C
subzone_1998_SF_DT[,SUBZONE_C:= str_c(SUBZONE_NO),]
subzone_1998_SF_DT[,SUBZONE_C:= ifelse(str_length(SUBZONE_C)==2,SUBZONE_C,str_c("0",SUBZONE_C)),]
subzone_1998_SF_DT[,SUBZONE_C:= str_c(PLN_AREA_C,"SZ",SUBZONE_C),]

## estimate SUBZONE_N based on 2008 data
foo_DT <- subzone_2008_SF_DT[,.(SUBZONE_C,SUBZONE_N),]
subzone_1998_SF_DT <- merge(subzone_1998_SF_DT,foo_DT,by="SUBZONE_C",all.x=T)

subzone_SF_DT <- rbind(
  subzone_1998_SF_DT[,.(id = OBJECTID, year, SUBZONE_NO,SUBZONE_C,SUBZONE_N,PLN_AREA_C,PLN_AREA_N,geometry)],
  subzone_2008_SF_DT[,.(id = OBJECTID, year, SUBZONE_NO,SUBZONE_C,SUBZONE_N,PLN_AREA_C,PLN_AREA_N,geometry)],
  subzone_2014_SF_DT[,.(id = OBJECTID, year, SUBZONE_NO,SUBZONE_C,SUBZONE_N,PLN_AREA_C,PLN_AREA_N,geometry)]
) 
# convert to usual Long and data
#subzone_SF_DT[,geometry:=st_transform(geometry,crs=4267)]
setkey(subzone_SF_DT,"year","PLN_AREA_C","SUBZONE_NO")

#######################################
#post code conversion to planning area#
#######################################

# Here we use the latest master plan
foo_SF_DT <- subzone_SF_DT[year==2014,]

post_DT <- fread(str_c(root,"/raw_data/locfile 20151126.csv"))
post_SF <- st_as_sf(post_DT,coords = c("x","y"),agr="constant",crs=4267)
post_SF <- st_transform(post_SF,all_crs)
post_SF_DT <- data.table(post_SF)

within_LS <- st_within(post_SF_DT[,geometry],foo_SF_DT[,geometry])
#handle non unique intersection
within_LS <- lapply(within_LS,function(x){ifelse(length(x)==1,x,NA)})

regionConversion_DT <- data.table(postcode=post_DT[,postcode], id = unlist(within_LS))
regionConversion_DT <- merge(regionConversion_DT,foo_SF_DT,by="id",all.x=T)
regionConversion_DT <- regionConversion_DT[,.(postcode,SUBZONE_C,PLN_AREA_C)]
fwrite(regionConversion_DT,str_c(root,"/processed_data/regionConversion_DT 20170816.csv"))

##########################
#Aging population example#
##########################
subzone_SF_DT[,geometry:=st_transform(geometry,crs=4267)]
pop_DT <- fread(str_c(root,"/respopagsex2000to2016.csv"))
pop_DT[,":="(PA = toupper(PA),
             SZ = toupper(SZ)),]
pop_DT[,start_AG := as.integer(str_extract(AG,"\\d*"))]
pop_DT[,Time2:=ifelse(Time < 2001,1998,
                      ifelse(Time<2011,2008,2014)) ]
# manual editing based on 65 and above result
##different 
pop_DT[SZ=="NATIONAL UNIVERSITY OF SINGAPORE",SZ:="NATIONAL UNIVERSITY OF S'PORE"]

graph_DT <- pop_DT[start_AG>=65]
graph_DT <- graph_DT[,.( Pop=sum(Pop)),
                     by=.(Time,SUBZONE_N = SZ, PLN_AREA_N = PA,year=Time2)]
graph_DT <- merge(graph_DT,subzone_SF_DT,
                  by=c("SUBZONE_N","PLN_AREA_N","year"),all.x=T)
#too many mismatch on 1998 map and 2000 population data
graph_DT <- graph_DT[year!=1998]
row.has.na <- apply(graph_DT, 1, function(x){any(is.na(x))})

graph2_DT <- graph_DT[,.(geometry=(st_union(geometry)),
                         Pop = sum(Pop)),by=.(PLN_AREA_N,PLN_AREA_C,year,Time)]
graph2_DT[,geometry:=st_cast(geometry)]


