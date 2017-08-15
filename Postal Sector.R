
foo_SF_DT <- subzone_SF_DT[year==2014]
singapore_SF_DT <- data.table(geometry = foo_SF_DT[,st_union(geometry),])

# Eyeballing Singapore Map
ggplot()+
  geom_sf(data=singapore_SF_DT,aes(geometry=geometry))


#Postcode data
locfile_DT <- fread(paste0(dirr,"/locfile 20151126.csv"))
postcode_SF <- st_as_sf(locfile_DT,coords = c("x","y"),agr="constant",crs=4267)
postcode_SF <- st_transform(postcode_SF,subzone_SF_DT[,st_crs(geometry),])
postcode_SF_DT <- data.table(postcode_SF)


# Eyeballing Singapore Map and postcode
ggplot()+
  geom_sf(data=singapore_SF_DT,aes(geometry=geometry))+
  geom_sf(data=postcode_SF_DT[1:100],aes(geometry=geometry))

within_LS <- st_within(postcode_SF_DT[,geometry],subzone_SF_DT[year==2014,geometry])
within_singapore <- unlist(lapply(within_LS,function(x){length(x)>0}))
postcode_SF_DT <- cbind(postcode_SF_DT,within_singapore)
postcode_SF_DT<- cbind(postcode_SF_DT,postcode_SF_DT[,st_coordinates(geometry),])
setnames(postcode_SF_DT,c("X","Y"),c("lon","lat"))

#eyballing whether the result of within_LS are sensible
ggplot()+
  geom_sf(data=singapore_SF_DT,aes(geometry=geometry))+
  geom_point(data=postcode_SF_DT,aes(x=lon,y=lat,color=within_singapore),size=0.5)+
  theme_void()

#getting postal sector
postcode_SF_DT[,postcode:= as.character(postcode),]
postcode_SF_DT[,postcode:= ifelse(str_length(postcode)==5, str_c(0,postcode),postcode)]
postcode_SF_DT[,postal_sector:= str_sub(postcode,1,2)]

postcode_SF_DT[,length(unique(postal_sector))]
# 80
## supposed to be 82

#missing postal sector
all_postal_sector <- 0:82
all_postal_sector <- as.character(all_postal_sector)
all_postal_sector <- ifelse(str_length(all_postal_sector)==1,str_c(0,all_postal_sector),all_postal_sector)

all_postal_sector[!all_postal_sector %in% postcode_SF_DT[,unique(postal_sector)]]
#[1] "00" "02" "74"
all_postal_sector[!all_postal_sector %in% postcode_SF_DT[within_singapore==T,unique(postal_sector)]]


#
foo <- st_voronoi(postcode_SF_DT[postal_sector=="05"&within_singapore==T,geometry],
                  singapore_SF_DT[,st_sfc(geometry)])

# data is large, we need to sample
set.seed(240193)
rand <- sample(1:nrow(postcode_SF_DT),20000,replace = F)
sample_SF_DT <- postcode_SF_DT[rand]

foo<-st_combine(sample_SF_DT[within_singapore==T,geometry])
voronoi_SF <- st_voronoi(foo)
voronoi_SF_DT <- data.table(geometry = st_cast(voronoi_SF))
#ggplot()+
#  geom_sf(data = voronoi_SF_DT,aes(geometry=geometry))

# a bit long
voronoi_clipped_SF_DT <- data.table( 
  geometry = st_cast(st_intersection(voronoi_SF_DT[,geometry],singapore_SF_DT[,geometry])))

#ggplot()+
#  geom_sf(data = voronoi_clipped_SF_DT,aes(geometry=geometry))

within_sample_SF_DT <- sample_SF_DT[within_singapore==T,]
within2_LS <- st_within(within_sample_SF_DT[,geometry],voronoi_clipped_SF_DT[,geometry])
summary(unlist(lapply(within2_LS,function(x){length(x)==1})))
#Mode    TRUE 
#logical    19969 
within_sample_SF_DT <- cbind(within_sample_SF_DT,id = unlist(within2_LS))
voronoi_clipped_SF_DT[,id:= 1:nrow(voronoi_clipped_SF_DT)]

postal_sector_SF_DT <- merge(within_sample_SF_DT,voronoi_clipped_SF_DT,by="id")
postal_sector_SF_DT <- postal_sector_SF_DT[,.(geometry= st_union(geometry.y)),by=postal_sector]
postal_sector_SF_DT[,geometry:=st_cast(geometry),]
postal_sector_SF_DT[,center:= st_centroid(geometry)]
postal_sector_SF_DT <- cbind(postal_sector_SF_DT,postal_sector_SF_DT[,st_coordinates(center)])
setnames(postal_sector_SF_DT,c("X","Y"),c("lon","lat"))

ggplot()+
  geom_sf(data=postal_sector_SF_DT,aes(geometry=geometry))+
  geom_text(data=postal_sector_SF_DT,aes(x=lon,y=lat,label=postal_sector))+
  theme_void()


district_SF_DT <- postcode_SF_DT[,.(postal_sector,district)]
district_SF_DT <- district_SF_DT[,.(district=unique(district)),by=postal_sector][order(postal_sector)]
#manual_cleaning
district_SF_DT[postal_sector=="07",district:=2]
district_SF_DT[postal_sector=="49",district:=17]
district_SF_DT[postal_sector=="50",district:=17]
district_SF_DT[postal_sector=="81",district:=17]
district_SF_DT <- district_SF_DT[,.(district=unique(district)),by=postal_sector][order(postal_sector)]

district_SF_DT <- merge(postal_sector_SF_DT[,.(postal_sector,geometry)],
                        district_SF_DT,by="postal_sector")
district_SF_DT <- district_SF_DT[,.(geometry= st_union(geometry)),by=district]
district_SF_DT[,geometry:=st_cast(geometry),]
district_SF_DT[,center:= st_centroid(geometry)]
district_SF_DT <- cbind(district_SF_DT,district_SF_DT[,st_coordinates(center)])
setnames(district_SF_DT,c("X","Y"),c("lon","lat"))

ggplot()+
  geom_sf(data=district_SF_DT,aes(geometry=geometry))+
  geom_text(data=district_SF_DT,aes(x=lon,y=lat,label=as.character(district)))+
  theme_void()
