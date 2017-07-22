library(reshape2)

#1
void <- c("00","02","74")
graph_1_DT <- fread(str_c(dirr,"/data/1.csv"))
graph_1_DT[,postal_sector:=as.character(postalsector)]
graph_1_DT[,postal_sector:=ifelse(str_length(postal_sector)==1, str_c("0",postal_sector),postal_sector)]
graph_1_DT <- graph_1_DT[!postal_sector%in%void]
graph_1_DT <- merge(graph_1_DT,postal_sector_SF_DT[,.(postal_sector,geometry)],by="postal_sector")
graph_1_DT <- melt(graph_1_DT, id.vars = c("postal_sector","geometry"),
     measure.vars = c("3am","6am","9am","12pm","3pm","6pm","9pm","12am"),
     variable.name = "hour_of_day",
     value.name = "diff")
ggplot()+
  geom_sf(data=graph_1_DT,aes(geometry = geometry, fill=diff),lty=0) + 
  theme_void()+
  facet_wrap(~hour_of_day,ncol=4)+
  theme(title = element_blank(),
        text = element_text(color = "#7f0000",size = 15),
        plot.background = element_rect(fill="#ffffbf"),
        panel.grid.major = element_line(color="#ffffbf"),
        panel.grid.minor = element_line(color="#ffffbf"),
        legend.position = "right")+
  scale_fill_distiller(type = "div",palette ="RdBu",direction = 1,limits= c(-70,70))

#2
void <- c("00","02","74")
graph_2_DT <- fread(str_c(dirr,"/data/2.csv"))
graph_2_DT[,postal_sector:=as.character(postalsector)]
graph_2_DT[,postal_sector:=ifelse(str_length(postal_sector)==1, str_c("0",postal_sector),postal_sector)]
graph_2_DT <- graph_2_DT[!postal_sector%in%void]
graph_2_DT <- merge(graph_2_DT,postal_sector_SF_DT[,.(postal_sector,geometry)],by="postal_sector")
graph_2_DT <- melt(graph_2_DT, id.vars = c("postal_sector","geometry"),
                   measure.vars = c("3am","6am","9am","12pm","3pm","6pm","9pm","12am"),
                   variable.name = "hour_of_day",
                   value.name = "diff")
ggplot()+
  geom_sf(data=graph_2_DT,aes(geometry = geometry, fill=diff),lty=0) + 
  theme_void()+
  facet_wrap(~hour_of_day,ncol=4)+
  theme(title = element_blank(),
        text = element_text(color = "#7f0000",size = 15),
        plot.background = element_rect(fill="#deebf7"),
        panel.grid.major = element_line(color="#deebf7"),
        panel.grid.minor = element_line(color="#deebf7"),
        legend.position = "right")+
  scale_fill_distiller(type = "div",palette ="RdYlGn",direction = 1,limits= c(-30,30))


#3
void <- c("00","02","74")
graph_3_DT <- fread(str_c(dirr,"/data/3.csv"))
graph_3_DT[,postal_sector:=as.character(postalsector)]
graph_3_DT[,postal_sector:=ifelse(str_length(postal_sector)==1, str_c("0",postal_sector),postal_sector)]
graph_3_DT <- graph_3_DT[!postal_sector%in%void]
graph_3_DT <- merge(graph_3_DT,postal_sector_SF_DT[,.(postal_sector,geometry)],by="postal_sector")
graph_3_DT <- melt(graph_3_DT, id.vars = c("postal_sector","geometry"),
                   measure.vars = c("3am","6am","9am","12pm","3pm","6pm","9pm","12am"),
                   variable.name = "hour_of_day",
                   value.name = "total")
ggplot()+
  geom_sf(data=graph_3_DT,aes(geometry = geometry, fill=diff),lty=0) + 
  theme_void()+
  facet_wrap(~hour_of_day,ncol=4)+
  theme(title = element_blank(),
        text = element_text(color = "#7f0000",size = 15),
        plot.background = element_rect(fill="#deebf7"),
        panel.grid.major = element_line(color="#deebf7"),
        panel.grid.minor = element_line(color="#deebf7"),
        legend.position = "right")+
  scale_fill_distiller(type = "div",palette ="RdYlGn",direction = 1,limits= c(-30,30))






max(graph_2_DT[,abs(diff)])

muted("red")
?scale_fill_distiller

