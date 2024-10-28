# EVALUATION DES DONNEES
rm(list=ls())
#LIBRARY
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(ncdf4)
library(DBI)
library(RSQLite)
require(ggplot2)
require(reshape2)
library(RColorBrewer)
library(hydroTSM)
library(hydroGOF)
library(plotrix)
# CONNECTION DE LA BASE DE DONNEES
setwd("D:/PROJET/Article/Merging")
CLIMATE_DB <- dbConnect(drv = RSQLite::SQLite(),"D:/PROJET/Article/Merging/Data/Station_Pluvio/Rain_DataBase.sqlite")

# TRAITEMENT DES DONNEES PLUVIOMETRIQUES

## Observation
var_names <- "CDT_SYNOP_RAIN_OBS"

SYNOP_OBS <- dbReadTable(conn =CLIMATE_DB,name = var_names)[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="OBS")%>%
  select(c(DATE,LABELS,colnames(.)))

SYNOP_STATION <- t(SYNOP_OBS)[-1,]%>%as.data.frame%>%
  transform(Station=rownames(.))
colnames(SYNOP_STATION) <- c("Lon","Lat","Names")
SYNOP_STATION

FRG_SYNOP_OBS <- SYNOP_OBS%>%
  melt(id.vars=c("DATE","LABELS"))
### BRUTE
SYNOP_RAW_CHIRPS <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_RAIN_BRUTE")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="BRUTE")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_SYNOP_BRUTE <- SYNOP_RAW_CHIRPS%>%
  melt(id.vars=c("DATE","LABELS"))

## AJUSTEE
SYNOP_ADJ_CHIRPS <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_RAIN_ADJUST")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="ADJUST")%>%
  select(c(DATE,LABELS,colnames(.)[-c(1,12)]))

FRG_SYNOP_ADJUST <- SYNOP_ADJ_CHIRPS%>%
  melt(id.vars=c("DATE","LABELS"))
## MERGEE

SYNOP_MRG_CHIRPS <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_RAIN_MRG2")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="MERGE")%>%
  select(c(DATE,LABELS,colnames(.)[order(colnames(.))]))

FRG_SYNOP_MRG <- SYNOP_MRG_CHIRPS%>%
  melt(id.vars=c("DATE","LABELS"))
## RBIND
FRG_SYNOP_DATA <- rbind(FRG_SYNOP_OBS,FRG_SYNOP_BRUTE,FRG_SYNOP_ADJUST,FRG_SYNOP_MRG)%>%
  transform(value=as.numeric(value))%>%
  filter(!is.na(value)&value>=1)%>%
  transform(LABELS=factor(x = LABELS,levels = c("OBS","BRUTE","ADJUST","MERGE")))


## PANEL LABELS
panel.labs <- dbReadTable(conn =CLIMATE_DB,name = var_names)[c(1:2),-1]

panel.labs2 <- data.frame(Names=colnames(panel.labs),
                          LON=unlist(panel.labs[1,]),
                          LAT=unlist(panel.labs[2,]))

panel.lab <- paste0(panel.labs2$Names,"\n" ," LON : ",panel.labs2$LON," ", "  LAT : ",panel.labs2$LAT)
names(panel.lab) <- unique(FRG_TMAX_SYNOP_DATA$variable)


# GRAPHIQUES
colornames <- rownames(RColorBrewer::brewer.pal.info)
RColorBrewer::brewer.pal(n = 9,name = "Blues")

ggplot(data = FRG_SYNOP_DATA,mapping = aes(x = LABELS,y = value),group=variable)+
  geom_boxplot(aes(fill=LABELS),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T,position = position_dodge(.9))+
  stat_summary(fun = mean, geom = "point",
               shape = 15, size = 1.5, color = "#000000")+
  stat_summary(fun = max,na.rm = T, geom = "point",
               shape = 21, size = 2, color = "red",fill="red")+
  stat_summary(fun= min,na.rm = T, geom = "point",
               shape = 19, size = 2, color = "green")+
  facet_wrap(facets = .~variable,ncol = 2,scales = "free_y",labeller = labeller(variable=panel.lab))+
  scale_fill_manual(name="",values = c("#41AB5D","#BD0026","#D9D9D9","#08519C"))+
  xlab("DONNEES")+
  ylab("PRECIPITATION[MM]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


## ERREUR STANDARD D'ESTIMATION

#==================================================================
#             TRAITEMENT DES DONNEES DE TEMPERATURES

### TMIN
dbfilenames <- dbListTables(CLIMATE_DB)[grep(pattern = "CDT_SYNOP_TMIN*",x = dbListTables(CLIMATE_DB))]
var_names <- "CDT_SYNOP_TMIN_2_OBS"
## Observation
TMIN_SYNOP_OBS <- dbReadTable(conn =CLIMATE_DB,name = "CDT_SYNOP_TMIN_2_OBS")[-c(1:2),]%>%
  rename(DATE=Station)%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="OBS")%>%
  select(c(DATE,LABELS,colnames(.)))
colnames(TMIN_SYNOP_OBS) <- toupper(names(TMIN_SYNOP_OBS))

FRG_TMIN_SYNOP_OBS <- TMIN_SYNOP_OBS%>%
  melt(id.vars=c("DATE","LABELS"))
### BRUTE
TMIN_SYNOP_BRUTE <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMIN_BRUTE")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="BRUTE")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_TMIN_SYNOP_BRUTE <- TMIN_SYNOP_BRUTE%>%
  melt(id.vars=c("DATE","LABELS"))

## AJUSTEE
TMIN_SYNOP_ADJUST <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMIN_ADJUST")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="ADJUST")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_TMIN_SYNOP_ADJUST <- TMIN_SYNOP_ADJUST%>%
  melt(id.vars=c("DATE","LABELS"))
## MERGEE
TMIN_SYNOP_MRG <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMIN_MRG2")[-c(1:2),]%>%
  rename(DATE=ID)%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="MERGE")%>%
  select(c(DATE,LABELS,colnames(.)[order(colnames(.))]))

FRG_TMIN_SYNOP_MRG <- TMIN_SYNOP_MRG%>%
  melt(id.vars=c("DATE","LABELS"))

## RBIND

FRG_TMIN_SYNOP_DATA <- rbind(FRG_TMIN_SYNOP_OBS,FRG_TMIN_SYNOP_BRUTE,FRG_TMIN_SYNOP_ADJUST,FRG_TMIN_SYNOP_MRG)
#FRG_TMIN_SYNOP_DATA$value[which(FRG_TMIN_SYNOP_DATA$value==-99)] <- NA

FRG_TMIN_SYNOP_DATA2 <- FRG_TMIN_SYNOP_DATA%>%
  transform(LABELS=factor(x = LABELS,levels = c("OBS","BRUTE","ADJUST","MERGE")))%>%
  filter(!is.na(value))
## PANEL LABELS
panel.labs <- dbReadTable(conn =CLIMATE_DB,name = var_names)[c(1:2),-1]
colnames(panel.labs) <- toupper(names(panel.labs))
panel.labs2 <- data.frame(Names=colnames(panel.labs),
                          LON=unlist(panel.labs[1,]),
                          LAT=unlist(panel.labs[2,]))

panel.lab <- paste0(panel.labs2$Names,"\n" ," LON : ",panel.labs2$LON," ", "  LAT : ",panel.labs2$LAT)
names(panel.lab) <- unique(FRG_TMIN_SYNOP_DATA2$variable)

# GRAPHIQUES
colornames <- rownames(RColorBrewer::brewer.pal.info)
RColorBrewer::brewer.pal(n = 9,name = "Greys")

ggplot(data = FRG_TMIN_SYNOP_DATA2,mapping = aes(x = LABELS,y = value),group=variable)+
  geom_boxplot(aes(fill=LABELS),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T,position = position_dodge(.9))+
  stat_summary(fun = mean, geom = "point",
               shape = 15, size = 1.5, color = "#000000")+
  stat_summary(fun = max,na.rm = T, geom = "point",
               shape = 21, size = 2, color = "red",fill="red")+
  stat_summary(fun= min,na.rm = T, geom = "point",
               shape = 19, size = 2, color = "green")+
  facet_wrap(facets = .~variable,ncol = 2,scales = "free_y",labeller = labeller(variable=panel.lab))+
  scale_fill_manual(name="",values = c("#41AB5D","#BD0026","#D9D9D9","#08519C"))+
  xlab("DONNEES")+
  ylab("TEMPERATURE[°C]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))

  
  
  
### TMAX
dbfilenames <- dbListTables(CLIMATE_DB)[grep(pattern = "CDT_SYNOP_TMAX*",x = dbListTables(CLIMATE_DB))]
var_names="CDT_SYNOP_TMAX_OBS"
## Observation
TMAX_SYNOP_OBS <- dbReadTable(conn =CLIMATE_DB,name = var_names)[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="OBS")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_TMAX_SYNOP_OBS <- TMAX_SYNOP_OBS%>%
  melt(id.vars=c("DATE","LABELS"))
### BRUTE
TMAX_SYNOP_BRUTE <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMAX_BRUTE")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="BRUTE")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_TMAX_SYNOP_BRUTE <- TMAX_SYNOP_BRUTE%>%
  melt(id.vars=c("DATE","LABELS"))

## AJUSTEE
TMAX_SYNOP_ADJUST <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMAX_ADJUST")[-c(1:2),]%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="ADJUST")%>%
  select(c(DATE,LABELS,colnames(.)))

FRG_TMAX_SYNOP_ADJUST <- TMAX_SYNOP_ADJUST%>%
  melt(id.vars=c("DATE","LABELS"))
## MERGEE
TMAX_SYNOP_MRG <- dbReadTable(conn =CLIMATE_DB ,name = "CDT_SYNOP_TMAX_MRG2")[-c(1:2),]%>%
  rename(DATE=ID)%>%
  filter(DATE<=20161231&DATE>19991231)%>%
  transform(LABELS="MERGE")%>%
  select(c(DATE,LABELS,colnames(.)[order(colnames(.))]))

FRG_TMAX_SYNOP_MRG <- TMAX_SYNOP_MRG%>%
  melt(id.vars=c("DATE","LABELS"))

## RBIND

FRG_TMAX_SYNOP_DATA <- rbind(FRG_TMAX_SYNOP_OBS,FRG_TMAX_SYNOP_BRUTE,FRG_TMAX_SYNOP_ADJUST,FRG_TMAX_SYNOP_MRG)%>%
  transform(LABELS=factor(x = LABELS,levels = c("OBS","BRUTE","ADJUST","MERGE")))%>%
  filter(!is.na(value))

## PANEL LABELS
panel.labs <- dbReadTable(conn =CLIMATE_DB,name = var_names)[c(1:2),-1]

panel.labs2 <- data.frame(Names=colnames(panel.labs),
                          LON=unlist(panel.labs[1,]),
                          LAT=unlist(panel.labs[2,]))

panel.lab <- paste0(panel.labs2$Names,"\n" ," LON : ",panel.labs2$LON," ", "  LAT : ",panel.labs2$LAT)
names(panel.lab) <- unique(FRG_TMAX_SYNOP_DATA$variable)
# GRAPHIQUES
colornames <- rownames(RColorBrewer::brewer.pal.info)
RColorBrewer::brewer.pal(n = 9,name = "Greys")

ggplot(data = FRG_TMAX_SYNOP_DATA,mapping = aes(x = LABELS,y = value),group=variable)+
  geom_boxplot(aes(fill=LABELS),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T,position = position_dodge(.9))+
  stat_summary(fun = mean, geom = "point",
               shape = 15, size = 1.5, color = "#000000")+
  stat_summary(fun = max,na.rm = T, geom = "point",
               shape = 21, size = 2, color = "red",fill="red")+
  stat_summary(fun= min,na.rm = T, geom = "point",
               shape = 19, size = 2, color = "green")+
  facet_wrap(facets = .~variable,ncol = 2,scales = "free_y",labeller = labeller(variable=panel.lab))+
  scale_fill_manual(name="",values = c("#41AB5D","#BD0026","#D9D9D9","#08519C"))+
  xlab("DONNEES")+
  ylab("TEMPERATURE[°C]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))



## CALCUL DE L'ERREUR STANDARD D'ESTIMATION
substr.mod <- function(x,start,stop){
  if(length(x)>1){
    y <- c()
    for (j in 1:length(x)) {
      y[j] <- substr(x = x[j],start = start,stop =stop )
    }
  }
  if(length(x)==1){
    y <- substr(x = x,start = start,stop =stop)
  }
  return(y)
  
}

dbfilenames <- dbListTables(CLIMATE_DB)[grep(pattern = "CDT_SYNOP*",x = dbListTables(CLIMATE_DB))]
#sapply(FUN = as.numeric)%>%data.frame%>%
obs <-  dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_OBS")[-c(1:2),]%>%
 # rename(DATE=Station)%>%
  mutate(DATE=as.Date(paste(substr(x =DATE,start = 1,4),
                            substr(x =DATE,start = 5,6),
                            substr(x =DATE,start = 7,8),sep = "-")),
         across(.cols = !is.Date,~as.numeric(.x)))%>%
  subset(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))
colnames(obs) <- toupper(names(obs))

sim <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_MRG2")[-c(1:2),]%>%
  #rename(DATE=ID)%>%
  mutate(DATE=as.Date(paste(substr(x =DATE,start = 1,4),
                            substr(x =DATE,start = 5,6),
                            substr(x =DATE,start = 7,8),sep = "-")),
         across(.cols = !is.Date,~as.numeric(.x)))%>%
  subset(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))

obs_ann <- round(daily2annual.data.frame(x = obs,
                                         FUN = base::sum,
                                         na.rm = T,
                                         out.fmt = "%Y",
                                         dates = 1),0)%>%
  as.data.frame

sim_ann <- round(daily2annual.data.frame(x = sim,
                                         FUN = sum,
                                         na.rm = T,
                                         out.fmt = "%Y",
                                         dates = 1),0)%>%
  as.data.frame

station_names <- names(sim_ann)

SDERROR <- as.data.frame(matrix(data = NA,nrow =nrow(sim_ann),ncol =ncol(sim_ann)))
n_iteration <- ncol(sim_ann)
j <- 0
repeat{
  j <- j+1
  sim_ann2 <- sim_ann%>%
    select(station_names[j])
  
  obs_ann2 <- obs_ann%>%
    select(contains(station_names[j]))
  
    SDERROR[,j] <- round(c(sim_ann2[,1]-obs_ann2[,1])/sd(obs_ann2[,1]),2)
  if(j==n_iteration){
    colnames(SDERROR) <- station_names
    rownames(SDERROR) <-rownames(sim_ann2) 
  }

  if(j==n_iteration)break
  
}

ESD_DATA <- SDERROR%>%transform(DATE=as.numeric(rownames(.)))%>%
  melt(id.vars="DATE")

ggplot(data = ESD_DATA,mapping = aes(x=rep(x = 1:17,10),y=value),group=variable)+
  #geom_bar( stat="identity", fill="steelblue",colour="black")+
  geom_pointrange(aes(ymin=0,ymax=value),color="steelblue")+
  scale_x_continuous(breaks = seq(1,17,4),labels =unique(ESD_DATA$DATE)[seq(1,17,4)] )+
    geom_hline(yintercept = 0, linetype = 3) + 
    
    #geom_hline(yintercept = 0, colour= "black")+
  geom_hline(yintercept = 2, colour= "red",linetype=1)+
  geom_hline(yintercept = -2, colour= "red",linetype=1)+
  facet_wrap(facets = .~variable,ncol = 2,scales = "free_y",labeller = labeller(variable=panel.lab) )+
  xlab("ANNEES")+
  ylab("ERREUR STANDARD")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))

## CRITERES
dbfilenames <- dbListTables(CLIMATE_DB)[grep("CDT_SYNOP_RAIN*",dbListTables(CLIMATE_DB))]
dbTimFiles <- c("CDT_SYNOP_RAIN_OBS","CDT_SYNOP_RAIN_BRUTE",
                "CDT_SYNOP_RAIN_ADJUST","CDT_SYNOP_RAIN_MRG2")

obs_data <- dbReadTable(conn = CLIMATE_DB,name =dbTimFiles[1] )[-c(1:2),]%>%
  #rename(DATE=Station)%>%
  subset(DATE>=20000101&DATE<=20161231)

brute_data <- dbReadTable(conn = CLIMATE_DB,name =dbTimFiles[2] )[-c(1:2),]%>%
  #rename(DATE=Station)%>%
  subset(DATE>=20000101&DATE<=20161231) 

adjust_data <- dbReadTable(conn = CLIMATE_DB,name =dbTimFiles[3] )[-c(1:2),]%>%
  #rename(DATE=Station)%>%
  subset(DATE>=20000101&DATE<=20161231)

mrg_data <- dbReadTable(conn = CLIMATE_DB,name =dbTimFiles[4])[-c(1:2),]%>%
  subset(DATE>=20000101&DATE<=20161231)

st_names <- colnames(obs_data)[-1]
#var.name <- c("OBS","BRUTE","ADJUST","MRG")
RAIN_CRIT_DATA <- rbind()
for (i in st_names) {
  obs_x <-obs_data%>%select(i)%>%unlist%>%as.numeric
  brute_x <-brute_data%>%select(contains(i))%>%unlist%>%as.numeric
  adjust_x <- adjust_data%>%select(contains(i))%>%unlist%>%as.numeric
  mrg_x <- mrg_data%>%select(contains(i))%>%unlist%>%as.numeric
  
  CRIT_DATA <- rbind(t(gof(sim = brute_x,obs_x)),
                          t(gof(sim = adjust_x,obs_x)),
                          t(gof(sim = mrg_x,obs_x)))%>%as.data.frame%>%
    select(c(RMSE,contains("PBIAS"),R2))%>%
  transform(LABELS=i,DATA=c("BRUTE","ADJUST","MERGE"))%>%select(LABELS,DATA,colnames(.))

  RAIN_CRIT_DATA <- rbind(RAIN_CRIT_DATA,CRIT_DATA)
  }

save_path <- "D:/PROJET/Article/Merging/Resultats/Fichier/"
write.table(x =RAIN_CRIT_DATA,file = paste0(save_path,"RAIN_CRIT_DATA",".txt"),sep = "\t",quote = F,row.names = F)


## Traitement de données 
RainData <- dbReadTable(conn = CLIMATE_DB,name = "RAIN_DATA_BF")%>%
  transform(Var="Rain")%>%select(-DATE)%>%melt(id.vars="Var")%>%
  filter(value>0)

TmaxData <- dbReadTable(conn = CLIMATE_DB,name = "TMAX_DATA_BF")%>%
  transform(Var="TMAX")%>%select(-DATE)%>%melt(id.vars="Var")

TminData <- dbReadTable(conn = CLIMATE_DB,name = "TMIN_DATA_BF")%>%
  transform(Var="TMIN")%>%select(-DATE)%>%melt(id.vars="Var")

ClimData <- rbind(AllDataRain,TmaxData,TminData)


ggplot(data = ClimData,mapping = aes(x = variable,y = value),group=variable)+
  geom_boxplot(aes(fill=variable),outlier.colour = "black",notch = F,
               notchwidth = .2,varwidth = F,na.rm = T,position = position_dodge(.9))+
  stat_summary(fun = mean, geom = "point",
               shape = 15, size = 1.5, color = "#000000")+
  stat_summary(fun = max,na.rm = T, geom = "point",
               shape = 21, size = 2, color = "red",fill="red")+
  stat_summary(fun= min,na.rm = T, geom = "point",
               shape = 19, size = 2, color = "green")+
  facet_wrap(facets = .~Var,ncol = 3,scales = "free_y")+
  scale_fill_manual(name="",values = c("#41AB5D","#BD0026","#D9D9D9","#08519C"))+
  xlab("DONNEES")+
  ylab("TEMPERATURE[°C]")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "outside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))

##

RainData2 <- dbReadTable(conn = CLIMATE_DB,name = "RAIN_DATA_BF")%>%
daily2annual.data.frame(FUN = sum,na.rm = T,dates = 1)

RainStandarERROR <- data.frame(Brute= round((RainData2$BRUTE-RainData2$OBS)/sd(RainData2$OBS),2),
                               Adjust= round((RainData2$ADJUST-RainData2$OBS)/sd(RainData2$OBS),2),
                               Merge= round((RainData2$MERGE-RainData2$OBS)/sd(RainData2$OBS),2))

TmaxData2 <- dbReadTable(conn = CLIMATE_DB,name = "TMAX_DATA_BF")%>%
  daily2annual.data.frame(FUN = mean,na.rm = T,dates = 1)%>%data.frame

TmaxStandarERROR <- data.frame(Brute= round((TmaxData2$BRUTE-TmaxData2$OBS)/sd(TmaxData2$OBS),2),
                               Adjust= round((TmaxData2$ADJUST-TmaxData2$OBS)/sd(TmaxData2$OBS),2),
                               Merge= round((TmaxData2$MERGE-TmaxData2$OBS)/sd(TmaxData2$OBS),2),
                               Var="Tmax")


TminData2 <- dbReadTable(conn = CLIMATE_DB,name = "TMIN_DATA_BF")%>%
  daily2annual.data.frame(FUN = mean,na.rm = T,dates = 1)%>%data.frame


TminStandarERROR <- data.frame(Brute= round((TminData2$BRUTE-TminData2$OBS)/sd(TminData2$OBS),2),
                               Adjust= round((TminData2$ADJUST-TminData2$OBS)/sd(TminData2$OBS),2),
                               Merge= round((TminData2$MERGE-TminData2$OBS)/sd(TminData2$OBS),2),
                               Var="Tmin")

TempData <- rbind(TmaxStandarERROR,TminStandarERROR)%>%
  melt(id.vars="Var")

ggplot(data = TempData,mapping = aes(x=rep(x = 1:17,6),y=value),group=variable)+
  geom_bar( stat="identity", fill="steelblue",colour="black")+
  scale_x_continuous(breaks = seq(1,17,4),labels =unique(2000:2016)[seq(1,17,4)] )+
  geom_hline(yintercept = 0, colour= "black")+
  geom_hline(yintercept = 2, colour= "red")+
  geom_hline(yintercept = -2, colour= "red")+
  facet_grid(facets = Var~variable,scales = "free_y")+
  xlab("ANNEES")+
  ylab("ERREUR STANDARD")+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 90),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


## Taylor diagram
### RAIN

dbListTables(conn = CLIMATE_DB)[grep(pattern = "RAIN",fixed = T,x = dbListTables(conn = CLIMATE_DB))]
TAMSAT_RAIN <- dbReadTable(conn = CLIMATE_DB,name = "TAMSAT_RAIN")%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))%>%filter(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))

date_ref <- TAMSAT_RAIN$DATE


TAMSAT_RAIN2 <- TAMSAT_RAIN%>%select(-DATE)%>%melt

OBS_RAIN <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_RAIN_OBS")[-c(1:2),]%>%
  transform(DATE=as.Date(paste(substr.mod(x =unlist(select(.,DATE)),start = 1,stop = 4),
                               substr.mod(x =unlist(select(.,DATE)),start = 5,stop = 6),
                               substr.mod(x =unlist(select(.,DATE)),start = 7,stop = 8),sep = "-")))%>%
  filter(DATE%in%date_ref)%>%
  select(-DATE)%>%melt(id.vars=c())%>%transform(value=as.numeric(value))

CHIRPS_RAIN <- dbReadTable(conn = CLIMATE_DB,name ="CHIRPS_RAIN")%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))%>%filter(DATE%in%date_ref)%>%
  select(-DATE)%>%melt

NOAA_RAIN <- dbReadTable(conn = CLIMATE_DB,name ="NOAA_RAIN" )%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))%>%filter(DATE%in%date_ref)%>%
  select(-DATE)%>%melt(id.vars=c())%>%transform(value=as.numeric(value))


## Taylor PLot
colornames <- rownames(RColorBrewer::brewer.pal.info)
RColorBrewer::brewer.pal(n = 9,name = "GnBu")

taylor.diagram(ref =OBS_RAIN$value,model = OBS_RAIN$value,col = "black",
               pch = 19,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),pcex = 1.5,normalize = F,main = " ")

taylor.diagram(model =TAMSAT_RAIN2$value,ref = OBS_RAIN$value,col = "red",
               pch = 9,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),add = T,pcex =1.3,normalize = F)

taylor.diagram(model =CHIRPS_RAIN$value, ref= OBS_RAIN$value,col = "blue",
               pch = 17,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),add = T,pcex = 1.3,normalize = F)

taylor.diagram(ref =OBS_RAIN$value,model = NOAA_RAIN$value,col = "orange",
               pch = 15,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),add = T,normalize = F,pcex = 1.3)

legend(x = 10.8,y = 10.4,legend =c("CHIRPS","TAMSAT","NOAA","OBSERVE"),col = c("blue","red","orange","black"),pch = c(17,15,9,19),pt.cex = 1.5)


###TEMP

dbListTables(conn = CLIMATE_DB)[grep(pattern = "TMIN",fixed = T,x = dbListTables(conn = CLIMATE_DB))]
NOAA_TMAX <- dbReadTable(conn = CLIMATE_DB,name = "NOAA_TMIN")%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))%>%filter(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))%>%
  select(-DATE)%>%melt(id.vars=c())%>%transform(value=as.numeric(value))


JRA_TMAX <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_TMIN_BRUTE")[-c(1:2),]%>%
  transform(DATE=as.Date(paste(substr.mod(x =unlist(select(.,DATE)),start = 1,stop = 4),
                               substr.mod(x =unlist(select(.,DATE)),start = 5,stop = 6),
                               substr.mod(x =unlist(select(.,DATE)),start = 7,stop = 8),sep = "-")))%>%
  filter(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))%>%
  select(-DATE)%>%melt(id.vars=c())%>%transform(value=as.numeric(value))

OBS_TMAX <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_TMIN_2_OBS")[-c(1:2),]%>%
  transform(DATE=as.Date(paste(substr.mod(x =unlist(select(.,Station)),start = 1,stop = 4),
                               substr.mod(x =unlist(select(.,Station)),start = 5,stop = 6),
                               substr.mod(x =unlist(select(.,Station)),start = 7,stop = 8),sep = "-")))%>%
  filter(DATE>=as.Date("2000-01-01")&DATE<=as.Date("2016-12-31"))%>%
  select(-c(DATE,Station))%>%melt(id.vars=c())%>%transform(value=as.numeric(value))



## Taylor PLot
colornames <- rownames(RColorBrewer::brewer.pal.info)
RColorBrewer::brewer.pal(n = 9,name = "GnBu")
source(file = "D:/PROJET/Article/Merging/Scripts/Taylor_diagram.R")
par(oma = c(0,0,0,0),omi=c(0,0,0,0),mfcol=c(1,2),bty="]")
taylor.diagram2(ref =OBS_TMAX$value,model = OBS_TMAX$value,col = "black",
               pch = 19,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),pcex = 1.5,normalize = F,main = " ",pos.cor = T)

taylor.diagram2(model =JRA_TMAX$value,ref = OBS_TMAX$value,col = "red",
               pch = 15,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),add = T,pcex =1.3,normalize = F)

taylor.diagram2(model =NOAA_TMAX$value, ref= OBS_TMAX$value,col = "blue",
               pch = 17,xlab = "Ecart-type",ylab = "RMSE",show.gamma = T,
               ngamma = 5,gamma.col ="#084081",ref.sd = T,sd.arcs = T,
               grad.corr.lines = c(seq(0.1,0.9,0.1),0.95,0.99),add = T,pcex = 1.3,normalize = F)

legend(x = 4.4,y = 5.75,legend =c("JRA-55","NOAA","OBSERVE"),col = c("red","blue","black"),pch = c(15,17,19),pt.cex = 1.5)
