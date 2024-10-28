#####################################################
#      EVALUTATION DES DONNEES MERGEES              #
#####################################################
## NETOYAGE
 #rm(list=ls())
#LIBRARIES
library(dplyr)
library(ggplot2)
library(DBI)
library(RSQLite)
library(reshape2)
library(stringr)
library(hydroTSM)
library(lubridate)
library(tidyverse)
# WORK SPACE AND DATA BASE
setwd("D:/PROJET/Article/Merging")
CLIMATE_DB <- dbConnect(drv = RSQLite::SQLite(),"D:/PROJET/Article/Merging/Data/Station_Pluvio/Rain_DataBase.sqlite")

# IMPORTATION DES DONNEES
## OBSERVATION
OBS_RAIN <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_OBS")[-c(1:2),]%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%
mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = sum,na.rm = TRUE,dates = 1)%>%
  round%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())

OBS_TMAX <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_TMAX_OBS" )[-c(1:2),]%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%
  mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = mean,na.rm = TRUE,dates = 1)%>%
  round(1)%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())

OBS_TMIN <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_2_OBS")[-c(1:2),]%>%
  rename(DATE=Station)%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%
  mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = mean,na.rm = TRUE,dates = 1)%>%
  round(1)%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())


## MERGEES
MERG_RAIN <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_MRG2")[-c(1:2),]%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%
  mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = sum,na.rm = TRUE,dates = 1)%>%
  round%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())

MERG_TMAX <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_MRG2")[-c(1:2),]%>%
  rename(DATE=ID)%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%#
  mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = mean,na.rm = TRUE,dates = 1)%>%
  round(1)%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())

MERG_TMIN <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_TMIN_MRG2" )[-c(1:2),]%>%
  rename(DATE=ID)%>%
  filter(DATE>=20000101,DATE<=20141231)%>%
  mutate(DATE=paste(substr(x = DATE,start = 1,stop = 4),
                    substr(x = DATE,start = 5,stop = 6),
                    substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date)%>%
  mutate(across(.cols =!is.Date,~as.numeric(.x)))%>%
  daily2annual.data.frame(FUN = mean,na.rm = TRUE,dates = 1)%>%
  round(1)%>%
  as.data.frame%>%
  mutate(DATE=rownames(.))%>%
  dplyr::select(DATE,everything())


## Calcul des résidus

RAIN_RESIDUAL <- (OBS_RAIN[,-1]-MERG_RAIN[,-1])%>%
  mutate(DATE=rownames(OBS_RAIN[,1]))


#############################################
# TRAITEMENT DES DONNEES
TablesNames <- dbListTables(conn = CLIMATE_DB)
## RAIN
station_names1 <- mrg_rain <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_MRG2")[-c(1:2),]%>%
colnames()
station_names <- station_names1[-c(1:2)]
mrg_rain <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_MRG2")[-c(1:2),]%>%
  mutate(across(.cols = station_names,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,station_names)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~sum(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(MERGE=mean(BOGANDE:PO))%>%
  dplyr::select(YEARS,MERGE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

adjust_rain <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_ADJUST")[-c(1:2),]%>%
  mutate(across(.cols = station_names,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,station_names)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~sum(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(ADJUST=mean(BOGANDE:PO))%>%
  dplyr::select(YEARS,ADJUST)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

brute_rain <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_BRUTE")[-c(1:2),]%>%
  mutate(across(.cols = station_names,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~sum(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(BRUTE=mean(BOGANDE:PO))%>%
  dplyr::select(YEARS,BRUTE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

obs_rain <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_OBS")[-c(1:2),]%>%
  mutate(across(.cols = station_names,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~sum(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(OBS=mean(BOGANDE:PO))%>%
  dplyr::select(YEARS,OBS)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

RAIN_DATA <- obs_rain%>%
  left_join(brute_rain,by="YEARS")%>%
  left_join(adjust_rain,by="YEARS")%>%
  left_join(mrg_rain,by="YEARS")%>%
  #summarize(across(.cols = 2:5,~(.x-OBS)/sd(OBS)))%>%
  mutate(DATE=2000:2016)%>%
  select(DATE,OBS:MERGE)%>%
  gather(key = variable,value = value,BRUTE,ADJUST,MERGE,OBS)%>%
  mutate(variable=factor(x = variable,levels = c("OBS","BRUTE","ADJUST","MERGE"),labels = c("OBS","BRUTE","AJUSTEE","MERGEE")),
         TEMP="PLUIE")



## TMAX
station_names <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_MRG2")[-c(1:2),]%>%
colnames
station_names <- station_names[-1]
mrg_tmax <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_MRG2")[-c(1:2),]%>%
  rename(DATE=ID)%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(MERGE=mean(BOBO:PO))%>%
  dplyr::select(YEARS,MERGE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

adjust_tmax <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_ADJUST")[-c(1:2),]%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(ADJUST=mean(BOBO:PO))%>%
  dplyr::select(YEARS,ADJUST)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

brute_tmax <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_BRUTE")[-c(1:2),]%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(BRUTE=mean(BOBO:PO))%>%
  dplyr::select(YEARS,BRUTE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

obs_tmax <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMAX_OBS")[-c(1:2),]%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(OBS=mean(BOBO:PO))%>%
  dplyr::select(YEARS,OBS)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

TMAX_DATA <- obs_tmax%>%
  left_join(brute_tmax,by="YEARS")%>%
  left_join(adjust_tmax,by="YEARS")%>%
  left_join(mrg_tmax,by="YEARS")%>%
  #summarize(across(.cols = 2:5,~(.x-OBS)/sd(OBS)))%>%
  mutate(DATE=2000:2016)%>%
  select(DATE,OBS:MERGE)%>%
  gather(key = variable,value = value,BRUTE,ADJUST,MERGE,OBS)%>%
  mutate(variable=factor(x = variable,levels = c("OBS","BRUTE","ADJUST","MERGE"),
                         labels =c("OBS","BRUTE","AJUSTEE","MERGEE") ),
         TEMP="TMAX")

## TMIN
station_names <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_MRG2")[-c(1:2),]%>%
  colnames
station_names <- station_names[-1]
mrg_tmin <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_MRG2")[-c(1:2),]%>%
  rename(DATE=ID)%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(MERGE=mean(BOBO:PO))%>%
  dplyr::select(YEARS,MERGE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

adjust_tmin <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_ADJUST")[-c(1:2),]%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(ADJUST=mean(BOBO:PO))%>%
  dplyr::select(YEARS,ADJUST)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

brute_tmin <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_BRUTE")[-c(1:2),]%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(BRUTE=mean(BOBO:PO))%>%
  dplyr::select(YEARS,BRUTE)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

col_remane <- function(data,names){
  colnames(data) <- names
  data
}

obs_tmin <- dbReadTable(conn = CLIMATE_DB,name = "CDT_SYNOP_TMIN_2_OBS")[-c(1:2),]%>%
  rename(DATE=Station)%>%
  col_remane(toupper(colnames(.)))%>%
  mutate(across(.cols = 2:11,~as.numeric(.x)),DATE=paste(substr(x = DATE,start = 1,stop = 4),
                                                         substr(x = DATE,start = 5,stop = 6),
                                                         substr(x = DATE,start = 7,stop = 8),sep = "-")%>%as.Date,
         YEARS=year(DATE))%>%
  select(12,2:11)%>%
  group_by(YEARS)%>%
  summarize(across(.cols =station_names,~mean(.x,na.rm = TRUE) ))%>%
  rowwise()%>%
  mutate(OBS=mean(BOBO:PO))%>%
  dplyr::select(YEARS,OBS)%>%
  filter(YEARS>=2000, YEARS<=2016)%>%
  ungroup()

TMIN_DATA <- obs_tmin%>%
  left_join(brute_tmin,by="YEARS")%>%
  left_join(adjust_tmin,by="YEARS")%>%
  left_join(mrg_tmin,by="YEARS")%>%
  #summarize(across(.cols = 2:5,~(.x-OBS)/sd(OBS)))%>%
  mutate(DATE=2000:2016)%>%
  select(DATE,OBS:MERGE)%>%
  gather(key = variable,value = value,BRUTE,ADJUST,MERGE,OBS)%>%
  mutate(variable=factor(x = variable,levels = c("BRUTE","ADJUST","MERGE","OBS"),
                         labels =c("BRUTE","AJUSTEE","MERGEE","OBS") ),
         TEMP="TMIN")

TEMP_DATA <- rbind(RAIN_DATA,TMIN_DATA,TMAX_DATA)
panel_names <- "PRECIPITATION"#c("OBS","BRUTE","AJUSTEE","MERGEE") 
names(panel_names) <-"PLUIE"# c("OBS","BRUTE","ADJUST","MERGE")

TEMP_DATA2 <- TEMP_DATA%>%
  filter(TEMP=="TMIN",variable=="OBS")

ggplot(data = TEMP_DATA2,mapping = aes(y=value,fill=variable))+
  #geom_bar( stat="identity", fill="steelblue",colour="black")+
  #geom_pointrange(aes(ymin=0,ymax=value),color="steelblue")+
  geom_boxplot()+
  #scale_x_continuous(breaks = seq(2000,2016,2) )+
  #geom_hline(yintercept = 0, linetype = 1) + 
  
  #geom_hline(yintercept = 0, colour= "black")+
  #geom_hline(yintercept = 2, colour= "red",linetype=2)+
  #geom_hline(yintercept = -2, colour= "red",linetype=2)+
  facet_wrap(facets = .~TEMP,labeller = labeller(TEMP=panel_names),scales = "free_y")+
  #ggplot2::stat_summary(geom = "point",fun.y = mean,shape=15,color="black")+
  xlab("DONNEES")+
  ylab("PRECIPITATION [MM]")+
  scale_fill_manual("",values = c("green","red","gray","blue"))+
  theme_bw()+
  theme(legend.position = "top",
        strip.placement = "inside",
        strip.background = element_rect(fill = NA,
                                        colour = "black",
                                        size = 1),
        strip.text = element_text(face = "bold",
                                  colour = "black",size = 10),
        axis.text.x = element_text(face = "bold",
                                   size = 10,colour = "black",angle = 0.0),
        axis.text.y =element_text(face = "bold",
                                  size = 10,colour = "black",angle = 0),
        axis.title.x =element_text(face = "bold",
                                   size = 12,colour = "black",angle = 0),
        axis.title.y = element_text(face = "bold",
                                    size = 12,colour = "black",angle = 90))


TablesNames <- dbListTables(CLIMATE_DB)
