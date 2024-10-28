###===================================================#
###      PREPARATION DES DONNEES DE SPATIALISATION               #
###====================================================
## NETOYAGE
#rm(list = ls())
##LIBRARIES
library(DBI)
library(RSQLite)
library(tidyverse)
library(dplyr)
library(foreign)
library(raster)
library(reshape2)
library(airGR)
##WORK SPACE
setwd("D:/PROJET/Article/Merging/Resultats")
## DATA BASE
db_path <- "D:/PROJET/Article/Merging/Projet_SIG/SpatialisationData/DataSpatialisation.sqlite"
MergingDataBase <- dbConnect(drv = RSQLite::SQLite(),dbname=db_path)
##EXTRACT POINTS
Cordinate.Station <- read.dbf(file = "D:/PROJET/Article/Merging/Projet_SIG/SpatialisationData/ExtractPoint.dbf")%>%
  mutate(Name=paste0("Stat_",1:44))%>%
  dplyr::select(Name,LONG,LAT)
##DATA EXTRACTION
dir.name <- c("RAIN","TMAX","TMIN")
CLIMDATA_BV_Nowk <- rbind()
j <- 0
repeat{
  j <- j+1
  if(j>length(dir.name))break
  file_path <- file.path(paste0(dir.name[j],"_MRG_BF"),"Extracted_DATA")
  File.Names <- dir(path = file_path)
  if(dir.name[j]=="RAIN"){
    DATE <- seq.Date(from = as.Date("1981-01-01"),
                     to = as.Date("2017-12-31"),by = "d")
  }else{
    DATE <- seq.Date(from = as.Date("1961-01-01"),
                     to = as.Date("2017-06-30"),by = "d")
  }
  ExtractData <- rbind()
  k <- 0
  for (i in File.Names) {
    print(paste0("Extract ", dir.name[j]," :", round(k/length(File.Names)*100,2), "%" ))
    k <- k+1
    file_brick <- brick(x = file.path(file_path,i))
    extracted_data <- t(round(raster::extract(x = file_brick,Cordinate.Station[,-1]),2))
    ExtractData <- rbind(ExtractData,extracted_data)
    if(k==length(File.Names)){
      colnames(ExtractData) <- Cordinate.Station$Name
      ExtractData2 <- ExtractData%>%
        as_tibble()%>%
        mutate(DATE=DATE)%>%
        dplyr::select(DATE,everything())%>%
        melt(id.vars="DATE")%>%
        mutate(VAR=dir.name[j])
      colnames(ExtractData2) <- c("DATE","Station","Value","Variable")
      
    }
  }
  CLIMDATA_BV_Nowk <- rbind(CLIMDATA_BV_Nowk,ExtractData2)
  
}

## TRAITEMENTS
ClimatePeriode <- interval(start = as.Date("1981-01-01"),end = as.Date("2010-12-31"))
ExtractPoints <- dbReadTable(conn = MergingDataBase,name ="ExtractPoints" )
DataSpatial <- dbGetQuery(conn =MergingDataBase,statement = "SELECT*FROM DataSpatialisation WHERE Variable !='RAIN2'" )%>%
  mutate(DATE=as.Date(DATE),YEARS=year(DATE))%>%
  filter(DATE%within%ClimatePeriode)

RainData <- DataSpatial%>%
  filter(Variable=="RAIN",!is.na(Value))%>%
  group_by(Station, YEARS)%>%
  summarize(value=sum(Value))%>%
  summarize(MeanValue=round(mean(value),2))%>%
  left_join(ExtractPoints,by=c("Station"="Name"))%>%
  dplyr::select(Station,LONG,LAT,MeanValue)
  
  

TminData <- DataSpatial%>%
  filter(Variable=="TMIN",!is.na(Value))%>%
  group_by(Station, YEARS)%>%
  summarize(value=mean(Value))%>%
  summarize(MeanValue=round(mean(value),2))%>%
  left_join(ExtractPoints,by=c("Station"="Name"))%>%
  dplyr::select(Station,LONG,LAT,MeanValue)

TmaxData <- DataSpatial%>%
  filter(Variable=="TMAX",!is.na(Value))%>%
  group_by(Station, YEARS)%>%
  summarize(value=mean(Value))%>%
  summarize(MeanValue=round(mean(value),2))%>%
  left_join(ExtractPoints,by=c("Station"="Name"))%>%
  dplyr::select(Station,LONG,LAT,MeanValue)

ETPData <- DataSpatial%>%
  filter(Variable!="RAIN",!is.na(Value))%>%
  left_join(ExtractPoints,by=c("Station"="Name"))%>%
  dplyr::select(DATE,Station,LONG,LAT,Variable,Value)%>%
  group_by(DATE,Station,LONG,LAT)%>%
  summarize(Tmean=mean(Value))%>%
  group_by(DATE,Station)%>%
  mutate(Jday=yday(DATE),
         ETP=PE_Oudin(JD =Jday,
                      Temp = Tmean,
                      Lat = LAT,
                      LatUnit = "deg"),
         YEARS=year(DATE))%>%
  dplyr::select(DATE,YEARS,Station,ETP)%>%
  group_by(Station, YEARS)%>%
  summarize(value=sum(ETP))%>%
  summarize(MeanValue=round(mean(value),2))%>%
  left_join(ExtractPoints,by=c("Station"="Name"))%>%
  dplyr::select(Station,LONG,LAT,MeanValue)
save_path2 <- "D:/PROJET/Article/Merging/Projet_SIG/SpatialisationData"
write.table(x = TmaxData,file = file.path(save_path2,"TmaxData.txt"),quote = FALSE,sep = "\t",row.names = FALSE )
