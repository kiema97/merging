# EXTRACTION DES DONNEES 
rm(list=ls())
#LIBRARIES
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(dplyr)
library(ncdf4)
library()
library(DBI)
library(RSQLite)
library(magrittr)
library(RNetCDF)

# FIXATION DU REPERTOIRE DE TRAVAIL
setwd("D:/PROJET/Article/Merging/Data/TAMSAT_v3_w&cafr")

# IMPORTATION DES DONNNEES
CLIMATE_DB <- dbConnect(drv = RSQLite::SQLite(),"D:/PROJET/Article/Merging/Data/Station_Pluvio/Rain_DataBase.sqlite")
Coordinate_files <- dbReadTable(conn =CLIMATE_DB,name = "syn_st_id_file" )

Coordinate_files2 <- Coordinate_files%>%select(c(x,Y))

## Extraction de données de précipitation
FilesNames <- dir(pattern = "*.nc")
DATE <- substr(x = FilesNames,start = 4,stop = 13)
n_iteration <- length(FilesNames)
ExampleFile <- raster::brick(FilesNames[1])

  SaveObject <- rbind()
  p <- 0
  repeat{
    p <- p+1
    RasterBirck <- raster::brick(FilesNames[p])
    RasterExtract <- t(round(raster::extract(x =RasterBirck, y = Coordinate_files2),2))
    SaveObject <- rbind(SaveObject,RasterExtract)
    
    if(p==n_iteration){
      DATE2 <- DATE[1:n_iteration]
      colnames(SaveObject) <- Coordinate_files$ID
      SaveObject_Final <- data.frame(DATE=DATE2,SaveObject)}
    
    message(noquote(paste0("EXTRACTION : ",round(p/n_iteration*100,2),"%")))
    if(p==n_iteration)break
  }
  
SaveObject_Final$DATE <- as.character(paste(substr(x = SaveObject_Final$DATE,start = 1,stop = 4),
                                       substr(x = SaveObject_Final$DATE,start = 6,stop = 7),
                                       substr(x = SaveObject_Final$DATE,start = 9,stop = 10),sep = "-"))
  
  
MRG_Synop_Tmin <- read.csv(file = "D:/PROJET/Article/Merging/Resultats/CDT/CDT_Tmin/CrossValidation_Temp_Data_1Jan2000_31Dec2016/CROSS-VALIDATION_DATA.csv",na.strings = "-99")#[-c(1:2),]
colnames(MRG_Synop_Tmin) <- toupper(names(MRG_Synop_Tmin))


rf_data <- read.table(file = "D:/PROJET/Article/Merging/Data/NOAA/resultat/chirps_pluie.txt",sep = "\t",header = T,dec = ",")%>%
  transform(DATE=rownames(.))%>%select(c(DATE,colnames(.)))

colnames(rf_data) <- toupper(colnames(SaveObject_Final))
rownames(rf_data) <- NULL

dbWriteTable(conn = CLIMATE_DB,name = "CHIRPS_RAIN",value = rf_data,overwrite=T,field.types=c(DATE="DATE"))
#dbRemoveTable(conn = CLIMATE_DB,name = "CHIRPS_TMIN")

CDT_SYNOP_RAIN_MRG <- dbReadTable(conn = CLIMATE_DB,name ="CDT_SYNOP_RAIN_BRUTE" )
CDT_SYNOP_RAIN_MRG_2 <- CDT_SYNOP_RAIN_MRG[-c(1:2),]%>%filter(DATE>=20000101)
CDT_SYNOP_RAIN_MRG_3 <- rbind(CDT_SYNOP_RAIN_MRG[c(1:2),],CDT_SYNOP_RAIN_MRG_2)

SYNOP_MRG_CHIRPS_ <- rbind(CDT_SYNOP_RAIN_MRG[c(1:2),],SYNOP_MRG_CHIRPS)

SYNOP_MRG_CHIRPS <- read.table(file = "D:/PROJET/Article/Merging/Data/CDT_Data/Obs/SYNOP_MRG_CHIRPS.txt",header = T,sep = "\t",na.strings = "-99")%>%
  transform(DATE=as.Date(DATE))%>%
transform(DATE=paste0(format(DATE,"%Y"),format(DATE,"%m"),format(DATE,"%d")))%>%
  select(-LABELS)
write.table(x = CDT_SYNOP_RAIN_MRG_3,file ="D:/PROJET/Article/Merging/CDT_DBase/CDT_SYNOP_RAIN_BRUTE_VAL.txt",quote = F,sep = "\t",na ="NA",row.names = F,col.names = T)

## FORMATAGE CDT
dbListTables(CLIMATE_DB)
CDT_SYNOP_RAIN_OBS <- read.table(file = "D:/PROJET/Article/Merging/Data/CDT_Data/Obs/CDT_RAIN_DATA2.txt",header = T,sep = "\t",na.strings =  "-9999")%>%
  select(contains(c("DATE","SYN")))
colnames(CDT_SYNOP_RAIN_OBS) <- CDT_SYNOP_RAIN_OBS[1,]
CDT_SYNOP_RAIN_OBS2 <- CDT_SYNOP_RAIN_OBS[-1,]%>%
  rename(DATE=Stations,FADA=FADA_NGOURMA)

colnames(CDT_SYNOP_TMAX_OBS) <- c("DATE",toupper(names(CDT_SYNOP_TMAX_OBS)[-1]))
colnames(CDT_SYNOP_TMAX_OBS)
#xx <- read.csv(file = "D:/PROJET/Article/Merging/Resultats/CDT/BOBO_CROSS_VAL/CrossValidation_Precip_Data_1Jan2000_31Dec2017/CROSS-VALIDATION_DATA.csv")[-c(1:2),]
#MRG_DATE <- seq.Date(from = as.Date("2000-01-01"),to = as.Date("2017-12-31"),by = "d")

CDT_SYNOP_RAIN_BRUTE <- dbReadTable(conn =CLIMATE_DB,name ="CHIRPS_SYNOP_RAIN")%>%
  filter(DATE>=19810101&DATE<=20161231)%>%
  rename(FADA=Fada_Ngourma)%>%
  select(contains(colnames(CDT_SYNOP_TMAX_OBS)))
  
colnames(CDT_SYNOP_RAIN_BRUTE) <- toupper(names(CDT_SYNOP_RAIN_BRUTE))
 
CDT_SYNOP_RAIN_BRUTE2 <- rbind(CDT_SYNOP_TMAX_OBS[c(1:2),],CDT_SYNOP_RAIN_BRUTE)

dbWriteTable(conn = CLIMATE_DB,name = "CDT_SYNOP_RAIN_OBS",value =CDT_SYNOP_RAIN_OBS2,overwrite=F )

save_path="D:/PROJET/Article/Merging/CDT_DBase/"
write.table(x =TmaxMRG ,file = paste0(save_path,"CDT_SYNOP_TMAX_MERGE",".txt"),sep = "\t",na = "-99",
            row.names = F)

colnames(CDT_CHIRPS_SYNOP_RAIN) <- c("DATE",paste0("SYN_",toupper(colnames(CDT_CHIRPS_SYNOP_RAIN)[-1])))

CDT_CHIRPS_SYNOP_RAIN2 <- rbind(CDT_OBS_SYNOP[1:3,],CDT_CHIRPS_SYNOP_RAIN)
CDT_OBS_SYNOP2 <- CDT_OBS_SYNOP%>%
  select(c(colnames(.)[1],colnames(.)[3:5],SYN_BOBO))

write.table(x = CDT_SYNOP_Tmin_Brute2,file ="D:/PROJET/Article/Merging/Data/CDT_Data/Obs/CROSS_VAL_RainData_4St.txt",quote = F,
            sep = "\t",na ="-99",row.names = F,col.names = F )





dd <- dbReadTable(conn = CLIMATE_DB,name = "TAMSAT_RAIN")

















## EXTRACTION DES DONNEES CHIRPS AU LIMITES DU BURKINA
bf_shp <- readOGR(dsn = "D:/AGRHYMET/SIG/Database/Donnes_SIG/Administration",layer = "Région")
r_path <- "D:/PROJET/Article/Merging/Data/Satellitaire/Rain/CHIRPS_19812018WA"
save_path <- "D:/PROJET/Article/Merging/Data/CHIRPS_BF/"
setwd(r_path)
bf_filenames <- dir(pattern = "*nc")
bf_extent <- as( extent(-6,2.5,9,15.5), 'SpatialPolygons')
 
rrr <- 
j <- 0
repeat{
  j <- j+1
  print(j)
  rr <- stack(x = bf_filenames[j])
  raster_files <- nc_open(filename = bf_filenames[j])
  if(j==1) crs(bf_extent) <- crs(raster_files)
  raster_crop_bf <- raster::crop(x = raster_files,
                                 y = bf_extent)
  raster::writeRaster(x = raster_crop_bf,filename = paste0(save_path,"bf_",bf_filenames[j]),overwrite=T)
if(j==length(bf_filenames))break
  }

