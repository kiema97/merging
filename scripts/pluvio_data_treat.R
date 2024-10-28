############    TRAITEMENT DES DONNEES PLUVIOMETRIQUES   ##########

#clean your work space
rm(list = ls())

#Library 
require(magrittr)
require(data.table)
require(Kendall)
require(DBI)
require(RSQLite)
require(dplyr)

#*********************************************************************#
#       REORGANISATION & STAOCKAGE DES DONNEES DANS UNE BASE          #
#*********************************************************************#

  # Création d'une base de données SQLite

setwd("G:/PROJET/Article/Merging/Data/Station_Pluvio")
RainDataBase <- dbConnect(RSQLite::SQLite(),"Rain_DataBase.sqlite")

rain_data <- dbReadTable(conn =RainDataBase ,name = "RAIN_DATA")
#write.table(x = rain_data,file = "rainfall.csv",append = FALSE,quote = FALSE,
            #sep = ",",row.names = FALSE)
stations_id_file <- read.table(file = "G:/PROJET/Article/Merging/Data/CDT_Data/Obs/CDT_RAIN_DATA.txt",
                        header =TRUE,sep = "\t")[1:3,]
stations_id_file2 <- as.data.frame(t(stations_id_file))
colnames(stations_id_file2) <- c("station","lon","lat")
stations_id_file3 <- stations_id_file2[-1,]

rain_data2 <- rain_data[,stations_id_file3$station]
station_id_file <- dbReadTable(conn =RainDataBase ,name = "station_id_file")


stations <- unique(colnames(rain_data))[-1]
stations_ids <- rbind()
for (i in 2:(ncol(rain_data))) {
  start_ <- !is.na(rain_data[,i])
  names(start_) <- 1:length(start_)
  n_date <- start_[start_==TRUE][1]
  start_date <- rain_data$DATE[as.numeric( names(n_date))]
  
  end_ <- start_[start_==TRUE]
  end_2 <- end_[length(end_)]
  end_date <- rain_data$DATE[as.numeric( names(end_2))]
  station <- stations[i]
  station_corrdinates <- station_id_file[station_id_file$Id_St==station,]
  
  station_id <- data.frame(NAME=station,
                             LONG=station_corrdinates$Long,
                             LAT=station_corrdinates$Lat,
                             START_DATE=start_date,
                             END_DATE=end_date)
  stations_ids <- rbind(stations_ids,station_id)
}


# Réorganisation des données et stockage dans la base de données

ff <- list.files(pattern = ".dat",full.names = F)

RR <- NULL
n_years <- NULL
for (i in ff) {
  x  <- nchar(i)-4
  id <- substr(i,1,x)
  dd <-  fread(i,
               na.strings = c("","NAN","NA","*","-99999","8888","9988","M"),
               header = F,skip = 0,
               sep = 'auto')   
  dd0   <- dd[,-c(1,2,3,4,seq(7,67,2))]
  colnames(dd0) <- c("date",paste0("Jour_",1:31))
  
  dbWriteTable(conn = RainDataBase,#
               name =toupper(id),#
               value =dd0,#
               overwrite=T)
  
}
#*******************************************************************#
#                 FORMATAGE DES DONNEES AU FORMAT CDT
#*******************************************************************#

# Organisation des données station

file_names <- dbListTables(conn = RainDataBase)[-c(74,75)]

o_date <- seq.Date(from = as.Date(x="1961-01-01",format="%Y-%m-%d"),
                   to=as.Date(x="2008-12-31",format="%Y-%m-%d"),by="d")

date_objet <- data.frame(DATE= paste(format(x = o_date,"%Y"), 
                                     format(x = o_date,"%m"),
                                    as.numeric(format(x = o_date,"%d")),sep = "-"))

rain_data <- data.frame(Xdate=seq.Date(from = as.Date(x="1961-01-01",format="%Y-%m-%d"),
                                       to=as.Date(x="2008-12-31",format="%Y-%m-%d"),by="d"))
l_month_idx <- c(1,3,5,7,8,10,12)
s_month_idx <- c(4,6,9,11)
j <- 0
repeat{
  j <- j+1
  if(j>length(file_names))break
  # 1
  or_file <- dbReadTable(conn = RainDataBase,name =file_names[j])
  year01 <- as.numeric(substr(x = or_file$date,start = 1,stop = 4))
  month01 <- as.numeric(substr(x = or_file$date,start = 6,stop = 7))
  md_file01 <- cbind(year01,month01,or_file[,-1])
  #2
  n_y <- unique(year01)
  station_data <- rbind()
  p <- 0
  for (jj in n_y) {
    p <- p+1
    md_file02 <- subset.data.frame(x = md_file01,subset =md_file01$year01==jj)
    year_data <- rbind()
    for (jjj in md_file02$month01 ) {
     if(jjj%in%l_month_idx){
       if(jjj<10){
         month_data <- data.frame(DATE=paste(jj,paste0("0",jjj),1:31,sep = "-"),
                                  VALUE=unlist(subset(x = md_file02,subset =month01==jjj ,select = -c(1,2))))
       }else{
         month_data <- data.frame(DATE=paste(jj,jjj,1:31,sep = "-"),
                                  VALUE=unlist(subset(x = md_file02,subset =month01==jjj ,select = -c(1,2))))
       }
     }
      if(jjj%in%s_month_idx){
        if(jjj<10){
          month_data <- data.frame(DATE=paste(jj,paste0("0",jjj),1:30,sep = "-"),
                                   VALUE=unlist(subset(x =md_file02 ,subset=month01==jjj,select=-c(1,2,33))))
        }else{
          month_data <- data.frame(DATE=paste(jj,jjj,1:30,sep = "-"),
                                   VALUE=unlist(subset(x =md_file02 ,subset=month01==jjj,select=-c(1,2,33))))
        }
        
      }
      if(jjj==2){
        if(jj%%4==0){
          month_data <- data.frame(DATE=paste(jj,paste0("0",jjj),1:29,sep = "-"),
                                   VALUE=unlist(subset(x =md_file02 ,subset=month01==jjj,select=-c(1,2,32,33))))
        }
        if(!jj%%4==0){
          month_data <- data.frame(DATE=paste(jj,paste0("0",jjj),1:28,sep = "-"),
                                   VALUE=unlist(subset(x =md_file02 ,subset=month01==jjj,select=-c(1,2,31,32,33))))        }
      }
      year_data <- rbind(year_data,month_data)
   
    }
    
    station_data <- rbind(station_data,year_data)
    if(p==length(n_y)){
      station_data_l <-  merge.data.frame(x=station_data,y = date_objet,by.y = "DATE",by.x = "DATE",all = T)
      rownames(station_data_l) <- NULL
    }
  
  }
    rain_data <- cbind(rain_data,select(station_data_l,VALUE))
    if(j==length(file_names)){
      colnames(rain_data) <- c("DATE",file_names)
    }
  
 
}

# FORMATAGE DES DONNEES AU FORMAT CDT
st_id_file <- dbReadTable(conn = RainDataBase,name = "station_id_file")#%>%
syn_st_id_file <- dbReadTable(conn = RainDataBase,name = "syn_st_id_file")%>%
  transform(ID=toupper(unlist(select(.,ID))))


Synoptic_data <- dbReadTable(conn = RainDataBase,name ="synoptic_rain" )%>%
  transform.data.frame(DATE=as.Date(paste(unlist(select(.,Year)),
                                          unlist(select(.,Month)),
                                          unlist(select(.,Day)),sep = "-")))%>%
  select(colnames(.)[c(14,4:13)])%>%
rename(Bobo=Bobo_dioulasso)
colnames(Synoptic_data) <- toupper(paste0("SYN_",colnames(Synoptic_data)))


Pst_Pluvio_data <-dbReadTable(conn = RainDataBase,name = "RAIN_DATA")%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))#%>%
  
st_names <- colnames(Pst_Pluvio_data)[-1]

Pst_Pluvio_data <- Pst_Pluvio_data%>%
  select(-c(st_names[!st_names%in%st_id_file$Id_St]))


duplicate.stations <- c("BOBO","BOROMO","DEDOUGOU","GAOUA")         
Pst_Pluvio_data_01 <- Pst_Pluvio_data%>%
select(-c(duplicate.stations))

Synoptic_data_01 <- Synoptic_data
colnames(Synoptic_data_01) <- toupper(paste0("SYN_",colnames(Synoptic_data_01)))

RAINDATA <- merge.data.frame(x = Pst_Pluvio_data_01,y =Synoptic_data, by.x ="DATE",by.y = "SYN_DATE",all = T )
RAINDATA$DATE <- paste0(format(RAINDATA$DATE,"%Y"),
                        format(RAINDATA$DATE,"%m"),
                        format(RAINDATA$DATE,"%d")) 

#
st_id_file_01 <- st_id_file%>%
  subset(Id_St%in%colnames(Pst_Pluvio_data_01))
 
station_caract <- t(data.frame(X1=c("Stations",st_id_file_01$Id_St,syn_st_id_file$ID),
                   X2=c("LON",st_id_file_01$Long,syn_st_id_file$x),
                   X3=c("DAILY/LAT",st_id_file_01$Lat,syn_st_id_file$Y))
)
colnames(station_caract) <- colnames(RAINDATA)

CDT_RAIN_DATA <- rbind(station_caract,RAINDATA)

write.table(x = obs_data3,file = "./CDT_DATA/CDT_RAIN_Val_DATA.txt",quote = F,sep = "\t",na = "-9999",row.names = F,col.names = T)

STATIONS.NAMES <- t(c("Stations",st_id_file_01$Id_St,syn_st_id_file$ID))
names(STATIONS.NAMES) <- colnames(RAINDATA)
LON <- t(c("LON",st_id_file_01$Long,syn_st_id_file$x))

LAT <- t(c("DAILY/LAT",st_id_file_01$Lat,syn_st_id_file$Y))

df <- rbind(STATIONS.NAMES,RAINDATA)

dd <- data.frame(STATIONS.NAMES,LON,LAT)

syn_st_id_file <- readxl::read_xlsx(path = "D:/PROJET/Article/Merging/Data/synoptic/stationsynoptique.xlsx")
Pst.pluvio.names <- colnames(Pst_Pluvio_data)

Exiting.stations <- subset(x = dd_rm_f,subset =dd_rm_f$id_st%in%station_id_file$Id_St )

station_id_file2 <- subset(x = station_id_file,subset = Id_St%in%Exit_file$id_st)


#===================================Fin

rain_data$DATE <- as.character(rain_data$DATE)
dbWriteTable(conn = RainDataBase,name = "syn_st_id_file",value = syn_st_id_file,overwrite=T)
dd_rm_f <- data.frame(id_st=dbListTables(RainDataBase))

bromo <- dbReadTable(conn = RainDataBase,name = "RAIN_DATA")%>%
  transform(DATE=as.Date(unlist(select(.,DATE))))%>%
  subset.data.frame(DATE>=as.Date("1980-01-01"))

synoptic_station <- dbReadTable(conn = RainDataBase,name ="synoptic_rain" )%>%
  subset(Year<=2008)

sum(bromo$GAOUA==synoptic_station$Gaoua,na.rm = T)

station_id_file <- dbReadTable(conn = RainDataBase,name = "station_id_file")

Exit_file <- subset(x = dd_rm_f,subset =dd_rm_f$id_st%in%station_id_file$Id_St )

station_id_file2 <- subset(x = station_id_file,subset = Id_St%in%Exit_file$id_st)

lat_ob <- t(data.frame(DAY= c("DAY/LAT",station_id_file)))
rain_data_2 <- rain_data[,c("DATE",Exit_file$id_st)]









