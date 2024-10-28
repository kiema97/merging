# TRAITEMENT DES DONNEES CLIMATIQUES MERGEES
rm(list = ls())
#LIBRARY
library(dplyr)
library(hydroTSM)
library(reshape2)
library(ggplot2)
library(readxl)
library(DBI)
library(RSQLite)
# ESPACE DE TRAVAIL
setwd("D:/PROJET/Article/Merging")

## MOD. FUNCTION
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

# IMPORTATION DES DONNEES

mr_data <- read.table(file = "D:/PROJET/Article/Merging/Data/CDT_Data/Obs/Tmindata2.txt",header = T,sep = "\t",na.strings = "-99")[-c(1:3),]#%>%
  transform(DATE=as.Date(paste(substr.mod(x = ID,start = 1,stop = 4),
                               substr.mod(x = ID,start = 5,stop = 6),
                               substr.mod(x = ID,start = 7,stop = 8),sep = "-")))%>%
  select(c(DATE,colnames(.)[-c(1,11)]))%>%
  filter(DATE>= as.Date("1981-01-01"))%>%
  transform(DATE=as.character(DATE))

## CREATION ET ALIMENTATION DE BASE DE DONNES
###CREATION
CLIM_DB <- dbConnect(drv = RSQLite::SQLite(),"./Data/Station_Pluvio/Rain_DataBase.sqlite")

### ALIMENTATION

dbWriteTable(conn = CLIM_DB,name = "merge_data",value = mr_data,
             field.types=c(DATE="DATE"))

yy <- dbReadTable(conn = CLIM_DB,name = "synoptic_rain")
