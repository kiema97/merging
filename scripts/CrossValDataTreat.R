setwd("D:/PROJET/Article/Merging/Resultats/CDT/CrossValidation_Precip_Data_1Jan1981_31Dec2017/tmp")
files_names <- dir(pattern = ".txt")
cros_val_data <- rbind()
for (i in files_names ){
  dframe <- read.table(file = i,header = FALSE,sep = ",")
  cros_val_data <- rbind(cros_val_data,dframe)
}
dframe2 <- read.table(file = "file:///D:/PROJET/Article/Merging/Resultats/CDT/CrossValidation_Precip_Data_1Jan1981_31Dec2017/STATIONS_DATA.csv",header = TRUE,sep = ",")
names(cros_val_data) <- c("DATE",colnames(dframe2)[-1]) 

setwd("D:/PROJET/Article/Merging")
CLIMATE_DB <- dbConnect(drv = RSQLite::SQLite(),"D:/PROJET/Article/Merging/Data/Station_Pluvio/Rain_DataBase.sqlite")
