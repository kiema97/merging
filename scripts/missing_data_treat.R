# MISE EN EVIDENCE DES DONNEES MANQUANTES
library(ggplot2)
library(reshape2)
library(RColorBrewer)
# Importation des données 
obs_data <- read.table(file = "D:/PROJET/Article/Merging/Data/CDT_Data/Obs/CDT_RAIN_DATA.txt",header = T,sep = "\t",na.strings = "-9999")

obs_data2 <- obs_data[-c(1,2,3),]%>%
  subset(DATE>=19810101&DATE<=20171231,select=c(69:78))

obs_data3 <- rbind(obs_data[c(1,2,3),c(69:78)],obs_data2)
#Modification de fonction
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
obs_data2$DATE <- substr.mod(x = obs_data2$DATE,start = 1,stop = 4)
#Calcul de données manquante
missing.data <- aggregate.data.frame(x = obs_data2[,-1],by = list(obs_data2$DATE),FUN = function(x){
  miss.data <- round(sum(is.na(x))/length(x)*100,2)
})%>%melt(id.vars="Group.1")
colnames(missing.data) <- c("Year","Station","Value")

# Représentation des données manquantes
attach(missing.data2)
RColorBrewer::brewer.pal(n = 9,name = "Greys")
RColorBrewer::display.brewer.all()
missing.data2 <- transform(missing.data,Station=as.numeric(missing.data$Station))

ggplot(missing.data2, aes(Station,Year),group=Year) +
  geom_raster(aes(fill =Value))+
  scale_fill_gradient(low = "#969696",
                      high = "#000000")+
  geom_hline(yintercept =seq(1981,2017,1),size=1,col="white" )+
  geom_vline(xintercept = seq(1,77,1),size=1,col="white")+
  #coord_equal(ratio = 1,expand = T,xlim = c(0,78),ylim = c(0,38))+
  #scale_x_discrete(breaks=seq(1,77,10),labels=unique(Station)[seq(1,77,10)])+
  #scale_y_discrete(breaks=unique(Year)[seq(1,37,2)],labels=unique(Year)[seq(1,37,2)])+
  theme_bw()+
  xlab("Stations")+
  ylab("Années")+
theme(axis.text.x =element_text(angle = 90,colour = "black",size = 6),
        axis.text.y = element_text(colour = "black",size = 8),
        axis.title.x = element_text(face = "bold",colour = "black",size = 10),
        axis.title.y = element_text(face = "bold",colour = "black",size = 10),
        legend.text.align =0,#
        legend.margin = margin(-18,0,0,-6),
        legend.key.height = unit(0.8,"cm"),
        legend.title = element_text(face = "bold",colour = "black",hjust = 0.5,vjust = 0.5),
        legend.text = element_text(face = "bold",colour = "black"))+
  guides(fill = guide_legend(title="(%)",reverse = T,title.hjust = 0))
