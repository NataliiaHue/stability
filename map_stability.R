library(sp)
library(maps)
library(mapdata)
library(maptools)
library(scales)  #for transparency

############# import data for points and text #############
languages<-read.csv("languages_map.csv",sep=";")

pdf(file="languages-map.pdf", width = 11, height=8)

#############  plot empty basic map ############# 
par(mar=c(0,0,0,0))
long_lim <- c(20, 170)
lat_lim  <- c(10, 90)
map(xlim=long_lim, ylim=lat_lim, col="#F2F2F2", asp=1, fill=TRUE)
#plot(world, xlim=long_lim, ylim=lat_lim, col="#F2F2F2", asp=1, fill=TRUE)
map.axes()

############ plot points ############

#Turkic
points(languages$longitude[languages$family=='Turkic'],languages$latitude[languages$family=='Turkic'],col=alpha("#D73027",0.6),pch=19,cex=7)
#Mongolic
points(languages$longitude[languages$family=='Mongolic'],languages$latitude[languages$family=='Mongolic'],col=alpha("#7570B3",0.6),pch=19,cex=7)
#Tungusic
points(languages$longitude[languages$family=='Tungusic'],languages$latitude[languages$family=='Tungusic'],col=alpha("#7FBC41",0.6),pch=19,cex=7)
#Korean
points(languages$longitude[languages$family=='Koreanic'],languages$latitude[languages$family=='Koreanic'],col=alpha("#E6AB02",0.6),pch=19,cex=7)
#Japonic
points(languages$longitude[languages$family=='Japonic'],languages$latitude[languages$family=='Japonic'],col=alpha("#E7298A",0.6),pch=19,cex=7)

############ plot text ############

text(languages$longitude,languages$latitude,
     labels = languages$short_name, col='#FFFFFF', cex=0.7, font = 2)

############ plot legend ############

legend(x=150,y=30,c("Turkic", "Mongolic", "Tungusic","Japonic", "Koreanic"),
       col=c("#D73027","#7570B3","#1B9E77","#E7298A","#E6AB02"),
       pch=16,cex=0.8,pt.cex=1.4)
dev.off()
