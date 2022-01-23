# install.packages() and load the package
library(ggplot2)
library(ggmap)

#load EOFs data
load(file="data/EOFs.RData")
load(file="data/RSdata.RData")

eofr=cbind(mod_rm[,2:3], EOFs)
eofm4=eofr[,1:6] 
colnames(eofm4) <- c("Lon", "Lat", "E1","E2","E3","E4")
eofm4f=data.frame(eofm4)

# Map 
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
#rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="satellite", crop=TRUE)

#Plot EOFs
for(i in 1:4){
  scale=eofm4f[,i+2]
  p<-ggmap(myMap) + geom_point(data=eofm4f, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
    ggtitle(paste("EOF",i, sep="")) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.5, "cm"))+ 
    labs(x="Longitude", y="Latitude")
  print(p)
}
