# install.packages() and load the package
library(ggplot2)
library(reshape2)
library(matrixStats)

# load observation and reconstruction data
load(file="data/OBdata.RData")
load(file="data/Reconstruction.RData")

# get station name and corresponding grid id
station <- dato[,1]
grid_id <- dato[,2]
dato1=t(dato[5:31])
dato1 <- as.data.frame(dato1)
colnames(dato1)<-station

gridout1_<-t(gridout1[4:30])
id_all<-gridout1[,1]
colnames(gridout1_)<-id_all

# create time sequence from 1997 to 2005
t1=seq(1997,2006,len=27)
# set figure with 4 rows and 4 columns
# plot validation figure including reconstruction data and observation data
for (i in 1:15) { 
  p <- ggplot() + geom_point(aes(t1,dato1[,i]),size=1,shape=1) + geom_line(aes(t1,dato1[,i]),size=0.5) +
    labs(title=paste(station[i],"Grid ID",grid_id[i],sep=" ")) + theme(plot.title = element_text(hjust = 0.5,size=10),
                                  axis.title.y=element_blank(), 
                                  axis.title.x=element_blank(),
                                  axis.text.y = element_text(size = 8))+
    ylim(-40,20) + geom_line(aes(t1,gridout1_[,grid_id[i]]),size=0.5,lty=1,col="blue")+
    annotate("text", x=1997.5, y=-30, label=paste("(",letters[i],")"),size=6)
  print(p)
}
