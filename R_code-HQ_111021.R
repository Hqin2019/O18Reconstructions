rm(list=ls(all=TRUE)) #Clear the environment.
#set up the working file.
library(RColorBrewer)
library(zoo)
library(dplyr)
library(reshape2)
library(R.matlab)
library(readxl)
library(lubridate)
library(ggmap)
library(matrixStats)

#Read model data
mod_dt <- read_xls("data_O18.xls")

#Extract summer months in model data 
#Work on the column names
mod_dt_summer <- as.data.frame(matrix(1,ncol=48,nrow=9009))
mod_dt_summer[ , 1:3] <- mod_dt[,1:3]
mod_dt_summer[ , seq(4,46, by =3 )] <- mod_dt[grepl("Jun", names(mod_dt))]
mod_dt_summer[ , seq(5,47, by =3 )] <- mod_dt[grepl("Jul", names(mod_dt))]
mod_dt_summer[ , seq(6,48, by =3 )] <- mod_dt[grepl("Aug", names(mod_dt))]
names(mod_dt_summer)[1:3] <- c("BoxID", "Lon", "Lat")
names(mod_dt_summer)[4:48] <- paste(rep(1997:2011, each = 3), rep(c("Jun", "Jul", "Aug"), 15))
mod_data <- sapply(mod_dt_summer, as.numeric)
mod_data[mod_data == -1000] <- NA
col_names_mod<- colnames(mod_data)
#write.csv(mod_data,"mod_data_summer.csv")

#Project to the desired area
#Read the 2016-09-24 reconstructed data to get lat and lon coordinates
tpdat1=read.csv("reconout160924r.csv", header=TRUE)
tpdatlatlon=tpdat1[,1:3] #909 obs. of 2 variables
colnames(tpdatlatlon) <- c("BoxID","Lon","Lat")
tpdat=data.frame(tpdatlatlon)

#Extract the corresponding rows from the model data to match the Lon-Lat of interested.
mod_data<- data.frame(mod_data)
colnames(mod_data)<- col_names_mod
mod_data1<- merge(mod_data[, -1], tpdat)# 909 obs. of 48 variables
attach(mod_data1)
mod_data2<- mod_data1[order(BoxID), ]
detach(mod_data1)
row.names(mod_data2)<- NULL
mod_data2 <- mod_data2 %>% relocate(BoxID, .before = Lon)
#mod_data2 909 obs. of 48 variables with the same BoxID as the previous model data.
names(mod_data2)
#"BoxID" "Lon" "Lat" "1997 Jun" ...

#Remove NA and leave the data with values.
#which(is.na(mod_data2))
#apply(is.na(mod_data2), 2, which) 
mod_rm<- na.omit(mod_data2) # 856 obs. of 48 varaibles
dim(mod_rm)
#[1] 856  48

mod_o18<- mod_rm[, 4:48]# only O18 values, 845 obs.of 45 variables
dim(mod_o18)
#[1] 856  45

#Compute standardized anomalies
clim_mod<- rowMeans(as.matrix(mod_o18), na.rm=TRUE)
length(clim_mod)
#[1] 856
sd_mod<- rowSds(as.matrix(mod_o18), na.rm=TRUE)
length(sd_mod)
#[1] 856
mod_std<- (mod_o18 - clim_mod) / sd_mod
dim(mod_std)
#[1] 856  45
timave1<- rowMeans(mod_std)
sum(round(timave1)==0)
#856 zeros
#Plot the model data anomalies used for generating EOFs
#For July 1997
library(ggplot2)
library(ggmap)
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
tp=ggmap(myMap)
ggmap(myMap)

#get lat and lon coordinates
tpdatlatlon=mod_rm[,2:3]
colnames(tpdatlatlon) <- c("Lon", "Lat")
tpdat=data.frame(tpdatlatlon)
moddf=data.frame(mod_std)
#Plot the July 1997 model data (anomalies)
O18=moddf$X1997.Jul
ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-3.5,3.5),low="blue",mid="white", 
                         midpoint=0, high = "red", space="rgb")+
  ggtitle("Model O18 Anomalies: July 1997") +
  theme(plot.title = element_text(hjust = 0.5))

#Compute EOF using SVD
jjasvd=svd(mod_std)
eig=(jjasvd$d)^2/45
eig4=sum(eig[1:4])/sum(eig)
eig4
#[1] 0.8328875
eig3=sum(eig[1:3])/sum(eig)
eig3
#[1] 0.7928953

#Plot the eigenvalues vs mode number
par(mar=c(4,4.5,0.5,4.5))
modn=1:45
plot(modn,100*eig/sum(eig), type='o',lwd=2.5,col='red', 
     xlab="", ylab="",cex.axis=1.5, cex.lab=1.5)
mtext("Eigenvalues [%]",side=2,line=3, cex=1.5, col="red")
mtext("EOF Mode Number",side=1,line=3, cex=1.5, col="black")
axis(2, col="red", col.ticks="red", col.axis="red", cex.axis=1.5)
par(new=TRUE)
varexp=100*cumsum(eig)/sum(eig)
plot(modn,varexp,type="o",col="blue",
     ylim=c(0,100),
     lwd=2.5,axes=FALSE,xlab="",ylab="", cex.axis=1.5)
axis(4, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1.5)
mtext("Percent Cumulative Eigenvalue [%]",side=4,line=3, cex=1.5, col="blue")
dev.off

# SVD EOF
#Mark lat and lon data as the first two columns of the EOF data 
eofm=jjasvd$u #EOF vectors
dim(eofm)
#[1] 856  45
eofr=cbind(mod_rm[,2:3], eofm)
dim(eofr)
#[1] 856  47 # 47 columns=lat, lon, plus 45 EOF vectors
#Save EOFs
colnames(eofm, do.NULL = FALSE)
eofm3=eofr[, 1:5] # 6
eofm4=eofr[,1:6] # 8 
#eofm5=eofr[,1:7] # 5
#eofm6=eofr[,1:8] # 4
#eofm7=eofr[,1:9] # 7
colnames(eofm3)<- c("Lon", "Lat", "E1","E2","E3")
colnames(eofm4) <- c("Lon", "Lat", "E1","E2","E3","E4")
#colnames(eofm5) <- c("Lon", "Lat", "E1","E2","E3","E4","E5")
#colnames(eofm6) <- c("Lon", "Lat", "E1","E2","E3","E4","E5","E6")
#colnames(eofm7) <- c("Lon", "Lat", "E1","E2","E3","E4","E5","E6","E7")
eofm3f=data.frame(eofm3)
eofm4f=data.frame(eofm4)
#eofm5f=data.frame(eofm5)
#eofm6f=data.frame(eofm6)
#eofm7f=data.frame(eofm7)


#Plot the EOFs
#rm(list=ls())
library(ggplot2)
library(ggmap)
#sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
#lon-lat of lowerleft and lon-lat of upperright
#maptype = c("terrain", "toner", "watercolor")
maptype = c("roadmap", "terrain", "satellite", "hybrid")
rm(list=c("myMap"))
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
tp=ggmap(myMap)
ggmap(myMap)
#Read the EOF data for TP
tpdat=data.frame(eofm4)
#plot the first four EOFs and save the figures
setwd("~/Documents/R_Work/O18Reconstructions/EOFs")
for(i in 1:3){
  scale=tpdat[,i+2]
  #ggplot of the first six EOFs
  p<- ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=scale), size=2.5) +
    scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
    ggtitle(paste("EOF",i, sep="")) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.5, "cm"))+ 
    labs(x="Longitude", y="Latitude")
  png(paste("Pattern of EOF",i, ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Plot EOF2
scale=tpdat$E2
ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb")+
  ggtitle("EOF2") +
  theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.7, "cm"), legend.key.width = unit(0.5, "cm"))
#Plot EOF3
O18=tpdat$E3
ggmap(myMap) + geom_point(data=tpdat, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-0.11,0.11),low="blue", mid="white", midpoint=0, high = "red", space="rgb")+
  ggtitle("EOF3") +
  theme(plot.title = element_text(hjust = 0.5))

#Plot PCs
#Plot the first three PCs
setwd("~/Documents/R_Work/O18Reconstructions/PCs")
time1 = seq(1997,2011, len = 45)
dev.off()
for(i in 1:3){
  pc = jjasvd$v[,i]
  png(paste("PC", i, ".png", sep = ""), width = 700, height = 400, res = 120)
  p<-  plot(time1, pc, type = "o", 
            main = paste("Principal Component #",i, sep = ""),
            xlab = "Time", ylab = "PC values",
            ylim = c(-0.45,0.4), lwd = 1.5)
  
  print(p)
  dev.off()
}


time1=seq(1997,2011, len=45)
pc1=jjasvd$v[,1]
plot(time1, pc1, type="o", 
     main="Principal Component #1", 
     xlab="Time", ylab="PC values", ylim=c(-0.45,0.4), lwd=1.5)

pc2=jjasvd$v[,2]
plot(time1, pc2, type="o", col="red",
     main="Principal Component #2", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.4,0.4), lwd=1.5)

pc3=jjasvd$v[,3]
plot(time1, pc3, type="o", col="blue",
     main="Principal Component #3", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.45,0.40), lwd=1.5)

#first Three PCs in one plot
time1=seq(1997,2011, len=45)
pc1=jjasvd$v[,1]
pc2=jjasvd$v[,2]
pc3=jjasvd$v[,3]
plot(time1, -pc1, type="l", col="black",
     main="The First Three Principal Components", 
     xlab="Time", ylab="PC values",
     ylim=c(-0.8,0.4), lwd=2.0)
lines(time1,pc2,col="red", type="l", lty=2, lwd=2.0 )
lines(time1,pc3,col="blue",lwd=2.0,type="l", lty=3)
legend("bottomleft", legend=c("pc1", "pc2", "pc3"),
       col=c("black", "red", "blue"), lty=1:3, bty = "n",text.font = 1,
       cex = 0.4)

#Read observation data
setwd("~/Documents/R_Work/O18Reconstructions")
dato=read.csv("cleaned_original.csv",header=TRUE)
#remove Yungcun row 9
dato<- dato[-9, ]
rownames(dato)<- NULL
colnames(dato)[2:4]<- c("BoxID", "Lat", "Lon")
colnames(dato)[5:37]<- colnames(mod_rm)[4:36]
IDloc<- dato[, 1:4]#Station, BoxID, Lat, Lon
stnyr=dim(dato)
stnyr
#[1] 16 37 
#16 stations: remove Bomi(no points) and Yungcun
#First 4 columns: StationName, BoxID, Lat, Lon
#column 5:37. JJA from 1997-2007: 3 months for 11 years = 33 columns
#For this study, we consider data of JJA from 1997-2005 (column 5:31): 3 months
#for 9 months = 27 columns
dato_O18<- dato[, 5:31]
dim(dato_O18)
#[1] 16 27
sum(!is.na(dato_O18))
#[1] 180 nonNA values

# compute standardized observation data using mean and standard deviation of remote-sensing data
f<- data.frame(cbind(mod_rm[, 1], clim_mod, sd_mod))
climo<- f[f$V1 %in% dato[,2], ][,2]            
sdo<- f[f$V1 %in% dato[,2], ][,3]
dato_O18_std<- (dato_O18 - climo)/sdo
dato_std<- data.frame(dato[, 1:4], dato_O18_std)
dim(dato_std)# 16x31
colnames(dato_std)<- colnames(dato[, 1:31])

#Three-mode reconstruction, from June 1997-Aug 2005. There are 30 columns:27 months+BoxID+Lon+Lat
recon=matrix(0,nrow=856,ncol=30)
for (i in c(5:31)) {y=complete.cases(dato_std[,i])
v=which(y)
u=dato_std[v,2]
# choose observation data as response
datr=dato_std[v,i]
# choose first three EOF modes data as predictors
eofr=eofm3[u,c("E1","E2","E3")]
df=data.frame(eofr,datr)
# fit multiple linear regression
reg=lm(formula=datr~E1+E2+E3, data=df)
# get corresponding estimate coefficients
coe=reg$coefficients
c1=rep(1,856)
res=cbind(c1,eofm3[,c("E1","E2","E3")])
# reconstruct data by multiplying estimate coefficients with first three EOF modes data
recon[,i-1]=data.matrix(res)%*%coe
}

#Put grid ID, lat and lon as the first three columns
recon<- data.frame(recon)
recon[,1:3] = mod_rm[,1:3]
#recon3[,1]=mod_rm[,3]
#recon3[,2]=mod_rm[,1]
#recon3[,3]=mod_rm[,2]
#Put proper header
jja=rep(c("Jun","Jul","Aug"),9)
yr2=rep(1997:2005,each=3)
hdjja1=paste(jja,yr2)
colnames(recon)<-c("BoxID","Lon","Lat", hdjja1)

write.csv(recon,file="~/Documents/R_Work/O18Reconstructions/reconout.csv")

#plot the results: space-time averages
#gridout2=read.csv("C:/Users/hniqd/Documents/O18Reconstructions/reconout.csv",header=TRUE)
#dim(gridout2)
#[1] 856  31 #The first column is the grid ID
#timeave2=rowMeans(gridout2[,5:31]) #Time ave 
#areaw2=cos((pi/180)*gridout2[,4])/sum(cos((pi/180)*gridout2[,4]))
#Show area weight variation wrt lat, close to be uniform
#plot(areaw2, ylim=c(1/856-0.0002,1/856+0.0002))
#wtgrid2=areaw2*gridout2[,5:31] #Area weighted data
#spaceave2=colSums(wtgrid2) #Spatial ave
#write.csv(spaceave2,file="C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/spaceave2mix.csv")
#write.csv(timeave2,file="C:/Users/hniqd/OneDrive/Documents/TP2020-07-03YaoChen/2017-10-28Computing/timeave2mix.csv")

#Plot space and time average
#length(spaceave2)
#[1] 27
#length(timeave2)
#[1] 856
#plot(seq(1997,2005,len=27),spaceave2,type="o", ylim=c(-25,30), 
#     main="Spatial Average O18 Anomalies",
#     xlab="Time: June, July, August of each year", 
#     ylab="TP spatial average O18", lwd=1.5)

##Large values for June 2006 (-24.21), July 2000 (-7.68), and Aug 1999 (-7.11) 
#which(recon[, 4:33] == max(recon[, 4:33]), arr.ind = TRUE)
#Max=56.66534 is in Jun 2006, recon[297,31]=56.67
#Min=-125.2796 is in Jun 2006, recon[711, 31]=-125.28


#Plot the data for a few months to check their ranges
#gridid=1:856
#plot(gridid, recon[,31],type="l", ylim=c(-150,100))
#lines(gridid,recon[,30],type="l", col="red")
#lines(gridid,recon[,32],type="l", col="blue")

#plot(seq(1,856),timeave2,type="l", ylim=c(-5,2),
#     main="Temporal Average O18 Anomalies over Tibetan Plateau: 3-Mode Reconstruction",
#     xlab="Grid box ID from 1 to 856", 
#     ylab="TP 1997-2006 average O18", lwd=1.5)

#test: plot reconstruction result (anomalies)
#tp=ggmap(myMap)
rm(list=c("myMap"))
myLocation <- c(60, 25, 110, 45)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = myLocation, source="google", maptype="terrain", crop=TRUE)
ggmap(myMap) + labs(x="Longitude", y="Latitude")
tpdatlatlon=data.frame(recon)
i=7
O18=pmax(pmin(recon[,i],10),-10) 
ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=O18), size=2) +
  scale_colour_gradient2(limits=c(-15,15),low="blue",mid="white", 
                         midpoint=0, high = "red", space="rgb")+
  ggtitle(paste("Model O18 Anomalies:", hdjja1[i-3])) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Longitude", y="Latitude")

#Plot the reconstruction data and save the figures in a folder
setwd("~/Documents/R_Work/O18Reconstructions/ReconFigs")
tpdatlatlon=data.frame(recon)
for(i in 4:30){
  scale=pmax(pmin(recon[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-15,15),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Reconstructed O18 Anomalies:", hdjja1[i-3])) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm"))+
    labs(x="Longitude", y="Latitude")
  png(paste("Reconstructed O18 Anomalies",hdjja1[i-3], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#ggplot
library(ggpubr)
library(gridExtra)
plot.list<- list()
for(i in 4:30){
  scale=pmax(pmin(recon[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-15,15),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(hdjja1[i-3]) 
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm"))+
    labs(x="Longitude", y="Latitude")
  plot.list[[i-3]]= p
}

plot<-ggarrange(plotlist=plot.list, ncol=5, nrow=6)
annotate_figure(plot, top = text_grob("Reconstructed O18", color = "black", face = "bold", size = 14))



#Plot the observed data and save the figures in a folder
setwd("~/Documents/R_Work/O18Reconstructions/ObservedFigs")
rm(list=c("myMap"))
#myLocation <- c(10, 20, 115, 50)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = c(60, 25, 110, 45), source="google", maptype="terrain", crop=TRUE)
ggmap(myMap)
tpdatlatlon=data.frame(dato)
for(i in 5:ncol(dato)){
  scale=pmax(pmin(dato[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-27,5),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Observed O18", hdjja1[i-4])) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm")) + 
    labs(x="Longitude", y="Latitude")
  png(paste("Observed O18",hdjja1[i-4], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}

#Plot the model data and save the figures in a folder
setwd("~/Documents/R_Work/O18Reconstructions/ModelFigs")
rm(list=c("myMap"))
#myLocation <- c(10, 20, 115, 50)
#maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = c(60, 25, 110, 45), source="google", maptype="terrain", crop=TRUE)
ggmap(myMap)
tpdatlatlon =data.frame(mod_rm)
jja=rep(c("Jun","Jul","Aug"),15)
yr2=rep(1997:2011,each=3)
hdjja2=paste(jja,yr2)
for(i in 4:48){
  scale=pmax(pmin(tpdatlatlon[,i],10),-10) 
  p<- ggmap(myMap) + geom_point(data=tpdatlatlon, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-15,5),low="blue",mid="white", 
                           midpoint=0, high = "red", space="rgb")+
    ggtitle(paste("Model O18:", hdjja2[i-3])) +
    theme(plot.title = element_text(hjust = 0.5), legend.key.height = unit(0.8, "cm"), legend.key.width = unit(0.5, "cm")) + 
    labs(x="Longitude", y="Latitude")
  png(paste("Model O18",hdjja2[i-3], ".png", sep = ""), width=600, height=400, res=120)
  print(p)
  dev.off()
}


#Validation
setwd("~/Documents/R_Work/O18Reconstructions")
# get station name and corresponding grid id
stn_name<-dato[,1]
grid_id=dato[,2]
gridout1<-recon #856x30
#convert the standardized anomaly back
gridout1ori_O18<- gridout1[, 4:30]*sd_mod + clim_mod
gridout1[, 4:30]<- gridout1ori_O18
id_index=which(gridout1[,1]%in%grid_id)
# create time sequence from 1997 to 2005
t1=seq(1997,2006,len=27)
# set figure with 4 rows and 4 columns
par(mfrow = c(3, 3))  
par(mgp=c(2,1,0))
par(mar=c(3,3,2,3))
# plot validation figure including reconstruction data and observation data
for (i in 1:9) { 
  plot(t1, dato[i,5:31],type="o", ylim=c(-40,20),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste(stn_name[i],",", "Grid ID", grid_id[i]))
  legend(1995, 56,  col=c("black"),lwd=2.0, lty=1,
         legend=c("Station data"),
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
  lines(t1, gridout1[id_index[i], 4:30], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2.0)
  legend(1995, 52,  col=c("blue"),lwd=2.0, lty=1,
         legend=c("Reconstructed data"),text.col = "blue",
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
}


for (i in 10:15) { 
  plot(t1, dato[i,5:31],type="o", ylim=c(-40,20),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste(stn_name[i],",", "Grid ID", grid_id[i]))
  legend(1995, 56,  col=c("black"),lwd=2.0, lty=1,
         legend=c("Station data"),
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
  lines(t1, gridout1[id_index[i], 4:30], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2.0)
  legend(1995, 52,  col=c("blue"),lwd=2.0, lty=1,
         legend=c("Reconstructed data"),text.col = "blue",
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
}

