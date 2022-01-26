# install.packages() and load the package
library(ggplot2)
library(matrixStats)
library(ggmap)
library(animation)
library(reshape2)

#load observation, satellite remote-sensing and EOF modes data
load(file="data/OBdata.RData")
load(file="data/RSdata.RData")
load(file="data/EOFs.RData")

#Compute standardized remote-sensing data
mod_o18<- mod_rm[, 4:48]
clim_mod<- rowMeans(as.matrix(mod_o18), na.rm=TRUE)    
sd_mod<- rowSds(as.matrix(mod_o18), na.rm=TRUE)   
mod_std<- (mod_o18 - clim_mod) / sd_mod

# load map 
#boundary coordinates of Tibetan Plateau
myLocation <- c(65, 25, 105, 45)
maptype = c("roadmap", "terrain", "satellite", "hybrid")
myMap = get_map(location = myLocation, source="google", maptype="satellite", crop=TRUE)

# compute standardized observation data using mean and standard deviation of remote-sensing data
dato_O18<- dato[,5:31]
f<- data.frame(cbind(mod_rm[, 1], clim_mod, sd_mod))
climo<- f[f$V1 %in% dato[,2], ][,2]            
sdo<- f[f$V1 %in% dato[,2], ][,3]
dato_O18_std<- (dato_O18 - climo)/sdo
dato_std<- data.frame(dato[, 1:4], dato_O18_std)   
colnames(dato_std)<- colnames(dato)

# check number of stations with values
n_mode=c()
for (i in 5:31) {
  v=which(complete.cases(dato_std[,i])) 
  n_mode[i-4]=length(v)-1 
}
n_mode

# create data frame with longitude, latitude and correpsonding EOFs data
eofr=cbind(mod_rm[,2:3], EOFs)
# extract first three EOF modes
eofm3=eofr[,1:5] 
colnames(eofm3) <- c("Lon", "Lat", "E1","E2","E3")
eofm3f=data.frame(eofm3)

# reconstruction O18 data
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

# put grid ID, lat and lon as the first three columns
recon <- data.frame(recon)
recon[,1:3] <- mod_rm[,1:3]
# create column names  for remaining columns
jja=rep(c("Jun","Jul","Aug"),9)
yr2=rep(1997:2005,each=3)
hdjja1=paste(jja,yr2)
colnames(recon)<-c("BoxID","Lon","Lat", hdjja1)

# reconstruction data without standardizing
gridout1<-recon
gridout1ori_O18<- gridout1[, 4:30]*sd_mod + clim_mod
gridout1[, 4:30]<- gridout1ori_O18

# ensure the background color is white
par(bg = "white") 
# clear history before recording
ani.record(reset = TRUE) 
# set up an empty frame, then add points one by one
plot.new()
# plot the reconstruction data and record them
for (i in 4:30) {
  scale <- gridout1[,i]
  p<-ggmap(myMap) + geom_point(data=gridout1, mapping=aes(x=Lon, y=Lat, colour=scale), size=2) +
    scale_colour_gradient2(limits=c(-55,55),low="blue", mid="white", midpoint=0, high = "red", space="rgb") +
    ggtitle(paste("Summer Reconstructed O18 in",colnames(gridout1[i]))) +
    theme(plot.title = element_text(hjust = 0.5),legend.key.height = unit(1, "cm"), legend.key.width = unit(0.5, "cm"))+ 
    labs(x="Longitude", y="Latitude") +
    labs(col="Units")
  print(p)
  ani.record()
}

# replay it, with an appropriate pause between frames:
oopts = ani.options(interval = 0.5, 
                    ani.width=450, 
                    ani.height=550,
                    title="Summer O18 Reconstructed Data Animation"
)

#Animate the frames in the plot window of R Studio
ani.replay() 

#Show the animation on an HTML page
saveHTML(ani.replay(), img.name = "O18SummerRecon_animation")
