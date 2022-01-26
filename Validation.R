# install.packages() and load the package
library(ggplot2)
library(reshape2)

# load observation and reconstruction data
load(file="data/OBdata.RData")
load(file="data/Reconstruction.RData")

# get station name and corresponding grid id
stn_name<-dato[,1]
grid_id=dato[,2]

# create time sequence from 1997 to 2005
t1=seq(1997,2006,len=27)
# set figure with 4 rows and 4 columns
par(mfrow = c(3, 3))  
par(mgp=c(2,1,0))
par(mar=c(3,3,2,3))
# plot validation figure including reconstruction data and observation data
for (i in 1:9) { 
  plot(t1, dato[i,5:31],type="o", ylim=c(-50,10),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste(stn_name[i],",", "Grid ID", grid_id[i]))
  legend(1995, 56,  col=c("black"),lwd=2.0, lty=1,
         legend=c("Station data"),
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
  lines(t1, gridout1[grid_id[i], 4:30], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2.0)
  legend(1995, 52,  col=c("blue"),lwd=2.0, lty=1,
         legend=c("Reconstructed data"),text.col = "blue",
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
}


for (i in 10:16) { 
  plot(t1, dato[i,5:31],type="o", ylim=c(-50,10),
       xlab="",ylab="",
       cex.axis=1.5,cex.lab=1.5,
       main = paste(stn_name[i],",", "Grid ID", grid_id[i]))
  legend(1995, 56,  col=c("black"),lwd=2.0, lty=1,
         legend=c("Station data"),
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
  lines(t1, gridout1[grid_id[i], 4:30], col="blue") 
  text(1998,-15, paste("(",letters[i],")"), cex=2.0)
  legend(1995, 52,  col=c("blue"),lwd=2.0, lty=1,
         legend=c("Reconstructed data"),text.col = "blue",
         bty="n",text.font=2.0,cex=1.0, seg.len = 0.8) 
}
