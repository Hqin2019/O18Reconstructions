# install.packages() and load the package
library(ggplot2)
library(ggmap)

#load EOFs data
load(file="data/PCs.RData")

#Plot PCs
#Plot the first three PCs
#first Three PCs in one plot
par(mar=c(4,4,3,4))
time1=seq(1997,2011, len=45)
PC1=PCs[,1]
PC2=PCs[,2]
PC3=PCs[,3]
PC4=PCs[,4]
plot(time1, PC1, type="l", col="black",
     main="The First Four Principal Components",
     xlab="", ylab="",
     ylim=c(-0.5,0.5), lwd=3.0,cex.axis=1,cex.lab=1.2) +
  title(ylab="Scale", xlab="Year",line=2.5, cex.lab=1.2)
lines(time1,PC2,col="red", type="l", lty=2, lwd=3.0 ) +   
  lines(time1,PC3,col="blue",lwd=3.0, type="l", lty=3) +
  lines(time1,PC4,col="green",lwd=3.0, type="l",lty=4)
legend(1998,-0.35, legend=c("PC1"),
       col=c("black"), lty=1, bty = "n",text.font =6,lwd=3.0,
       cex = 1) 
legend(2001,-0.35, legend=c("PC2"),
       col=c("red"), lty=2, bty = "n",text.font =6,lwd=3.0,
       cex = 1) 
legend(2004,-0.35, legend=c("PC3"),
       col=c("blue"), lty=3, bty = "n",text.font =6,lwd=3.0,
       cex = 1)
legend(2007,-0.35, legend=c("PC4"),
       col=c("green"), lty=4, bty = "n",text.font =6,lwd=3.0,
       cex = 1)
