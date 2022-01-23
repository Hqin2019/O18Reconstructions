# install.packages() and load the package
library(ggplot2)

#load Eigenvalues data
load(file="data/Eigenvalues.RData")

eig=(eigs)^2/45          
par(mar=c(4,4.5,1.5,4.5))      #graphic size
modn=1:45
plot(modn,100*eig/sum(eig), type='o',lwd=2.5,col='red', 
     xlab="", ylab="", main="Eigenvalues of covariance matrix",cex.axis=1, cex.lab=1.5)
mtext("Variance [%]",side=2,line=2.5, cex=1.5, col="red")
mtext("Mode Number",side=1,line=2.5, cex=1.5, col="black")
axis(2, col="red", col.ticks="red", col.axis="red", cex.axis=1)
par(new=TRUE)
varexp=100*cumsum(eig)/sum(eig)
plot(modn,varexp,type="o",col="blue",
     ylim=c(0,100),
     lwd=2.5,axes=FALSE,xlab="",ylab="", cex.axis=1.5)
axis(4, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1)
mtext("Cumulative Variance [%]",side=4,line=2.5, cex=1.5, col="blue")
