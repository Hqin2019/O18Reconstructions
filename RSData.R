# install.packages() and load the package
library(matrixStats)

# load remote-sensing data
load(file="data/RSdata.RData")

mod_o18<- mod_rm[, 4:48]
#compute mean of remote-sensing data
clim_mod<- rowMeans(as.matrix(mod_o18), na.rm=TRUE)  
#compute standard deviation of remote-sensing data
sd_mod<- rowSds(as.matrix(mod_o18), na.rm=TRUE)  
#compute standardized anomalies of remote-sensing data
mod_std<- (mod_o18 - clim_mod) / sd_mod

#apply svd to standardized anomalies of remote-sensing data
jjasvd<-svd(mod_std)
#get eigenvalues
eigs<-jjasvd$d
#get EOFs
EOFs<-jjasvd$u
#get PCs
PCs<-jjasvd$v
