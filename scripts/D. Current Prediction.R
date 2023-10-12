######################################################################
###   Project Spp distributions: current 
###   Anna Drake  Dec 9, 2022
#######################################################################
## (1) Bring in current climate normals 
#######################################################################

# Load packages  ____________________________________________________
require(foreach)
require(raster)
require(gbm)
require(ggplot2)
require(matrixStats)
library(stringr)
require(data.table)
require(parallel)

# add function____________________________________________________
`%notin%` <- Negate(`%in%`)


# set directory____________________________________________________
dir<-'C:/Users//andrake/Desktop/Drake_YK_Refugia/'
#dir<-'/Users/annadrake/Desktop/Yukon Project Data/qpad-offsets-main/'
setwd(dir)

### Get species list____________________________________________________           

Shrinkage<-read.csv("Shrinkage.csv")
SpList<-Shrinkage$Sp  #78 species

##### Optimal Trees____________________________________________________

Ntrees<-read.csv("Ntrees.csv")
Ntrees<-Ntrees$x


######################################################################
## (2) Bring in Ecozone conditions for 2071, scenarios 2.6,4.5,7.0
######################################################################

setwd(paste(dir,"/BRT_Output/FutureRasters/", sep=""))

EcozoneFutureList<-list.files(pattern="2071*")
EcozoneFuture<-list()
for (m in 1:length(EcozoneFutureList)) {
  EcozoneFuture[[m]]<-setDT(readRDS(EcozoneFutureList[[m]]))
}

#gbm.fit order confirm
# Column order *has to be: 
#"bFFP","CMD","eFFP","FFP","MAT","MCMT","MWMT","NFFD","PPT_sm","PPT_wt","RH","tpi","eastness","northness","tri","Thaw","year","Cat" 
# OrderedDat<-Data[c(3:5,7:14,18:21,15:17)]

setwd(paste(dir,"BRT_output/NoBarrier",sep=""))


#### Get each species in turn ____________________________________________________

for (i in c(1:77)){
  spfile<-list.files(pattern=SpList[i], all.files=TRUE, full.names=FALSE)
  spfile<-spfile[-grep(".rds", spfile, fixed=T)] #drop influence file
  load(spfile) #get set of 50 GBM models
  
  ## Predict for each GCM and Period____________________________________________________
  
  for (m in 1:length(EcozoneFutureList)) {
    
    OrderedDat<-EcozoneFuture[[m]][,c(3:5,7:14,18:21,15:17)] #Ensure correct order for predict
    
    DensOut<-sapply(GBM, function(x){exp(predict.gbm(object=x, type="link", 
                                                     newdata=OrderedDat, n.trees = Ntrees[i],na.rm=TRUE))}) 
    
    DensSum<-cbind(round(rowMeans(DensOut),4), round(rowQuantiles(DensOut,probs=0.95),4),round(rowQuantiles(DensOut,probs=0.05),4))
    
    Output<-cbind(EcozoneFuture[[1]][,c(1:2)],DensSum)
    names(Output)[c(1:5)]<-c("X","Y","Mean","Upper90_CI","Lower90_CI")
    
    saveRDS(Output,paste("W:/EDM/CCVegMod4/Drake_YKRefugiaOutput/FutureRasters_1991Norm/",SpList[i],EcozoneFutureList[m], sep=""), compress=TRUE)
    
  } # end of GCM/Scenarios
} # end of species prediction



######################### END OF CODE #############################

#Parallel coding but not using while gone in case causes Present core use to fail


### register cores
#detectCores() #16 - use half between 2 processes so 4 each
#cl <- makeCluster(4)
#registerDoParallel(cl)

#Split <- sort(rank(1:nrow(EcozoneFuture[[1]])) %% 5) 

## Predict for each GCM and Period
#for (m in 1:length(EcozoneFutureList)) {
# test<- EcozoneFuture[[m]][c(1:10),]
#  FutureDensity <- foreach(r = unique(Split),
#                          .combine = rbind,
#                          .packages = c("gbm","matrixStats")) %dopar% {
#                            DensOut<-sapply(GBM, function(x){exp(predict.gbm(object=x, type="link", 
#                                                                                  newdata =test[Split== r,], n.trees = Ntrees[i],na.rm=TRUE))}) 
#                            DensSum<-cbind(round(rowMeans(DensOut),4), round(rowQuantiles(DensOut,probs=0.95),4),round(rowQuantiles(DensOut,probs=0.05),4))
#                            return(DensSum)
#                          }
#Output<-cbind(EcozoneFuture[[1]][,c(1:2)],FutureDensity)
#names(Output)[c(1:5)]<-c("X","Y","Mean","Upper90_CI","Lower90_CI")
#saveRDS(Output,paste("SppFuture/",SpList[i],EcozoneFutureList[m],".rds", sep="", compress=TRUE))
#} # end of GCM/Scenarios
#} # end of species prediction


