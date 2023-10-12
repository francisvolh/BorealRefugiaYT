#################################################################
###        E. Climate Velocity and Refugia Assessment         ###
### Modified from Ana Raymundo's and Diana Stralberg's code   ###
###                     Anna Drake Feb 1, 2023                ###
#################################################################

### Load libraries ----------------------------------------------------------

library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible, data.table,
               stringr,tidyverse, terra, yaImpute, scales, gridExtra, sf, ggplot2, scales, raster)
g <- gc(reset = TRUE)
rm(list = ls())

### Functions ---------------------------------------------------------------

fattail <- function(x, alpha, c) {
  left <- (c/(2*alpha*gamma(1/c)))
  right <- exp(-1*((abs(x/alpha))^c))
  result <- left*right
  return(right)  # right side of function provides value of 1 to 0, full result is same shape but returns probability density (1/distance)
}

ftmean <- function(alpha, c) {
  result <-(alpha*gamma(2/c))/gamma(1/c)
  return(result)
}

`%notin%` <- Negate(`%in%`)

### Having a look at the functions
#dist<-data.frame(c(1:80000)) #meters or km...
#dist$totalfun<-fattail(dist$c.1.80000.,8333.3335,0.5) # set to return "result"
#dist$right<-fattail(dist$c.1.80000.,8333.3335,0.5) # set to return "right"

#fattail(50000,8333.3335,0.5)
#fattail(50,8.333,0.5) # returns the same probability

#plot(totalfun~c.1.80000.,dist) # should be probability density
#plot(totalfun/2.967316e-05~c.1.80000.,dist) # rescaled
#points(right,c.1.80000.,dist,col="red") # same as right side of formula


### Load data ---------------------------------------------------------------
dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main'
dirupper<-'/Users//annadrake/Desktop/Yukon Project Data'
setwd(dir)

### Extract key of incomplete co-variate data locations ---------------------------------------------------------------
Norm1991<-readRDS("BRT_output/PresentRasters/Norm1991_EcozoneNormals.rds")
NAKey<-subset(Norm1991,is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,is.na(NAKey$tri))
NAKey<-paste(NAKey$x,NAKey$y,sep=".") #raster squares with incomplete co-variate set
rm("Norm1991")

# Import key of masked AK regions (AK portion of BCR 3 + BCR 2) -------------------------------------
AK_remove<-read.csv("AK_removalregion.csv")
AK_Key<-paste(AK_remove$x,AK_remove$y,sep=".")

ModList<-list.files("./BRT_Output/PresentRasters/SppPresent/") #list Present rasters
Spp<-substr(ModList,1,4)

#### Region shapefiles ---------------------------------------------------------------
crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters
setwd(dirupper)

## Breakdown Region -----------------------------------------------------
BCR40_USACAN <- st_read("CordilleraBreakdown/BCR4.0_USACAN.shp") %>% st_transform(., crs)
BCR41_USACAN <- st_read("CordilleraBreakdown/BCR4.1_USACAN.shp") %>% st_transform(., crs)
BorealCordCAN <- st_read("CordilleraBreakdown/BorealCordilleraCAN.shp") %>% st_transform(., crs)
TaigaCordCAN <- st_read("CordilleraBreakdown/TaigaCordilleraCAN.shp") %>% st_transform(., crs)


######## BOOTSTRAP MEAN ################

############### Run through all species #####################

for (u in c(1:54)){

#Present modeled distribution ---------------------------------------------------------------
 
setwd(paste(dir,"/BRT_Output/PresentRasters/SppPresent/",sep=""))
filePresL<-list.files(pattern=Spp[u])
filePres<-readRDS(filePresL)
filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% NAKey), c("Mean", "UPR90", "LWR90")] <- NA # NA removal
filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% AK_Key), c("Mean", "UPR90", "LWR90")] <- NA #Western AK removal

assign("Present",filePres)

# Future modeled distributions ---------------------------------------------------------------

scenario<-c("_26_2071","_45_2071","_70_2071")
modelVers<-c("ECEarth3_","CNRMESM21_","UKESM10LL_")

setwd(dir)
setwd("./BRT_Output/FutureRasters/SppFuture")
for (k in c(1:3)) {
  for (j in c(1:3)) {
Future<-c(paste(Spp[u],modelVers[k],scenario[j],"*", sep=""))
file<-list.files(pattern=Future, all.files=TRUE, full.names=FALSE)
file<-readRDS(file)
file[which(paste(file$X,file$Y, sep=".") %in% NAKey), c("Mean", "Upper90_CI", "Lower90_CI")] <- NA  #note retaining western AK in future predictions
assign(paste(substr(modelVers[k],1,2),substr(scenario[j],2,3),sep=""),file)
}}

# Put in list ---------------------------------------------------------------
List<-list(Present,CN26,CN45,CN70,EC26,EC45,EC70,UK26,UK45,UK70)
Names<-c("Present","CN26","CN45","CN70","EC26","EC45","EC70","UK26","UK45","UK70")


# Regional Mid-value, top 95% + 75% cumulative population threshold of present abundance ---------------------------------------------------------------

Med<-median(Present$Mean, na.rm=TRUE) #median present density
Max95<-quantile(Present$Mean, 0.95, na.rm=TRUE) #top 95% density
rs <- sort(Present$Mean, decreasing=TRUE)
cum_75 <- min(rs[cumsum(rs) < sum(rs)*0.75])


# Calculate Refugia output ------------------------------------------
# Apply species-specific, core 75% of population as threshold ---------------------------------------------------------------
Thres <-lapply(List, function(x){cbind(x[,c(1:2)],ifelse(x$Mean>=cum_75,1,0))}) 

#Empty raster for stacking -----------------------------------------------
setwd(dir)
Stack<-Blank<-rast("EmptyRegionBase.tiff")

# Velocity metric ---------------------------------------------------------
for (i in c(2:10)){ #run through 9 future RCP/GCMs
tblPres <- Thres[[1]] #present raster
colnames(tblPres)[3] <- 'prev'
tblFut <- Thres[[i]]
colnames(tblFut)[3] <-  'prev'
  
        p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>%
          dplyr::select(pixelID, X, Y, prev)
        f.xy <- mutate(tblFut, pixelID = 1:nrow(tblFut)) %>% 
          dplyr::select(pixelID, X, Y, prev)
    
        p.xy2 <- 
          filter(p.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
        f.xy2 <-
          filter(f.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
        
        if (nrow(f.xy) > 0) { d.ann <- as.data.frame(ann(
          as.matrix(p.xy2[, -1, drop = FALSE]),
          as.matrix(f.xy2[, -1, drop = FALSE]),
          k = 1,
          verbose = F)$knnIndexDist) # returning in meters
        d1b <- as.data.frame(cbind(f.xy2, round(sqrt(d.ann[, 2])/1000))) #change to km
        names(d1b) <- c("ID", "X", "Y", "bvel")
        } else {
          print(spec[i])
        }
        
        f.xy <- as.data.frame(f.xy)
        colnames(f.xy) <- c('ID', 'X', 'Y', 'Pres')
        f.xy <- as_tibble(f.xy)
        d1b <- left_join(f.xy, d1b, by = c('ID', 'X', 'Y'))
        d1b <- mutate(d1b, fat = fattail(bvel, 8.3333, 0.5)) # 8.3333 is appropriate since the scale is km
        sppref <- rast(d1b[, c(2, 3, 6)])
        sppref[is.na(sppref)] <- 0
        Stack <- c(Stack,sppref)
}

Stack<-Stack[[-1]]#get rid of template
mean <- mean(Stack, na.rm = T)  #average all scenarios

# Write out Refugia
setwd(dir)
setwd("./BRT_output/1991_RefugiaRasters/")
writeRaster(mean, paste(Spp[u],"_Refugia_RCPmean.tif",sep=""),overwrite=TRUE)
rm("Stack","d1b","d.ann","f.xy","f.xy2","Thres")

### Get suitability metric from density -------------------------

for (k in 2:length(Names)){
  suit<-rast(get(Names[k])[,c(1:3)])
  suitscale<-suit/Max95  # scaled against present 95% max 
  values(suitscale)[values(suitscale) > 1] = 1  # assign areas that get more suitable (>1) to 1
  Blank<-c(Blank,suitscale) #stack
    }
FutSuit<-Blank[[-1]]

## Average across scenarios
meansuit<-mean(FutSuit,na.rm = T) #get average across scenarios

#Muliply by refugia score to get joint score == "is it good?" * "can they get there?"
SuitRefMean<-meansuit*mean

writeRaster(meansuit, paste(Spp[u],"ScaledSuitability_RCPmean.tif",sep=""),overwrite=TRUE)
writeRaster(SuitRefMean, paste(Spp[u],"Suitability_by_Refugia_RCPmean.tif",sep=""),overwrite=TRUE)

rm("mean","meansuit","SuitRefMean")

}

####### END OF MEAN ######


######################## UPPER 90% CI ########################

for(u in c(1:54)){
  
### Present modeled distribution ---------------------------------------------------------------
  
  setwd(paste(dir,"/BRT_Output/PresentRasters/SppPresent/",sep=""))
  filePresL<-list.files(pattern=Spp[u])
  filePres<-readRDS(filePresL)
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% NAKey), c("Mean", "UPR90", "LWR90")] <- NA # NA removal
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% AK_Key), c("Mean", "UPR90", "LWR90")] <- NA #Western AK removal
  
  assign("Present",filePres)
  
### Future modeled distributions ---------------------------------------------------------------
  
  scenario<-c("_26_2071","_45_2071","_70_2071")
  modelVers<-c("ECEarth3_","CNRMESM21_","UKESM10LL_")
  
  setwd(dir)
  setwd("./BRT_Output/FutureRasters/SppFuture")
  for (k in c(1:3)) {
    for (j in c(1:3)) {
      Future<-c(paste(Spp[u],modelVers[k],scenario[j],"*", sep=""))
      file<-list.files(pattern=Future, all.files=TRUE, full.names=FALSE) %>% readRDS(.)
      file[which(paste(file$X,file$Y, sep=".") %in% NAKey), c("Mean", "Upper90_CI", "Lower90_CI")] <- NA  #note: retaining western AK in future predictions
      assign(paste(substr(modelVers[k],1,2),substr(scenario[j],2,3),sep=""),file)
    }}
 
### Put in list ---------------------------------------------------------------
  List<-list(Present,CN26,CN45,CN70,EC26,EC45,EC70,UK26,UK45,UK70)
  Names<-c("Present","CN26","CN45","CN70","EC26","EC45","EC70","UK26","UK45","UK70")
  

### Regional Mid-value, top 95% + 75% cumulative population threshold of present abundance ---------------------------------------------------------------
  
  #Med<-median(Present$Mean, na.rm=TRUE) #median present density
  Max95<-quantile(Present$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
  rs <- sort(Present$Mean, decreasing=TRUE)  # 75% population threshold of Mean
  cum_75 <- min(rs[cumsum(rs) < sum(rs)*0.75])
  
#### Calculate Refugia output ########------------------------------------------

### Apply species-specific, core 75% of population as threshold ---------------------------------------------------------------
  Thres <-lapply(List, function(x){cbind(x[,c(1:2)],ifelse(x[,4]>=cum_75,1,0))}) #UPR90 == [,4]
  
###Empty raster for stacking -----------------------------------------------
  setwd(dir)
  Stack<-Blank<-rast("EmptyRegionBase.tiff")

### Velocity metric ---------------------------------------------------------
  for (i in c(2:10)){ #run through 9 future RCP/GCMs
    tblPres <- Thres[[1]] #present raster
    colnames(tblPres)[3] <- 'prev'
    tblFut <- Thres[[i]]
    colnames(tblFut)[3] <-  'prev'
   
    p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>%
      dplyr::select(pixelID, X, Y, prev)
    f.xy <- mutate(tblFut, pixelID = 1:nrow(tblFut)) %>% 
      dplyr::select(pixelID, X, Y, prev)
   
    p.xy2 <- 
      filter(p.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    f.xy2 <-
      filter(f.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    
    if (nrow(f.xy) > 0) { d.ann <- as.data.frame(ann(
      as.matrix(p.xy2[, -1, drop = FALSE]),
      as.matrix(f.xy2[, -1, drop = FALSE]),
      k = 1,
      verbose = F)$knnIndexDist) # returning in meters
    d1b <- as.data.frame(cbind(f.xy2, round(sqrt(d.ann[, 2])/1000))) #change to km
    names(d1b) <- c("ID", "X", "Y", "bvel")
    } else {
      print(spec[i])
    }
    f.xy <- as.data.frame(f.xy)
    colnames(f.xy) <- c('ID', 'X', 'Y', 'Pres')
    f.xy <- as_tibble(f.xy)
    d1b <- left_join(f.xy, d1b, by = c('ID', 'X', 'Y'))
    d1b <- mutate(d1b, fat = fattail(bvel, 8.3333, 0.5)) # 8.3333 is appropriate since the scale is km
    sppref <- rast(d1b[, c(2, 3, 6)])
    sppref[is.na(sppref)] <- 0
    Stack <- c(Stack,sppref)
  }

  Stack<-Stack[[-1]]#get rid of template
  mean <- mean(Stack, na.rm = T)  #average all scenarios
  
### Write out Refugia ---------------------------------------------
  setwd(dir)
  setwd("./BRT_output/1991_RefugiaRasters_UPR90/")
  writeRaster(mean, paste(Spp[u],"_Refugia_RCP_UPR90.tif",sep=""),overwrite=TRUE)
  
  rm("Stack","d1b","d.ann","f.xy","f.xy2","Thres")
  
### Get suitability metric from density -------------------------
  
  for (k in 2:length(Names)){
    suit<-rast(get(Names[k])[,c(1:2,4)]) # scale UPR90CI
    suitscale<-suit/Max95  # scaled against present 95% max 
    values(suitscale)[values(suitscale) > 1] = 1  # assign areas that get more suitable (>1) to 1
    Blank<-c(Blank,suitscale) #stack
  }
  FutSuit<-Blank[[-1]]
  
  ### Average across scenarios -------------------------------------
  meansuit<-mean(FutSuit,na.rm = T) #get average across scenarios
  
  #Muliply by refugia score to get joint score - is it good? x can they get there?
  SuitRefMean<-meansuit*mean
 
  writeRaster(meansuit, paste(Spp[u],"ScaledSuitability_RCP_UPR90.tif",sep=""),overwrite=TRUE)
  writeRaster(SuitRefMean, paste(Spp[u],"Suitability_by_Refugia_RCP_UPR90.tif",sep=""),overwrite=TRUE)
  
  rm("mean","meansuit","SuitRefMean")
  
}

##### END OF UPPER 90%CI REFUGIA #####

######################## LOWER 90% CI ########################

#Some present/future rasters are entirely 0. Show error if this occurs in Present raster
#Then patch fix: add one distant pixel which is then cut out, outputs will be 0:
addon<-data.table(pixelID=3605101,X=-792500,Y=1120500,prev=1) # 

for(u in c(1:54)){
  #Present modeled distribution ---------------------------------------------------------------
  
  setwd(paste(dir,"/BRT_Output/PresentRasters/SppPresent/",sep=""))
  filePresL<-list.files(pattern=Spp[u])
  filePres<-readRDS(filePresL)
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% NAKey), c("Mean", "UPR90", "LWR90")] <- NA # NA removal
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% AK_Key), c("Mean", "UPR90", "LWR90")] <- NA #Western AK removal
  
  assign("Present",filePres)
  
  # Future modeled distributions ---------------------------------------------------------------
  
  scenario<-c("_26_2071","_45_2071","_70_2071")
  modelVers<-c("ECEarth3_","CNRMESM21_","UKESM10LL_")
 
  setwd(dir)
  setwd("./BRT_Output/FutureRasters/SppFuture")
  for (k in c(1:3)) {
    for (j in c(1:3)) {
      Future<-c(paste(Spp[u],modelVers[k],scenario[j],"*", sep=""))
      file<-list.files(pattern=Future, all.files=TRUE, full.names=FALSE)
      file<-readRDS(file)
      file[which(paste(file$X,file$Y, sep=".") %in% NAKey), c("Mean", "Upper90_CI", "Lower90_CI")] <- NA  #note retaining western AK in future predictions
      assign(paste(substr(modelVers[k],1,2),substr(scenario[j],2,3),sep=""),file)
    }}
  
  # Put in list ---------------------------------------------------------------
  List<-list(Present,CN26,CN45,CN70,EC26,EC45,EC70,UK26,UK45,UK70)
  Names<-c("Present","CN26","CN45","CN70","EC26","EC45","EC70","UK26","UK45","UK70")
  
# Regional top 95% dens + 75% cumulative pop threshold of present abundance ---------------------------------------------------------------
  
  #Med<-median(Present$Mean, na.rm=TRUE) #median present density
  Max95<-quantile(Present$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
  rs <- sort(Present$Mean, decreasing=TRUE)  # 75% population threshold of Mean
  cum_75 <- min(rs[cumsum(rs) < sum(rs)*0.75])
  
### Calculate Refugia output ------------------------------------------
### Apply species-specific, core 75% of population as threshold ---------------------------------------------------------------
  Thres <-lapply(List, function(x){cbind(x[,c(1:2)],ifelse(x[,5]>=cum_75,1,0))})  #LWR90 == [,5]

  #Empty raster for stacking -----------------------------------------------
  setwd(dir)
  Stack<-Blank<-rast("EmptyRegionBase.tiff")

  ### Velocity metric ---------------------------------------------------------
  for (i in c(2:10)){ #run through 9 future RCP/GCMs
    tblPres <- Thres[[1]] #present raster
    colnames(tblPres)[3] <- 'prev'
    tblFut <- Thres[[i]]
    colnames(tblFut)[3] <-  'prev'

    p.xy <- mutate(tblPres, pixelID = 1:nrow(tblPres)) %>%
      dplyr::select(pixelID, X, Y, prev)
    f.xy <- mutate(tblFut, pixelID = 1:nrow(tblFut)) %>% 
      dplyr::select(pixelID, X, Y, prev)
    
    tryCatch (if(max(p.xy$prev,na.rm=TRUE)==0) stop(paste(Spp[u],' Not present in present'))
      , error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    if(max(p.xy$prev,na.rm=TRUE)==0){ 
          p.xy<-rbind(p.xy, addon)}
    
    if(max(f.xy$prev,na.rm=TRUE)==0){
      f.xy<-rbind(f.xy, addon) }  
    
    p.xy2 <- 
      filter(p.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    f.xy2 <-
      filter(f.xy, prev > 0.1) %>% dplyr::select(1:3) %>% as.matrix()
    
    if (nrow(f.xy) > 0) { d.ann <- as.data.frame(ann(
      as.matrix(p.xy2[, -1, drop = FALSE]),
      as.matrix(f.xy2[, -1, drop = FALSE]),
      k = 1,
      verbose = F)$knnIndexDist) # returning in meters
    d1b <- as.data.frame(cbind(f.xy2, round(sqrt(d.ann[, 2])/1000))) #change to km
    names(d1b) <- c("ID", "X", "Y", "bvel")
    } else {
      print(spec[i])
    }
    
    f.xy <- as.data.frame(f.xy)
    colnames(f.xy) <- c('ID', 'X', 'Y', 'Pres')
    f.xy <- as_tibble(f.xy)
    d1b <- left_join(f.xy, d1b, by = c('ID', 'X', 'Y'))
    d1b <- mutate(d1b, fat = fattail(bvel, 8.3333, 0.5)) # 8.3333 is appropriate since the scale is km
    sppref <- rast(d1b[, c(2, 3, 6)])
    sppref <- crop(sppref,Stack)
    sppref[is.na(sppref)] <- 0
    Stack <- c(Stack,sppref)
  }

  Stack<-Stack[[-1]]#get rid of template
  mean <- mean(Stack, na.rm = T)  #average all scenarios
  
  ### Write out Refugia ---------------------------------------------
  setwd(dir)
  setwd("./BRT_output/1991_RefugiaRasters_LWR90/")
  writeRaster(mean, paste(Spp[u],"_Refugia_RCP_LWR90.tif",sep=""),overwrite=TRUE)
  
  rm("Stack","d1b","d.ann","f.xy","f.xy2","Thres")
  
  ### Get suitability metric from density -------------------------
 
  for (k in 2:length(Names)){
    suit<-rast(get(Names[k])[,c(1:2,5)]) # scale LWR90CI
    suitscale<-suit/Max95  # scaled against present 95% max 
    values(suitscale)[values(suitscale) > 1] = 1  # assign areas that get more suitable (>1) to 1
    Blank<-c(Blank,suitscale) #stack
  }
  FutSuit<-Blank[[-1]]
  
  ### Average across scenarios -------------------------------------
  meansuit<-mean(FutSuit,na.rm = T) #get average across scenarios
  
  #Muliply by refugia score to get joint score: is it good? * can they get there?
  SuitRefMean<-meansuit*mean
  
  writeRaster(meansuit, paste(Spp[u],"ScaledSuitability_RCP_LWR90.tif",sep=""),overwrite=TRUE)
  writeRaster(SuitRefMean, paste(Spp[u],"Suitability_by_Refugia_RCP_LWR90.tif",sep=""),overwrite=TRUE)
  
  rm("mean","meansuit","SuitRefMean")

}
####### END OF LOWER 90% CI #########

####################### END OF CODE ##########################