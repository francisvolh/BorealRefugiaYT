###############################
#### G. Plotting output #######
###############################

require(raster)
require(terra)
require(sf)
require(viridis)
require(rgeos)

`%notin%` <- Negate(`%in%`)

# Load data ---------------------------------------------------------------
crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters

#dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main'
######dirupper<-'/Users//annadrake/Desktop/Yukon Project Data'

#setwd('/Users/annadrake/Desktop/Yukon Project Data/Mapping Resources/')

CEC<-st_read('data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_CEC_Eco_Level2/NA_CEC_Eco_Level2.shp')
plot(CEC)

CEC <- st_transform(CEC, crs) 
shape <- st_transform(CEC, crs = "EPSG:3573")


ggplot()+
  geom_spatraster(data = sample_rast, #aes(fill = "bigfile[, varname]")
  )+
  scale_fill_hypso_c()+
  geom_sf(data = CEC, aes(), alpha = 0)+
  geom_sf(data = borealtaigacordillera, aes(), alpha = 0)+
  theme_bw()



require(colorspace)
bgy <- sequential_hcl(10, "ag_GrnYl", rev=TRUE)

# Extract key of complete co-variate data locations ---------------------------------------------------------------
setwd(dir)
Norm1991<-readRDS("BRT_output/PresentRasters/Norm1991_EcozoneNormals.rds")
NAKey<-subset(Norm1991,!is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,!is.na(NAKey$tri))
NA_rast<-rasterFromXYZ(NAKey[1:3],crs=crs)
NA_rast[!is.na(NA_rast)] <-1

NAKey<-paste(NAKey$x,NAKey$y,sep=".") #raster squares with complete covariates only

rm("Norm1991")

# Import key of masked AK regions (AK portion of BCR 3 + BCR 2) -------------------------------------
AK_remove<-read.csv("AK_removalregion.csv")
AK_Key<-paste(AK_remove$x,AK_remove$y,sep=".")

## Breakdown Region -----------------------------------------------------
#### Region shapefiles ---------------------------------------------------------------
crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters
setwd(dirupper)

BCR40_USACAN <- st_read("CordilleraBreakdown/BCR4.0_USACAN.shp") %>% st_transform(., crs = "EPSG:3573")
BCR41_USACAN <- st_read("CordilleraBreakdown/BCR4.1_USACAN.shp") %>% st_transform(., crs = "EPSG:3573")
BorealCordCAN <- st_read("CordilleraBreakdown/BorealCordilleraCAN.shp") %>% st_transform(., crs = "EPSG:3573")
TaigaCordCAN <- st_read("CordilleraBreakdown/TaigaCordilleraCAN.shp") %>% st_transform(., crs = "EPSG:3573")
#Ocean <- st_read("Mapping Resources/GSHHG/NorthAmericanCoast.shp")%>% st_transform(., crs)
#Coast<-st_crop(Ocean, xmin=-3191000, ymin=1120000, xmax=-798000, ymax=3785000)



### Get Present Rasters No barrier
setwd(dir)
setwd("./BRT_Output/PresentRasters/SppPresent/")

ModList<-list.files() #list Present rasters
Spp<-substr(ModList,1,4)

#filePres$Mean<-ifelse(is.na(filePres$Mean),NA,-1)
#test<-filePres
#background<-rasterFromXYZ(test[1:3],crs=crs)
#plot(background)

for (u in 1:52){
 
  setwd(paste0(dir,"/BRT_Output/PresentRasters/SppPresent/"))
  
  filePres<-list.files(pattern=Spp[u])
  filePres<-readRDS(filePres)
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %notin% NAKey), c("Mean", "UPR90", "LWR90")] <- NA # NA removal
  filePres[which(paste(filePres$X,filePres$Y, sep=".") %in% AK_Key), c("Mean", "UPR90", "LWR90")] <- NA #Western AK removal
  
  rast<-rasterFromXYZ(filePres[1:3],crs=crs)

## Set limits------------------------------------------
  prev <- cellStats(rast, 'mean')	
  low <- cellStats(rast, 'min', na.rm=TRUE)
  zmin <- max(prev,0.001, na.rm=TRUE)
  zmin <- min(zmin,0.01, na.rm=TRUE)
  zmax <- cellStats(rast, 'max', na.rm=TRUE)
  q99 <- quantile(rast, probs=c(0.999), na.rm=TRUE)

#Plot it -------------------------------------
setwd(paste0(dir,"/BRT_output/PresentRasters/Present Plots"))

png(file=paste0(Spp[u],"_pred1km.png"), width=2000, height=2000, res=216)
  plot(st_geometry(BCR41_USACAN),border="black", xlim = c(-3169846, -746769),ylim = c(1116692, 3790154))
  plot(background, col="grey", axes=FALSE, legend=FALSE,  add=TRUE)
  plot(rast, col=bgy, zlim=c(zmin,q99), axes=FALSE, add=TRUE)
  plot(rast, col="#255668", zlim=c(q99,zmax), axes=FALSE, legend=FALSE, add=TRUE)
  plot(rast, col="#F9FFAF", zlim=c(low,zmin), axes=FALSE, legend=FALSE, add=TRUE)
  plot(st_geometry(BCR40_USACAN),border="black", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="black", add=TRUE)
dev.off()
 }

require(khroma)
batlow <- colour("batlow")
plot(batlow(40))

### Get Refugia - ## NEED TO DELETE MISSING DATA AREAS IN RASTERS
NA_rast<-crop(NA_rast,Scale) #crop using file below
setwd(dir)
setwd("./BRT_Output/1991_RefugiaRasters")

for (u in 1:52){
filePres<-list.files(pattern=Spp[u])

Ref<-raster(filePres[1]) %>% mask(.,NA_rast)
Scale<-raster(filePres[2]) %>% mask(.,NA_rast)
ScaleRef<-raster(filePres[3])%>% mask(.,NA_rast)

png(file=paste0(Spp[u],"_Refugia.png"), width=3000, height=1000, res=216)
par(mfrow=c(1,3))
plot(Ref, col=batlow(20), axes=FALSE, main="Refugia")
plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)

plot(Scale, col=batlow(20), axes=FALSE, main="Future Suitability")
plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)

plot(ScaleRef, col=batlow(20), axes=FALSE, main="Suitability x Refugia")
plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
dev.off()

}

##### Upper limits

setwd(dir)
setwd("./BRT_Output/1991_RefugiaRasters_UPR90")

for (u in 1:52){
  filePres<-list.files(pattern=Spp[u])
  
  Ref<-raster(filePres[1]) %>% mask(.,NA_rast)
  
  Scale<-raster(filePres[2]) %>% mask(.,NA_rast)
  ScaleRef<-raster(filePres[3])%>% mask(.,NA_rast)
  
  png(file=paste0(Spp[u],"_Refugia_UPR90.png"), width=3000, height=1000, res=216)
  par(mfrow=c(1,3))
  plot(Ref, col=batlow(20), axes=FALSE, main="Refugia")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  
  plot(Scale, col=batlow(20), axes=FALSE, main="Future Suitability")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  
  plot(ScaleRef, col=batlow(20), axes=FALSE, main="Suitability x Refugia")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  dev.off()
  
}

##### Lower limits

setwd(dir)
setwd("./BRT_Output/1991_RefugiaRasters_LWR90")

for (u in 1:52){
  filePres<-list.files(pattern=Spp[u])
  
  Ref<-raster(filePres[1]) %>% mask(.,NA_rast)
  Scale<-raster(filePres[2]) %>% mask(.,NA_rast)
  ScaleRef<-raster(filePres[3])%>% mask(.,NA_rast)
  
  png(file=paste0(Spp[u],"_Refugia_LWR90.png"), width=3000, height=1000, res=216)
  par(mfrow=c(1,3))
  plot(Ref, col=batlow(20), axes=FALSE, main="Refugia")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  
  plot(Scale, col=batlow(20), axes=FALSE, main="Future Suitability")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  
  plot(ScaleRef, col=batlow(20), axes=FALSE, main="Suitability x Refugia")
  plot(st_geometry(BCR40_USACAN),border="white", add=TRUE)
  plot(st_geometry(BCR41_USACAN),border="white", add=TRUE)
  dev.off()
  
}


############# End of code ###########################