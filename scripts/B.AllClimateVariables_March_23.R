###############################################################################
#####    B. CLIMATE NA DATA 7.3 - PC and Ecozones                         #### 
#####    Current Normals (4 periods) and Projected 2070-2100 Normals      ####
#####    Anna Drake  March, 2023                                          ####
###############################################################################

## Note: ignore "barrier" notes for current models - this was an experiment to see if 
## predicted vs observed bird distributions could be better aligned using a migratory barrier 
## estimate for east>west and south>north west populations - but not applied in current models


# add function
`%notin%` <- Negate(`%in%`)
   
## load packages
library(pacman)
pacman::p_load(stringr,maptools,intrval,raster,doBy,rgdal,dplyr,exactextractr,sf,terra,magrittr)

##################################################################
# Part 1:  Extract Climate Normals, Topography for point counts ##
# Normals are for 4, 30 yr periods: 1961,1971,1981,1991         ##
##################################################################

#Source: https://adaptwest.databasin.org/pages/adaptwest-climatena/
#ClimateNA details(http://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ReadMe_ClimateNA_CMIP6.txt)
#Topography source 1km resolution https://www.earthenv.org/topography

#############################
### Get Survey locations ####
#############################
dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main'
setwd(dir)

KEY<-readRDS("./QPAD_Output/ALFL_Data.rds") #Should be 202712 records
Clim_xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations
Clim_xy <- Clim_xy_frame[,c(2:3)]

# Spatial object
coordinates(Clim_xy) <- ~ lon + lat
proj4string(Clim_xy) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Match the climate raster projection
crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters
Clim_xy <- spTransform(Clim_xy, crs) 
Clim_buffer <-st_buffer(st_as_sf(Clim_xy),150) # Create a buffer around the points 150 m 

##########################################
# Import & reproject topographic values
##########################################

TPI_Raster<-raster("PresentDayNormals/Topography/tpi_1KMmd_GMTEDmd.tif")#250/90 m windows
Eastness<-raster("PresentDayNormals/Topography/eastness_1KMmd_GMTEDmd.tif")
Northness<-raster("PresentDayNormals/Topography/northness_1KMmd_GMTEDmd.tif")
TRI<-raster("PresentDayNormals/Topography/tri_1KMmd_GMTEDmd.tif") #250/90 m windows

TopoStack<-stack(TPI_Raster,Eastness,Northness,TRI)
TopoStack<-crop(TopoStack, extent(-180,-80,10,85)) #reduce raster extent

### Standardize against Climate raster
TemplateClim<-raster("PresentDayNormals/Norm1961/Normal_1961_1990_bFFP.tif")
template<-projectRaster(from = TopoStack, to = TemplateClim, alignOnly = TRUE) 
Topo_align<-projectRaster(from = TopoStack, to = template)
Topo_align2<-crop(Topo_align, extent(-3462442,-411705,536035.6,3786192)) # crop accommodates region and PC extents

###### Import Derived Barrier data (see "BarrierRastersOct26.R") 
#E_Bar<-raster("PresentDayNormals/Topography/E_Barrier_Elev.tif")
#N_Bar<-raster("PresentDayNormals/Topography/N_Barrier_Elev.tif")
#N_Bar<-crop(N_Bar,extent(Topo_align2))
#E_Bar<-crop(E_Bar,extent(Topo_align2))
#Topo_alignT<-stack(Topo_align2,E_Bar,N_Bar)

dir("./PresentDayNormals/Topography")
writeRaster(Topo_align2,"PresentDayNormals/Topography/AlignedStack", overwrite=TRUE)

# Point count bounding box to speed things up
e2 <- as(extent(Clim_buffer), 'SpatialPolygons')
crs(e2) <- crs

# Crop topography to bounding box
TopoCropped<-crop(Topo_align2,e2)

###################################################
### Extract 1961 and 1991 Climate Normal Period
###################################################

Periods<-c('Norm1961','Norm1991')
FolderPath<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/PresentDayNormals/'

# Get Climate Normal
for (i in 1:2) { # open Climate Normal loop
  
setwd(paste(FolderPath,Periods[i],sep=""))
Normrast<- list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

# Get and stack Climate rasters for that period
Norm<-list()
for (j in 1:length(Normrast)) {
  Norm[[j]]<-raster(Normrast[[j]])
}
NormStack<-stack(Norm[1:length(Normrast)]) 
NormStack<- crop(NormStack, e2)

# Add topography
NormStack<-stack(NormStack,TopoCropped) # 18 layers

# Clean variable names
List<-names(NormStack)
List<-gsub(substr(Normrast[1],1,17),"",gsub("_1KMmd_GMTEDmd","",List))

# Data frame to populate 
output<-Clim_xy_frame

# Extract
for (k in 1:length(List)){ #Extraction loop
  Dat<-exactextractr::exact_extract(NormStack[[k]], Clim_buffer,'weighted_mean', weights='area')
  Dat<-data.frame(Dat)
  Dat$Dat<-round(Dat$Dat,2)
  names(Dat)[names(Dat)=="Dat"] <- List[k]
  output<-cbind(output,Dat)
} #close extraction loop

#set FFP to NFFD where it exceeds NFFD (dealing with the FFP issue at some high elevation locations)
output$FFP<-ifelse(output$NFFD-output$FFP<0,output$NFFD,output$FFP)
output$bFFP<-ifelse(output$NFFD-output$FFP<0,NA,output$bFFP) # set dates to NA where obviously wrong
output$eFFP<-ifelse(output$NFFD-output$FFP<0,NA,output$eFFP)
output$Thaw<-output$NFFD-output$FFP
output<-subset(output,!is.na(output$FFP)) #61288 locations - 151 missing climate data

# write out and rename by period
write.csv(output, paste(FolderPath,Periods[i],"_data.csv",sep="")) #write out
assign(paste(Periods[i],"PCs",sep="_"), output) # name

} # Close climate normal loop

summary(output) # 2 NAs only of 64865 sites
# Barrier data is 9% NAs because bird movement 
# was constrained to NE/W-NW/N when northbound, W, NW when westbound 
# birds were only allowed to hop ~4km over water
# leave as NA - agnostic as to whether all spp will "round corners" 
# (ie. go S,W,E) during spring or make longer flights over gaps

###### END OF POINT COUNT DATA EXTRACTION ########

######################################################
### PART II.  Extract Current climate data from   ####
### full Ecozone regions for predictions          ####
######################################################

#### Import Ecozone boundaries ####
dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main'
FolderPath<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/PresentDayNormals/'
setwd(dir)

crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters

Ecozones <- st_read("RegionShapefile/BorealTaigaCordillera.w.AK.shp")
Ecozones <- st_transform(Ecozones, crs) 
EcozonesBuf <- sf::st_buffer(Ecozones, 400000) #for our tree dispersal est (fat-tailed dist) P of movement 400km within a century==0.0009. So limit of possibly founding populations

# Topographic Data
TopoBuf<-crop(Topo_align2,EcozonesBuf)
extent(TopoBuf)
names(TopoBuf)<-gsub("_1KMmd_GMTEDmd","",names(TopoBuf))

# Add standardized survey type and year raster layer
year<-Cat<-TopoBuf[[1]]
values(year)<-2022
names(year)<-"year"
values(Cat)<- 1 # PC
names(Cat)<-"Cat"

# Function to deal with FFP errors
FunCorr <- function(x, y) ifelse(y-x<0,y,x) # where NFFD<FFP, use NFFD
FunDat <-function(x, y) ifelse(y<0,NA,x)

Periods<-c('Norm1961','Norm1991')

# Run through each period of interest
for (i in 1:length(Periods)) {
  setwd(paste(FolderPath,Periods[i],sep=""))
  Normrast<- list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)

# Rasters at 1km resolution == 150m values, as is.
  Norm<-list()
  for (j in 1:length(Normrast)) {
    Norm[[j]]<-raster(Normrast[j])
    Norm[[j]]<-crop(Norm[[j]], EcozonesBuf)
  
# Clean layer names to match models
    names(Norm[[j]])<-gsub("Normal_1991_2020_","",names(Norm[[j]]))
    names(Norm[[j]])<-gsub("Normal_1981_2010_","",names(Norm[[j]]))
    names(Norm[[j]])<-gsub("Normal_1971_2000_","",names(Norm[[j]]))
    names(Norm[[j]])<-gsub("Normal_1961_1990_","",names(Norm[[j]]))
  }
   NormStack<-stack(Norm[1:length(Normrast)]) 
   
   # deal with errors in FFP dates/data
   error<-NormStack[[9]]-NormStack[[5]] # where FFP exceeds NFFD (nonsensical)
   NormStack[[1]] <-overlay(NormStack[[1]], error, fun = FunDat)
   names(NormStack[[1]])<-"bFFP"
   NormStack[[3]] <- overlay(NormStack[[3]], error, fun = FunDat)
   names(NormStack[[3]])<-"eFFP"
   #correct FFP 
   NormStack[[5]] <- overlay(NormStack[[5]], NormStack[[9]], fun = FunCorr)
   names(NormStack[[5]])<-"FFP"
  
   # add thaw metric
   Thaw<-NormStack[[9]]-NormStack[[5]]
   names(Thaw)<-"Thaw"
   
   #Stack 
   NormStack<-addLayer(NormStack,Thaw,year,Cat,TopoBuf)
   writeRaster(NormStack, filename=paste(dir,"/Ecozone_Climate/",Periods[i],names(NormStack),sep=""), bylayer=TRUE,format="GTiff",overwrite=TRUE) 
   
   #Pare down and write out as dataframe
   Climate<-as.data.frame(NormStack, xy=TRUE)
   Climate<-subset(Climate, !is.na(Climate$EMT)) # get rid of ocean data
   saveRDS(Climate, paste(dir,"/BRT_output/PresentRasters/",Periods[i],"_","EcozoneNormals.rds",sep=""))
}

#Note - very high precip values in some locations...
#Wendler et al. 2017 (Atmosphere) map rainfall >7500 in AK mountains
#greatest rainfall recorded in AK is 5727, globally recorded: 11,871

########### END OF CURRENT CLIMATE NORMALS ############

######################################################
### PART III.  Extract Future climate data from   ####
### full region for predictions                   ####
######################################################

### https://adaptwest.databasin.org/pages/adaptwest-climatena/ ####
### From Mahony et al. 2022 and using 3 projection models
### for the NWN, we should use: CNRM, UK and EC models
### If *excluding* UK model we should use ACC model
### UK model exceeds the predicted temperature buffer (ESC) ("very likely" scenario)
### however there is evidence that changes in cloud state could create such a scenario..)
### CNRM-ESM2-1,UKESM1.0-LL, EC-Earth3, + ACCESS-ESM1.5 where not using the UK
######################################################

#### Projection buffer: just region of interest and 150m edge
EcozonesFut <- sf::st_buffer(Ecozones, 150) # future climate, buffer at edge

# Topographic Data
TopoFut<-crop(TopoBuf,EcozonesFut)

# Add standardized survey type and year raster layer
year<-Cat<-TopoFut[[1]]
values(year)<-2022
names(year)<-"year"
values(Cat)<- 1
names(Cat)<-"Cat"

#### Import Future climate datasets
FolderPath<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/2071_rasters/'
ClimMod<-c("CNRMESM21","ECEarth3","UKESM10LL")
Scenario<-c("_26_2071","_45_2071","_70_2071") #2.6,4.5,7.0 emission scenarios, end of century

### Get the GCM and Emission Scenario of interest
for (i in 1:length(ClimMod)) { #Call GCM
  for (j in 1:length(Scenario)) { #Call Scenario
  setwd(paste(FolderPath,ClimMod[i],Scenario[j],sep=""))
  Projrast<- list.files(pattern='.tif$', all.files=TRUE, full.names=FALSE)
  
#Stack rasters for given scenario/GCM
  Projection<-list()
  for (k in 1:length(Projrast)) {
    Projection[[k]]<-raster(Projrast[[k]])
    Projection[[k]]<- crop(Projection[[k]], EcozonesFut)
    # Get rid of awkward column labels, append 150m 
    names(Projection[[k]])<-gsub("CNRMESM21_ssp126_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("CNRMESM21_ssp245_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("CNRMESM21_ssp370_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("ECEarth3_ssp126_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("ECEarth3_ssp245_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("ECEarth3_ssp370_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("UKESM10LL_ssp126_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("UKESM10LL_ssp245_2071_2100_","",names(Projection[[k]]))
    names(Projection[[k]])<-gsub("UKESM10LL_ssp370_2071_2100_","",names(Projection[[k]]))
  } # end of climate variables

  # deal with errors in FFP dates/data
  error<-Projection[[9]]-Projection[[5]] # where FFP exceeds NFFD (nonsensical)
  Projection[[1]] <-overlay(Projection[[1]], error, fun = FunDat)
  names(Projection[[1]])<-"bFFP"
  Projection[[3]] <- overlay(Projection[[3]], error, fun = FunDat)
  names(Projection[[3]])<-"eFFP"
  #correct FFP 
  Projection[[5]] <- overlay(Projection[[5]], Projection[[9]], fun = FunCorr)
  names(Projection[[5]])<-"FFP"
  # add thaw metric
  Thaw<-Projection[[9]]-Projection[[5]]
  names(Thaw)<-"Thaw"
  
  #Stack 
  ProjStack<-stack(Projection[1:length(Projrast)]) #stack them
  ProjStack<-addLayer(ProjStack,Thaw,year,Cat,TopoFut) # add remaining layers
  writeRaster(ProjStack, paste(dir,"/Ecozone_Climate/",ClimMod[i],"_",Scenario[j],names(ProjStack),sep=""), bylayer=TRUE,format="GTiff",overwrite=TRUE)
  
  #Pare down and write out as dataframe
  Climate<-as.data.frame(ProjStack, xy=TRUE)
  Climate<-subset(Climate, !is.na(Climate$EMT)) # get rid of ocean data
  saveRDS(Climate, paste(dir,"/BRT_output/FutureRasters/",ClimMod[i],"_",Scenario[j],".rds",sep=""))
  }# End of scenarios
} # end of GCM

################# END OF FUTURE CLIMATE NORMALS ###################

