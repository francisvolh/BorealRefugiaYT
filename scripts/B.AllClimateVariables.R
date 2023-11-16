###############################################################################
#####    B. CLIMATE NA DATA 7.3 - PC and Ecozones                         #### 
#####    Current Normals (4 periods) and Projected 2070-2100 Normals      ####
#####    Anna Drake  March, 2023                                          ####
###############################################################################

## Note: ignore "barrier" notes for current models - this was an experiment to see if 
## predicted vs observed bird distributions could be better aligned using a migratory barrier 
## estimate for east>west and south>north west populations - but not applied in current models

# Note 2: FVO started cleaning up and modifying script to restrict it to the use of sf, terra, and other 
# up to date packages, after october 2023, final phase of the R spatial-evolution

# add function
##`%notin%` <- Negate(`%in%`) ### not used in this script
   
## load packages


##################################################################
# Part 1:  Extract Climate Normals, Topography for point counts ##
# Normals are periods:1991                                     ##
##################################################################

#Source: https://adaptwest.databasin.org/pages/adaptwest-climatena/
#ClimateNA details(http://s3-us-west-2.amazonaws.com/www.cacpd.org/CMIP6/ReadMe_ClimateNA_CMIP6.txt)
#Topography source 1km resolution https://www.earthenv.org/topography

#############################
### Get Survey locations ####
#############################

#get location from QPAD bird abundance estimations
KEY<-readRDS("data/YT Boreal Refugia Drive/QPAD_Output/ALFL_Data.rds") #Should be 202712 records

Clim_xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations, avoiding "re sightings"

Clim_xy <- Clim_xy_frame[,c(2:3)] # only coordinates in lat lon

# produce a Spatial object
Clim_xy<-terra::vect(Clim_xy, crs = "EPSG:4326") ## CRS is  for lat lon data coming from QPAD

# Match (reproject) points to the climate raster projection
clim.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/"

rast.crs<- terra::crs(terra::rast(paste0(clim.folder,"Normal_1991_2020_bFFP.tif"))) ## get CRS object from climate rasters

Clim_xy <- terra::project(Clim_xy, rast.crs) #reproject from lat lon to climate rasters CRS

Clim_buffer <-sf::st_buffer(sf::st_as_sf(Clim_xy),150) # Create a buffer around the points 150 m, this is now a Polygon object


##########################################
# Import & reproject topographic values
##########################################
topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"

TPI_Raster<-terra::rast(paste0(topo.folder,"tpi_1KMmd_GMTEDmd.tif"))#250/90 m windows
Eastness<-terra::rast(paste0(topo.folder,"eastness_1KMmd_GMTEDmd.tif"))
Northness<-terra::rast(paste0(topo.folder,"northness_1KMmd_GMTEDmd.tif"))
TRI<-terra::rast(paste0(topo.folder,"tri_1KMmd_GMTEDmd.tif")) #250/90 m windows

TopoStack<-c(TPI_Raster,Eastness,Northness,TRI) # stack topo rasters 
TopoStack<-terra::crop(TopoStack, terra::ext(-180,-80,10,85)) #reduce raster extent to predetermined 

### Standardize against Climate raster
TemplateClimrast<-terra::rast(paste0(clim.folder,"Normal_1991_2020_bFFP.tif")) #choose a raster of climate

template<-terra::project(TopoStack, TemplateClimrast, align = TRUE, threads = TRUE)##beware: Anna used align as TRUE, not just a projection transform
#did a first run to compare just re-project with a crs vs using two rasters

Topo_align<-terra::project(TopoStack, template) # this seems to produce the extact same results as before, but recovers the names of the layers
#looking at both stacks, there is only a tiny move of the 10 value tick for tpi... does it matter? will keep Anna's steps then to be safe

Topo_align2<-terra::crop(Topo_align, terra::ext(-3462442,-411705,536035.6,3786192)) # crop accommodates region and PC extents

# Point count bounding box to speed things up
e2<-Clim_buffer
# Crop topography to bounding box
TopoCropped<-terra::crop(Topo_align2,e2) #worked with the terra clim_buffer object, cross check after if the code does not break!

###################################################
### Extract 1991 Climate Normal Period to feed BRT models only
###################################################

#not looping on periods anymore so may not need these parameters
Periods<-c('Norm1991')

Normrast<- list.files(path = clim.folder, pattern='.tif$', all.files=TRUE, full.names=TRUE)

# Get and stack Climate rasters for that period
NormStack<-terra::rast(Normrast)
Normrastnames<- list.files(path = clim.folder, pattern='.tif', all.files=TRUE, full.names=FALSE) # 12 names

NormStack<- terra::crop(NormStack, e2)

names(NormStack) <- Normrastnames 

#terra::writeRaster(NormStack, "data/Norms_Clim_Aligned_raster.grd", overwrite=TRUE) # wrote file as grd like Anna had

#NormStack <- rast("data/Norms_Clim_Aligned_raster.grd")

# Add topography
NormStack<-c(NormStack,TopoCropped) # 18 layers,################################## I HAVE 16 layers only!!!!! not 18
#plot(NormStack)

#terra::writeRaster(NormStack, "data/Norms_ALL_Aligned_raster.grd", overwrite=TRUE) # wrote file as grd like Anna had


############################## next time load all clim and topo vars from raster produced before Norms_ALL_Aligned_raster.grd
#NormStack<- terra::rast("data/Norms_ALL_Aligned_raster.grd")
#plot(NormStack)

# Clean variable names
List<-names(NormStack)

List<-gsub("_1KMmd_GMTEDmd","",List)
List<-gsub("Normal_1991_2020_","",List)
List<-gsub(".tif","",List)


# Data frame to populate 
output<-Clim_xy_frame

# Extract
system.time({
  for (k in 1:length(List)){ #Extraction loop
    Dat<-exactextractr::exact_extract(NormStack[[k]], Clim_buffer,'weighted_mean', weights='area')
    Dat<-data.frame(Dat)
    Dat$Dat<-round(Dat$Dat,2)
    names(Dat)[names(Dat)=="Dat"] <- List[k]
    output<-cbind(output,Dat)
  } #close extraction loop
})
  bkupoutput<-output
  
  #set FFP to NFFD where it exceeds NFFD (dealing with the FFP issue at some high elevation locations)
  output$FFP<-ifelse(output$NFFD-output$FFP<0,output$NFFD,output$FFP)
  output$bFFP<-ifelse(output$NFFD-output$FFP<0,NA,output$bFFP) # set dates to NA where obviously wrong
  output$eFFP<-ifelse(output$NFFD-output$FFP<0,NA,output$eFFP)
  output$Thaw<-output$NFFD-output$FFP
  output<-subset(output,!is.na(output$FFP)) 


#61288 locations - 151 missing climate data

# write out and rename by period
#write.csv(output, paste(FolderPath,Periods[i],"_data.csv",sep="")) #write out


#write.csv(output, "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991_data_FVOversion.csv") #write out
###LOAD ANNAS file  output<-read.csv(file.choose())   ############################### may only need to read the Norm1991_data.csv file in the Drive##############
## in data/YK Refugia Code and material/PresentDayNormal


assign(paste(Periods,"PCs",sep="_"), output) # name


#} # Close climate normal loop

summary(output) # 2 NAs only of 64865 sites     ################ if I run it all myself I HAVE MORE NAs in the climate vars, only 1 in the topo vars

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

#### Import Ecozone boundaries                                          ######## may not need these!
#dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main'
#FolderPath<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/PresentDayNormals/'
#setwd(dir)

#crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters


#Ecozones <- st_read("RegionShapefile/BorealTaigaCordillera.w.AK.shp")

#Ecozones <- st_transform(Ecozones, crs) 

Ecozones <- sf::st_union(BCR4.1_USACAN, BCR4.0_USACAN)
Ecozones<-sf::st_transform(Ecozones, crs = rast.crs)
#Ecozones <- InclusionShapefile #no sure this is it.... need to confirm with Diana and Lisa
# or is it BCR 40 and 41

### got the file now!!!!!!!!!!!
Ecozones <- sf::st_read(file.choose())

alaska <- USA|>
  dplyr::filter(NAME_1 == "Alaska")

  ggplot2::ggplot()+
  ggplot2::geom_sf(data = Ecozones, ggplot2::aes(), fill = "green", alpha = 0.3)+
  ggplot2::geom_sf(data= alaska, alpha = 0)+
  ggplot2::geom_sf(data = canada, alpha = 0)

####
#### check this buffer!!!!! it takes to long or it explodes########################################################################################################################

####
system.time({
  EcozonesBuf <- sf::st_buffer(Ecozones, 400000) #for our tree dispersal est (fat-tailed dist) P of movement 400km within a century==0.0009. So limit of possibly founding populations
  
})

  study.area<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = Ecozones,   ggplot2::aes(), fill = "green", alpha = 0.3)+
    ggplot2::geom_sf(data = EcozonesBuf,   ggplot2::aes(), fill = "red", alpha = 0.3)+
    ggplot2::geom_sf(data= alaska, alpha = 0)+
    ggplot2::geom_sf(data = canada ,   ggplot2::aes(), fill = "yellow", alpha = 0.3)+
    ggplot2::theme_bw()


  
  #ggplot2::ggsave(study.area, filename = "plot.study.area.png", path = "plots/", units = "in", width = 20, height = 10, dpi = 300, bg = "white")
  
# Topographic Data
TopoBuf<-terra::crop(Topo_align2,EcozonesBuf)
ext(TopoBuf)
names(TopoBuf)<-gsub("_1KMmd_GMTEDmd","",names(TopoBuf))

# Add standardized survey type and year raster layer
year<-Cat<-TopoBuf[[1]]
values(year)<-2022
names(year)<-"year"
values(Cat)<- 1 # PC
names(Cat)<-"Cat"

# Function to deal with FFP errors
#   re-written to apply directly into the rasters and not using overlay() from raster package

#FunCorr <- function(x, y) ifelse(y-x<0,y,x) # where NFFD<FFP, use NFFD

FunCorrTerra <- function(x, y) {
  result <- x
  result[y-x<0] <- y
  return(result)
}

#FunDat <-function(x, y) ifelse(y<0,NA,x)

FunDatTerra <- function(x, y) {
  result <- x
  result[y < 0] <- NA
  return(result)
}

#Periods<-c(#'Norm1961',
 # 'Norm1991')

# Run through each period of interest
#for (i in 1:length(Periods)) {
  #setwd(paste(FolderPath,Periods[i],sep=""))
  Normrast<- list.files(path = clim.folder, pattern='.tif$', all.files=TRUE, full.names=FALSE)
  
# Rasters at 1km resolution == 150m values, as is.
  Norm<-list()
  for (j in 1:length(Normrast)) {
    Norm[[j]]<-terra::rast(paste0(clim.folder,Normrast[j]))
    names(Norm[[j]]) <- terra::varnames(Norm[[j]])
    Norm[[j]]<-terra::crop(Norm[[j]], EcozonesBuf)

# Clean layer names to match models
    names(Norm[[j]])<-gsub("Normal_1991_2020_","",names(Norm[[j]]))
    #names(Norm[[j]])<-gsub("Normal_1981_2010_","",names(Norm[[j]]))
    #names(Norm[[j]])<-gsub("Normal_1971_2000_","",names(Norm[[j]]))
    #names(Norm[[j]])<-gsub("Normal_1961_1990_","",names(Norm[[j]]))
  }
  
  #stak the rasters
   NormStack<-c(Norm[1:length(Normrast)]) # which are this not aligned?
   
   # deal with errors in FFP dates/data
   error<-NormStack[[9]]-NormStack[[5]] # where FFP exceeds NFFD (nonsensical)
   
   NormStack[[1]] <- FunDatTerra(NormStack[[1]], error)
   #names(NormStack[[1]])<-"bFFP" #names was already carried from previoys lines
   
   NormStack[[3]] <- FunDatTerra(NormStack[[3]], error)
   #names(NormStack[[3]])<-"eFFP"
   
   #correct FFP 
   NormStack[[5]] <- FunCorrTerra(NormStack[[5]], error)
   #names(NormStack[[5]])<-"FFP"
  
   # add thaw metric
   Thaw<-NormStack[[9]]-NormStack[[5]]
   names(Thaw)<-"Thaw"
   
   #Stack 
   NormStack<-c(NormStack,Thaw,year,Cat,TopoBuf) ## should have made rast(NormStack) to make it an actual SpatRasterStack!!!!
   
   NormStack2<-rast(NormStack)
   
   writeRaster(NormStack2, filename=paste0("data/corrected_rasters_clim_topo/","all_corrected_",Periods,".grd"))
   plot(NormStack2)
   #this would write each raster 1 by 1
   #for (k in 1:length(NormStack)) {
    # r <- NormStack[[k]]
     #writeRaster(r, filename=paste0("data/corrected_rasters_clim_topo/",Periods,names(r),".grd")) #individual rasters
     
   #}
   #writeRaster(NormStack, filename=paste0("data/corrected_rasters_clim_topo",Periods,names(r),".grd"), bylayer=TRUE, format="GTiff", overwrite=TRUE) 
   
   ##rename files in hard drive due to a location name error
   # Replace text in file names
  # to_change<-list.files("data/corrected_rasters_clim_topo/", full.names = TRUE)
   #for (i in 1:length(to_change)) {
    #  if (file.exists(to_change[i])) {
     #  file.rename(to_change[i],gsub("corrected_rasters_clim_topo/corrected_rasters_clim_topo","/corrected_rasters_clim_topo/",to_change[i] ) )
    #  print(paste0("File  ", gsub("data/corrected_rasters_clim_topo/corrected_rasters_clim_topo","",to_change[i] ), "was rename"))
     #  } else{
      #  print("File not exists..")
     #} 
     #}
   NormStack <- NormStack2 ## just labelled as 2 because I wanted to keep another file of the same name I loaded before
   #NormStack <- rast(NormStack) # this is only needed if it comes from a list, but it is already a Spat Stack
   
   
   
   #just to visualize them
   plot_list<-list()
   
   for (i in 1:length(names(NormStack))) {
     
     p <- ggplot()+
       geom_spatraster(data = NormStack[[i]], #aes(fill = "bigfile[, varname]")
       )+
       geom_sf(data = Ecozones, aes(),alpha = 0)+
       scale_fill_hypso_c()+
       ggtitle(names(NormStack[[i]]))+
       theme_bw()+
       theme(legend.position = "none")
     ## get rid of axis labels for this cowplot
     
     plot_list[[i]] <- p
     
   }

   cowplot::plot_grid(plotlist = plot_list, nrow = 4, ncol = 5)


   #Pare down and write out as dataframe
   Climate<-as.data.frame(NormStack, xy=TRUE)
   Climate<-subset(Climate, !is.na(Climate$EMT)) # get rid of ocean data
   saveRDS(Climate, paste("data/corrected_rasters_clim_topo/all_rasts_Correct_Ecozones.RDS")) #FVO trial version equivalent to below line

#}


