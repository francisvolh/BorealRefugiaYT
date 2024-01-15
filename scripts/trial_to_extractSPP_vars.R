


##############################################################
##############################################################
##############################################################
### Extract mean elevation of CORE AREAS of PRESENT AND FUTURE Dist
### for all birds in a category #####
### ()
##############################################################

#Curent loop runs over all predefined groups
{begin.time <- Sys.time()

topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"

#sample refugia raster in clim vars CRS for EXTENT purposes
bird.elev <- terra::rast("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/ALFL_Refugia_RCPmean.tiff")
terra::crs(bird.elev) <- rast.crs

elevation <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))

elevation <-  terra::crop(elevation, project(NA_rast.crs, "EPSG:4326" ))
elevation <-  terra::project(elevation, NA_rast.crs )

####before extracting need to mask areas of raster that are not of interest
###### NA for Alaska, NA for modeling, and outside of area of interest ---  check with the team!

## crop to ONLY the area of interest (the modeled refugia raster area)
elevation <- terra::mask(elevation, NA_rast.crs) #mask using NAs from env variables 
elevation <- terra::mask(elevation, AKrast) # mask alaska extreme model areas


files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)
files.future <- list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds",  full.names = TRUE)

altitude.mean <- NULL



par(mfrow = c(5, 2)) ## only for testing viz


#loop for to run all groups
for (k in groupings_labs) {

  print(paste("Starting", k,"at", format(Sys.time(), "%X")))
  
  #for one group
  group_spp <- get(k)
  
  #ref.ras1<-NULL
  #suit.ras1<-NULL
  #refxsuit.ras1<-NULL
  print(paste("Processing", k, "species"))
  
  
  for (i in c(group_spp)) {

    print(paste("Working current", i,"at", format(Sys.time(), "%X")))
    
    
    #if I Current dist, and do not use Anna produced cores
    #get the Current dist mean alt
    
    sample1 <- readRDS(files.vect[grep(i, files.vect)] )
    ###################\
    
    sample1 <- terra::rast(sample1[1:3])
    terra::crs(sample1) <- rast.crs
    ####
    ### crop to the "interest area" first, and then get the q75 for high quality OR GET IF FOR THE ECOREGION AREA ONLY
    ###
    sample1 <- terra::crop(sample1, bird.elev)
    
    sample1 <- terra::mask(sample1, NA_rast.crs)#mask using NAs from env variables 
    sample1 <- terra::mask(sample1, AKrast)
    sample1 <- mask(sample1, bcr[1])
    ###find the quantile to cap extreme high values
    Max95<-quantile(terra::values(sample1$Mean), 0.95, na.rm=TRUE) #top 95% density of Mean
    
    ## for core areas DONT DO THIS
    
    #cap but do not standardize!!!!!!!!!!
    
    #sample1 <- sample1 |>
     # dplyr::mutate(
      #  std.mean = Mean/Max95
      #) #standardize to q95
    
    #cap the higher values 
    sample1 <- terra::ifel(sample1 >= Max95, Max95, sample1 )

  
    
    #plot(sample1)
    #catch the 75% of the population =  CORE AREA
    rsvals <- sort(terra::values(sample1$Mean), decreasing=TRUE)
    cum_75 <- min(rsvals[cumsum(rsvals) < sum(rsvals)*0.75])
    
    #find the high quality value at q75: not for densities
    #q75<-quantile(terra::values(sample1$Mean), 0.25, na.rm=TRUE) #top 95% density of Mean
    
    q75<-cum_75
    #extract only the rows of data with high quality 
    sample1<- dplyr::filter(sample1, Mean >= q75)
    #plot(sample1)
    plot(sample1) # only for the first visual run comparison
    title(paste(k, "current dist", i)) ## only for the first visual run comparison
    
    alts <- terra::mask(elevation, sample1) 
    mean.alt <- mean(terra::values(alts$elevation_1KMmd_GMTEDmd), na.rm = TRUE)
 
    #tryout <- terra::ifel(sample1 >=q75,  1, NA) ## this keeps the raster, but only manipulates the values within
    
    sample1 <- ifelse(terra::values(sample1)>=q75, 1, NA) #gets a matrix now, using lambert equal area allows for this assignment of 1 (sq km) to all cells
    
    area.curr <-sum(sample1, na.rm = TRUE)
    
    one.species <- c(k, i, "current", mean.alt, area.curr)
    
    altitude.mean<- rbind( altitude.mean,one.species)
    
    print(paste("Working future", i,"at", format(Sys.time(), "%X")))
    
    #get the future distributions and mean alt
    
    #suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
    #names(suit.ras1) <- terra::varnames(suit.ras1)
    #no need to mask as the elevation raster is already masked
    #suit.ras1 <- terra::mask(suit.ras1, NA_rast)# mask using NAs from env variables 
    #suit.ras1 <- terra::mask(suit.ras1, AKrast)
    
    #if I calculate mean future distribution, and do not use Anna produced cores
    ##################################################################################################
    ## Read FUTURE rasters (not the scaled), and average them
    future.spp <- files.future[grep(paste(i), files.future)] # vector of future models RDS for 1 spp
    future.spp.list<-list() #empty list to put rasters there for staking and averging 1 spp
    for (m in future.spp) {
      future1 <- as.data.frame(readRDS(m)) #read 1 future model for 1 spp
      future1<-terra::rast(future1) # make RDS a raster
      #plot(future1)
      terra::crs(future1) <- rast.crs #assign crs to avoid warning
      #plot(future1)
      future1 <- mask(future1, bcr[1]) #MASKING TO ONLY GET VALUES WITHIN THE ECOREGIONS
      #plot(future1)
      
      future1 <- terra::mask(future1, NA_rast.crs)#mask using NAs from env variables 
      #plot(future1)
      future1 <- terra::mask(future1, AKrast)
      #plot(future1)
      
      future1 <- future1[[1]]# keep only the mean value layer, could use the 90CI later
      #plot(future1)
      #Max95<-quantile(terra::values(future1$Mean), 0.95, na.rm=TRUE) # get top 95% density of Mean for capping overestimations
      
      
      future1 <- ifel(future1 >=Max95, Max95, future1 ) # cap a Max to the q95 
      #plot(future1)
      
      future.spp.list[[m]]<-future1 #add to list for 1 spp
    }
    
    future.rasti <- terra::app(terra::rast(x = future.spp.list), "mean") # get the mean of all climate models for 1 spp future dist
    
   # plot(future.rasti)
    #names(future.rasti) <- i # assign a name, THIS BREAKS THE LATER FILTER with q75 for some reason!
    
    suit.ras1<-future.rasti # object name to follow up with code already written
    ##################################################################################################

    
    rsvals <- sort(terra::values(suit.ras1), decreasing=TRUE)
    cum_75 <- min(rsvals[cumsum(rsvals) < sum(rsvals)*0.75])
    
    q75<-cum_75
    
    #q75<-quantile(terra::values(suit.ras1), 0.25, na.rm=TRUE) #top 95% density of Mean
    suit.ras1<- dplyr::filter(suit.ras1, mean >= q75)
    
    plot(suit.ras1)
    #tryout2 <- terra::ifel(suit.ras1 >=q75,  1, NA) ## this keeps the raster, but only manipulates the values within
    
    alts <- terra::mask(elevation, suit.ras1) 
    mean.alt1 <- mean(terra::values(alts$elevation_1KMmd_GMTEDmd), na.rm = TRUE)
    
    suit.ras1 <- ifelse(terra::values(suit.ras1)>=q75, 1, NA) #gets a matrix now, using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.suit <-sum(suit.ras1, na.rm = TRUE)
    one.species <- c(k, i, "suit", mean.alt1, area.suit)
    
    altitude.mean<- rbind( altitude.mean,one.species)
    
  }
  print(paste("Finished", k))
  
}

end.time <- Sys.time()

print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
} # as is "total duration of run 69.9 mins" for only elevation extraction
# record new time after adding the current distribution raster creation and crop before the extraction of q75 

#saveRDS(altitude.mean, "data/altitude.mean.RDS")

#altitude.mean <- readRDS("data/altitude.mean.RDS")
altitude.mean <- as.data.frame(altitude.mean, row.names = FALSE)
altitude.mean$V4 <- as.numeric(altitude.mean$V4)
altitude.mean$V5 <- as.numeric(altitude.mean$V5)
names(altitude.mean) <- c("m_status", "species", "output","avg_altitude", "HQ_area" )

#summary(altitude.mean)

summary_table2 <- altitude.mean|>
  tidyr::pivot_wider(names_from = output,
                     values_from = c(avg_altitude, HQ_area)
                    )|>
  mutate(
    d_alt = round(avg_altitude_suit - avg_altitude_current,2),
    d_area =  HQ_area_suit -  HQ_area_current,
    Perc_area_d = round( (HQ_area_suit -  HQ_area_current)/HQ_area_current*100, 1),
    #Perc_area_d2 = round( (HQ_area_refxsuit -  HQ_area_current)/HQ_area_current*100, 1)
  )

altitude.mean|>
  tidyr::pivot_wider(names_from = output,
                     values_from = c(avg_altitude, HQ_area)
  )|>
  mutate(
    d_alt = round(avg_altitude_suit - avg_altitude_current,2),
    d_area =  HQ_area_suit -  HQ_area_current,
    Perc_area_d = round( (HQ_area_suit -  HQ_area_current)/HQ_area_current*100, 1),
    #Perc_area_d2 = round( (HQ_area_refxsuit -  HQ_area_current)/HQ_area_current*100, 1)
  )|>
  filter(Perc_area_d <0)|>
  pull()|>
  length()

spp_list_pub <- read.csv( "data/spp_list_pub.csv")

spp_list_pub <- spp_list_pub  %>%
  dplyr::left_join(summary_table2, by=join_by(CODE == species)) 

#write.csv(spp_list_pubWORKING, "data/spp_list_pubCOMPARISON.csv", row.names = FALSE)
