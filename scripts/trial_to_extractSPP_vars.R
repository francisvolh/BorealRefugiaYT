


##############################################################
##############################################################
##############################################################
### Extract mean elevation of CORE AREAS of PRESENT AND FUTURE Dist
### for all birds in a category #####
### ()
##############################################################

#Curent loop runs over all predefined groups
{
  begin.time <- Sys.time()

topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"

#sample refugia raster in clim vars CRS for EXTENT purposes
bird.elev <-  terra::rast("data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/ALFL_Refugia_RCPmean.tif")

terra::crs(bird.elev) <- rast.crs

elevation <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))

elevation <-  terra::crop(elevation, terra::project(NA_rast.crs, "EPSG:4326" ))
elevation <-  terra::project(elevation, NA_rast.crs ) # cropped to a refugia raster and in the study CRS 

####before extracting need to mask areas of raster that are not of interest
###### NA for Alaska, NA for modeling, and outside of area of interest ---  check with the team!

## crop to ONLY the area of interest (the modeled refugia raster area)
elevation <- terra::mask(elevation, NA_rast.crs) #mask using NAs from env variables 
elevation <- terra::mask(elevation, AKrast) # mask alaska extreme model areas


files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)
files.future <- list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds",  full.names = TRUE)

file.presabs <- list.files("C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/github/BorealRefugiaYT/data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/Threshold_PA_Distributions_MEAN", 
                           full.names = TRUE)

alt.area.pres.mean.spp <- NULL # to save all spp present mean elev and core area
alt.area.fut.mean.spp <- NULL #to save all spp future mean elev and core are




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

    print(paste("Working current",k, i,"at", format(Sys.time(), "%X")))
    
    
    #if I Current dist, and do not use Anna produced cores
    #get the Current dist mean alt
    
    #sample1 <- readRDS(files.vect[grep(i, files.vect)] ) #load Anna's pres/abs files instead
    #sample1 <- terra::rast(sample1[1:3])
    ###################/
    sample1<- terra::rast( file.presabs[grep(i, file.presabs)][grep("Present",file.presabs[grep(i, file.presabs)])])
    
    terra::crs(sample1) <- rast.crs
    sample1 <- terra::crop(sample1, elevation)  # crop present raster, with elevation raster
    sample1<- terra::ifel(sample1 ==1,  1, NA)
    sample1 <- terra::mask(sample1, bcr[1])
    
    alts <- terra::mask(elevation, sample1) 
    mean.alt <- mean(terra::values(alts$elevation_1KMmd_GMTEDmd), na.rm = TRUE)
 
    #tryout <- terra::ifel(sample1 >=q75,  1, NA) ## this keeps the raster, but only manipulates the values within
    
    #area.curr <-sum(terra::values(sample1), na.rm = TRUE)
    


    print(paste("Working future",k, i,"at", format(Sys.time(), "%X")))
    
    
    #get the future distributions and mean alt
    # from Anna's pres abs files
    
    #suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
    #names(suit.ras1) <- terra::varnames(suit.ras1)
    #no need to mask as the elevation raster is already masked
    #suit.ras1 <- terra::mask(suit.ras1, NA_rast)# mask using NAs from env variables 
    #suit.ras1 <- terra::mask(suit.ras1, AKrast)
    
    #if I calculate mean future distribution, and do not use Anna produced cores
    ##################################################################################################
    ## Read FUTURE rasters (not the scaled), and average them
    future.spp <-file.presabs[grep(i, file.presabs)][-grep("Present",file.presabs[grep(i, file.presabs)])] # vector of future models RDS for 1 spp
    
    future.data.alt<-NULL#empty list to put rasters there for staking and averging 1 spp
    future.data.area<- NULL
    
    for (m in future.spp) { #loops over each future scenario pres abs and gets a mean alt and mean core area
      future1<-terra::rast(m) # make RDS a raster
      #plot(future1)
      terra::crs(future1) <- rast.crs #assign crs to avoid warning
      #plot(future1)
      future1 <- terra::mask(future1, bcr) #MASKING TO ONLY GET VALUES WITHIN THE ECOREGIONS
      future1 <- terra::ifel(future1 ==1, 1, NA) # only the values of presence in the core area remain
      altfut1 <- terra::mask(elevation, future1) 
      mean.altfut1 <- mean(terra::values(altfut1$elevation_1KMmd_GMTEDmd), na.rm = TRUE)
      
      
      
      #future.spp.area <-sum(terra::values(future1), na.rm = TRUE)
      
      
      future.data.alt <-cbind(future.data.alt, mean.altfut1)
      future.data.area <-cbind(future.data.area, future.spp.area)
    }
    
    mean.one.alt <-mean(future.data.alt)
    mean.one.area <-mean(future.data.area)
    
    one.specie.all <- c(k, i,  mean.alt, #area.curr, 
                        mean.one.alt, #mean.one.area
                        )
    
    alt.area.fut.mean.spp<- rbind(alt.area.fut.mean.spp, one.specie.all)

    
    
  }
  print(paste("Finished", k))
  
}

end.time <- Sys.time()

print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
} # as is "total duration of run 69.9 mins" for only elevation extraction
# record new time after adding the current distribution raster creation and crop before the extraction of q75 

#saveRDS(altitude.mean, "data/altitude.mean.RDS")

#altitude.mean <- readRDS("data/altitude.mean.RDS")
alt.area.fut.mean.spp.DF <- as.data.frame(alt.area.fut.mean.spp, row.names = FALSE)

Elev_Shift_spp <- alt.area.fut.mean.spp.DF|>
  dplyr::select(m_status=V1,species_code=V2,avg_Cur_Elev=V3, avg_Fut_Elev=V5)|>#only if I run the area cores and get them in this DF, it has been # out
  dplyr::mutate(
    avg_Cur_Elev = as.numeric(avg_Cur_Elev),
    avg_Fut_Elev = as.numeric(avg_Fut_Elev)
  ) |>
  dplyr::mutate(
    avg_Cur_altitude = round(avg_Cur_Elev, 2),
    avg_Fut_altitude = round(avg_Fut_Elev, 2),
    Delta_Elev = avg_Fut_altitude-avg_Cur_altitude
  )|>
  dplyr::select(m_status, species_code, avg_Cur_altitude, Delta_Elev)#|>
#dplyr::arrange(Delta_Elev)


#write.csv(Elev_Shift_spp, "data/Elev_Shift_spp.csv")
Elev_Shift_spp <- read.csv("data/Elev_Shift_spp.csv")

spp_list_pub <- read.csv( "data/spp_list_pub.csv")

spp_list_pub <- spp_list_pub |>
  dplyr::left_join(Elev_Shift_spp, by= dplyr::join_by(CODE == species_code)) |>
  dplyr::select("CODE","ENGLISH.NAME","SCIENTIFIC.NAME","Migra_status", "included_or_dropped", "avg_Cur_altitude", "Delta_Elev")

#write.csv(spp_list_pubWORKING, "data/spp_list_pubCOMPARISON.csv", row.names = FALSE)

##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### 
##### add top 5 influence vars per spp
##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### 
top5spp <- read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/influence/Top5Influence.csv")

top5spp.wide <- top5spp|>
  dplyr::group_by(Spp)|>
  dplyr::summarise(
    variables = paste(unique(ClimateVar), collapse = ', ')
                  )

spp_list_pub <- spp_list_pub |>
  dplyr::left_join(top5spp.wide, by= dplyr::join_by(CODE == Spp))
