
#Chunk to extract mean elevation for FUTURE SCENARIO CORES (450 per species x 47 species)
# ACTIVATE SETDIFF filter to only do new birds and skip the ones already done in the RDS files 

{
  
  ### after getting the 2 dfs with current elevations and future elevations per each boostrapped model, run 
  ## the following to match 1 current model elevation to 9 future model elevations, and get differences
  ## the code appends at the end the current mean elevation with the differeces
  ## pooled and per ecoregion
  means.future.df <- readRDS("data/means.future.df.RDS")
  current.means.df1 <- readRDS("data/current.means.df1.RDS")
  
  # filter out the birds already done
  all.done <-unique(current.means.df1$Species)
  begin.time <- Sys.time()
  
  onebirdcur <- readRDS("data/YT Boreal Refugia Drive/bootstrapped rasters/AMRO_CurrentCores.rds")
  
  current.names<-names(onebirdcur[3:ncol(onebirdcur)])
  #rm(onecurrent)
  
  #coordinates
  xcur<-onebirdcur$x
  ycur<-onebirdcur$y
  
  rast1cur<-terra::rast(x=data.frame(x=xcur, y=ycur,value=onebirdcur[current.names[1]]), crs =  rast.crs) 
  rm(onebirdcur)
  
  #get 1 bird to get the model names and coordinates
  onebird<-readRDS("data/YT Boreal Refugia Drive/bootstrapped rasters/AMROFutureCore_df.RDS")
  
  #head(onebird)
  
  #coordinates
  x<-onebird$x
  y<-onebird$y
  
  #model names
  model.names<-names(onebird[3:length(onebird)])
  
  #length(names(onebird[3:length(onebird)]))
  
  onemodel<-model.names[1]
  
  #folder with bootstraps
  bootstraps <-"data/YT Boreal Refugia Drive/bootstrapped rasters"
  
  future.core.files <- list.files(pattern = "FutureCore", path = bootstraps, full.names = TRUE)
  
  #get 1 p/a raster to use for extent cropping of elevation raster 
  sample1<-terra::rast(x = data.frame(x=x, y=y, value = onebird[,c(onemodel)] ), crs =  rast.crs) 
  
  rm(onebird)

  #bird.elev <-  terra::rast("data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/ALFL_Refugia_RCPmean.tif")
  
  #terra::crs(bird.elev)<- rast.crs
  
  #load folder with elevation raster
  topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"
  
  #load elevation raster
  elevation <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))
  
  elevation <-  terra::crop(elevation, terra::project(NA_rast.crs, "EPSG:4326" ))
  
  elevation <-  terra::project(elevation, NA_rast.crs )
  
  elevation <-  terra::crop(elevation, sample1) # elevation raster cropped for future P/A rasters
  
  elevationcur <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))
  
  KEY<-readRDS("data/YT Boreal Refugia Drive/QPAD_Output/ALFL_Data.rds") #Should be 202712 records
  
  Clim_xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations, avoiding "re sightings"
  
  Clim_xy <- Clim_xy_frame[,c(2:3)] # only coordinates in lat lon
  
  # produce a Spatial object
  Clim_xy<-terra::vect(Clim_xy, crs = "EPSG:4326")
  
  elevationcur <-  terra::crop(elevationcur,  Clim_xy)
  
  elevationcur <-  terra::project(elevationcur, rast1cur )
  
  elevationcur <-  terra::crop(elevationcur,  rast1cur)
  
  rm(sample1, rast1cur)
  
  #crop elevation raster to only the area of interest:p/a raster extent, need to project the sample p/afirst
  #after cropping reproject elevation to crs of the study area
  
  means.future<-NULL
  
  current.core.files <- list.files(pattern = "CurrentCore", path = bootstraps, full.names = TRUE)
  
  current.means.df<-NULL
  
  #filter our birds done already
  groupings<-setdiff(groupings, all.done)
  

  
  for (i in groupings) { # process current cores
    cond <-grep(i, future.core.files) # either future or current should have the same file number
    
    if (identical(cond, integer(0))) { # this is only for the testing without ALL the birds
      print(paste0("Bird future ", i, " not found"))
    }else{
      cond <-grep(i, current.core.files) 
      if (identical(cond, integer(0))) { # this is only for the testing without ALL the birds
        print(paste0("Bird current ", i, " not found"))
      }else{
      
      onebirdcur <-readRDS(current.core.files[grep(i, current.core.files)])
      
      for (n in 1:length(current.names)) {
        print(paste("Working current", grep(i,groupings),"/", length(groupings) , i,"at", format(Sys.time(), "%X"), n,"/ 50"   ))
        
        #build one model p/a dataframe and make a raster
        rast1<-terra::rast(x=data.frame(x=xcur, y=ycur,value=onebirdcur[2+n]), crs =  rast.crs) 
        #terra::plot(rast1)
        rast1<- terra::ifel(rast1 ==1,  1, NA)
        
        #both ecoregions
        alts <- terra::mask(elevationcur, rast1) 
        mean.both<-mean(terra::values(alts), na.rm = TRUE)
        #terra::plot(alts)
        
        alt.1 <- terra::mask(alts, borealcord.crs)
        #terra::plot(alt.1)
        mean.boreal<-mean(terra::values(alt.1), na.rm = TRUE)
        
        alt.2 <- terra::mask(alts, taigacord.crs)
        #terra::plot(alt.2)
        mean.taiga<-mean(terra::values(alt.2), na.rm = TRUE)
        
        means <- c("Current",i,  mean.both, mean.boreal, mean.taiga)
        
        current.means.df <- rbind(current.means.df, means)
      }
      
      onebird <-readRDS(future.core.files[grep(i, future.core.files)])
      
      for (n in 1:length(model.names)) {
        print(paste("Working future", grep(i,groupings),"/", length(groupings) , i,"at", format(Sys.time(), "%X"), n,"/ 450"   ))
        
        #build one model p/a dataframe and make a raster
        rast1<-terra::rast(x=data.frame(x=x, y=y,value=onebird[2+n]), crs =  rast.crs) ### CROSS CHECK IF THE new 2+n works
        #terra::plot(rast1)
        rast1<- terra::ifel(rast1 ==1,  1, NA)
        
        #both ecoregions
        alts <- terra::mask(elevation, rast1) 
        mean.both<-mean(terra::values(alts), na.rm = TRUE)
        #terra::plot(alts)
        
        alt.1 <- terra::mask(alts, borealcord.crs)
        #terra::plot(alt.1)
        mean.boreal<-mean(terra::values(alt.1), na.rm = TRUE)
        
        alt.2 <- terra::mask(alts, taigacord.crs)
        #terra::plot(alt.2)
        mean.taiga<-mean(terra::values(alt.2), na.rm = TRUE)
        
        means <- c("Future",i, model.names[n], mean.both, mean.boreal, mean.taiga)
        
        means.future <- rbind(means.future, means)
      }
    }
    
  }
}
  
  end.time <- Sys.time()
  
  print(paste("total duration of raster processing", round(difftime(end.time, begin.time, units = "mins"),2), "mins"))
  
}


means.future.df <-  as.data.frame(means.future) 
names(means.future.df) <- c("Scenario", "Species", "it", "mean.both", "mean.boreal", "mean.taiga")
current.means.df1 <-  as.data.frame(current.means.df) 
names(current.means.df1) <- c("Scenario", "Species","mean.both", "mean.boreal", "mean.taiga")


means.future.dfrun2<-means.future.dfrun2
current.means.df1run2<-current.means.df1
#saveRDS(means.future.df, "data/means.future.dfrun2.RDS")
#saveRDS(current.means.df1, "data/current.means.df1run2.RDS")

fut<-rbind(means.future.dfrun2, means.future.df)
means.future.df<-fut
cur<-rbind(current.means.df1run2, current.means.df1)
current.means.df1<-cur



# Calculate the Delta Elevation and CIs pooled and per ecoregion
### after getting the 2 dfs with current elevations and future elevations per each boostrapped model, run 
## the following to match 1 current model elevation to 9 future model elevations, and get differences
## the code appends at the end the current mean elevation with the differeces
## pooled and per ecoregion

means.future.df <- readRDS("data/means.future.df.RDS") #first 14 spp version
current.means.df1 <- readRDS("data/current.means.df1.RDS") #first run with 14 spp version

# filter out the birds already done
all.done <-unique(current.means.df1$Species)



{
  
  my_seq <- seq(from =1, to = 450, by = 9)
  all_means_diffs<-NULL
  
  for (n in unique(current.means.df1$Species)) {
    currentonebird <- dplyr::filter(current.means.df1, Species== n)
    futureonebird<-dplyr::filter(means.future.df, Species == n)
    
    one_bird_diffs<-NULL
    one_bird_diffsbor<-NULL
    one_bird_diffstai<-NULL
    for (i in 1:nrow(currentonebird)) {
      
      onemeancurrent <-as.numeric(currentonebird$mean.both[i])
      
      ninemeanfuture <- as.numeric(futureonebird$mean.both[my_seq[i]:(my_seq[i]+8)])
      
      nine_diffs <- ninemeanfuture - rep(onemeancurrent, 9)
      
      one_bird_diffs<-rbind(one_bird_diffs, nine_diffs)
      
      
      
      onemeancurrentbor <-as.numeric(currentonebird$mean.boreal[i])
      
      ninemeanfuturebor <- as.numeric(futureonebird$mean.boreal[my_seq[i]:(my_seq[i]+8)])
      
      nine_diffsbor <- ninemeanfuturebor - rep(onemeancurrentbor, 9)
      
      one_bird_diffsbor<-rbind(one_bird_diffsbor, nine_diffsbor)
      
      
      onemeancurrenttai <-as.numeric(currentonebird$mean.taiga[i])
      
      ninemeanfuturetai <- as.numeric(futureonebird$mean.taiga[my_seq[i]:(my_seq[i]+8)])
      
      nine_diffstai <- ninemeanfuturetai - rep(onemeancurrenttai, 9)
      
      one_bird_diffstai<-rbind(one_bird_diffstai, nine_diffstai)
      
    }
    
    mean.bird.pool <-mean(one_bird_diffs, 0.025, na.rm = TRUE)
    lwr025.pool <- quantile(one_bird_diffs, 0.025, 0.025, na.rm = TRUE)
    upr975.pool <- quantile(one_bird_diffs, 0.975, 0.025, na.rm = TRUE)
    
    
    mean.bird.bor <-mean(one_bird_diffsbor, 0.025, na.rm = TRUE)
    lwr025.bor <- quantile(one_bird_diffsbor, 0.025, 0.025, na.rm = TRUE)
    upr975.bor <- quantile(one_bird_diffsbor, 0.975, 0.025, na.rm = TRUE)
    
    
    mean.bird.tai <-mean(one_bird_diffstai, na.rm = TRUE)
    lwr025.tai <- quantile(one_bird_diffstai, 0.025, na.rm = TRUE)
    upr975.tai <- quantile(one_bird_diffstai, 0.975, na.rm = TRUE)
    
    
    
    
    
    one.vector <- c(n, mean.bird.pool, lwr025.pool, upr975.pool, mean.bird.bor , lwr025.bor, upr975.bor, mean.bird.tai, lwr025.tai, upr975.tai)
    
    all_means_diffs <- rbind(all_means_diffs, one.vector)
    
    
    
  }
}


all_means_diffs.df <- as.data.frame(all_means_diffs, row.names = FALSE)
names(all_means_diffs.df) <- c("Species", "mean.bird.pool", "lwr025.pool", "upr975.pool", "mean.bird.bor" , "lwr025.bor", "upr975.bor", "mean.bird.tai", "lwr025.tai", "upr975.tai")
  
all_means_diffs.df<-all_means_diffs.df|>
  dplyr::mutate_at(x=all_means_diffs.df, 2:ncol(all_means_diffs.df), as.numeric)|>
  dplyr::arrange(Species)|>
  tibble::as_tibble()



### FIX THIS@@@@@@@@@@@@@@@@@@@@


#means for current and future with CI and attach the DELTA elevation and CIs
test.wide.table <- 
  current.means.df1|>
  ### CALCULATE mean VALUES and CIs for CURRENT .... and at the deltas to future 
  dplyr::mutate_at(3:length(names(current.means.df1)), as.numeric)|>
  dplyr::group_by(Species)|>
  dplyr::summarize(
    mean.pooled = mean(mean.both, na.rm = TRUE),
    lwr025.pool = quantile(mean.both, 0.025, na.rm = TRUE),
    upr975.pool = quantile(mean.both, 0.975, na.rm = TRUE),
    mean.bor = mean(mean.boreal, na.rm = TRUE),
    lwr025.bor = quantile(mean.boreal, 0.025, na.rm = TRUE),
    upr975.bor = quantile(mean.boreal, 0.975, na.rm = TRUE),
    mean.taig = mean(mean.taiga, na.rm = TRUE),
    lwr025.taig = quantile(mean.taiga, 0.025, na.rm = TRUE),
    upr975.taig = quantile(mean.taiga, 0.975, na.rm = TRUE)
    
  )|>
  dplyr::left_join(all_means_diffs.df, by='Species')|> ### adding the deltas of elevations to future
  dplyr::rename(
    "current.pooled"="mean.pooled",
    #"lwr025.pool" =
    #"upr975.pool"
     "curr.boreal" = "mean.bor",
   # "lwr025.bor" 
    #"upr975.bor" 
    "curr.taiga" = "mean.taig",
    #"lwr025.taig" 
    #"upr975.taig"  
    "d_Elev" =  "mean.bird.pool",
   "d_Elev_lwr" = "lwr025.pool.y", 
   "d_Elev_upr" = "upr975.pool.y", 
   "d_Elev.bor" = "mean.bird.bor",
    "d_Elev.bor.upr" = "lwr025.bor.y", 
    "d_Elev.bor.lwr" = "upr975.bor.y", 
    "d_Elev.tai" = "mean.bird.tai", 
   "d_Elev.tai.lwr" = "lwr025.tai" ,
   "d_Elev.tai.upr" = "upr975.tai"
  )

#write.csv(test.wide.table, "data/test.wide.tablerun2.csv")


current.means.df1|>
  dplyr::filter(Species == "YBSA")
