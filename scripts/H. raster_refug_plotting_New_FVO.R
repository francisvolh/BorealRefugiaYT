library(ggplot2)
library(dplyr)
library(terra)
library(sf)
library(tidyterra)
library(leaflet)

#################################################################################### 
################### Produce group classification to sum and plot ################### 

{
  ####### 1) prep files and folders
  
#load classification file and make make vectors with codes
#class_spp <- read.csv("data/SpeciesStatus.csv")

#unique(class_spp$Migration1)

  #### 
  #### CHSP, LISP, OCWA, TRES as short dist
  #### 
  
df.spp.names.merged <- read.csv("data/df.all.birds.merged - CHSP LISP OCWA.csv", #stringsAsFactors = TRUE, 
                                na.strings=c("NA","NaN", ""))



df.spp.names.merged <- df.spp.names.merged |>
  dplyr::filter(is.na(dropped))

LDM <- df.spp.names.merged |>
   dplyr::filter(is.na(dropped))|> 
   dplyr::filter(Migration1 == "Neotropical migrant") |> 
  dplyr::select(all.birds)|> 
  dplyr::pull()

SDM <- df.spp.names.merged |> 
   dplyr::filter(is.na(dropped)) |> 
   dplyr::filter(Migration1 == "Short distance migrant")|> 
  dplyr::select(all.birds) |>
  dplyr::pull()

RES <- df.spp.names.merged |>
   dplyr::filter(is.na(dropped)) |> 
   dplyr::filter(Migration1 == "Resident") |>
  dplyr::select(all.birds) |> 
  dplyr::pull()

NOM <- df.spp.names.merged |>
   dplyr::filter(is.na(dropped)) |> 
  # dplyr::filter(species_code %in% spp.to.work) %>% 
   dplyr::filter(Migration1 == "Nomadic")|> 
  dplyr::select(all.birds) |> 
  dplyr::pull()

N_assig <- df.spp.names.merged |> 
   dplyr::filter(is.na(dropped)) |>  
  # dplyr::filter(species_code %in% spp.to.work) %>% 
   dplyr::filter(is.na(Migration1))|> 
  dplyr::select(all.birds) |> 
  dplyr::pull()

groupings_labs <- c("RES","LDM","SDM"#,"NOM"#,"N_assig"
                    )
groupings <- c(RES,LDM,SDM#,NOM#,N_assig
               )

max.len <- max(length(LDM),length(SDM),length(RES))
SDM.spp = c(SDM, rep(NA, max.len - length(SDM)))
LDM.spp = c(LDM, rep(NA, max.len - length(LDM)))
RES.spp = c(RES, rep(NA, max.len - length(RES)))
cat.spp.df<-data.frame(SDM.spp, LDM.spp, RES.spp)
#write.csv(cat.spp.df, "data/cat.spp.df.csv")
num.spp <- c(length(RES), length(LDM), length(SDM))

#load maps
NA_CEC_Eco_Level2 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_Terrestrial_Ecoregions_v2_Level_II_Shapefile/NA_Terrestrial_Ecoregions_v2_level2.shp") #### new 2021 version HUGE SHAPEFILE
BCR4.1_USACAN<- NA_CEC_Eco_Level2|>
   dplyr::filter(NameL2_En == "Taiga Cordillera")

BCR4.0_USACAN<- NA_CEC_Eco_Level2|>
   dplyr::filter(NameL2_En == "Boreal Cordillera")

level3<- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_Terrestrial_Ecoregions_v2_Level_III_Shapefile/NA_Terrestrial_Ecoregions_v2_level3.shp")




#BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
#BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_1.shp")
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")

#set up foder routes
####   "data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/ALFL_Refugia_RCPmean.tif"
files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)

#set refugia raster results directory
#the file list
files.to.read<-list.files("data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/", full.names = TRUE)

#for increasing decreasing population -- WHEN NEEDED, not yet
#files.future <- list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds", full.names = TRUE )

#the directory route
ref.ras.dir <- "data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/"


# produce a raster to clean Ref, suit, and refxsuit rasters
rast.crs<- terra::crs(terra::rast("data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif")) ## from data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif
Norm1991<-readRDS("data/corrected_rasters_clim_topo/all_rasts_Correct_Ecozones.RDS") #test with my file
Norm1991<-readRDS("data/Norm1991_EcozoneNormals.rds") ## THE ECOZONES FILE FROM ANNA
NAKey<-subset(Norm1991,!is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,!is.na(NAKey$tri))
NA_rast<-terra::rast(NAKey , type="xyz")
NA_rast <-NA_rast[[1]]

NA_rast <- terra::ifel(is.na(NA_rast), NA, 1) # NA are ocean and high climate modeled values (nonsensical)
#load a bird refugia raster
bird_rast <- terra::rast("data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/ALFL_Refugia_RCPmean.tif")
terra::crs(bird_rast)<-rast.crs
NA_rast <-terra::crop(NA_rast,bird_rast)


NA_rast.crs <- NA_rast
terra::crs(NA_rast.crs) <- rast.crs
rm(NAKey) 
rm("Norm1991")


ecor_713 <- level3|>
  dplyr::filter(LEVEL3 == "7.1.3")|>
  sf::st_transform(rast.crs)

eco_3.1 <- NA_CEC_Eco_Level2 |>
  dplyr::filter(LEVEL2 == "3.1")|>
  sf::st_transform(rast.crs)

# area of eco 3.1 outside the previous rasters
#eco31_out <- sf::st_difference(eco_3.1, usa_crop) 

#usa_can_crop <- sf::st_union(canada, USA) # dont do this, keep each country separate

usa_crop <- sf::st_transform(USA, rast.crs )

usa_crop <- sf::st_crop(usa_crop, c(xmin=as.numeric(terra::ext(bird_rast)[1]), 
                                        xmax=as.numeric(terra::ext(bird_rast)[2]), 
                                        ymin=as.numeric(terra::ext(bird_rast)[3]), 
                                        ymax=as.numeric(terra::ext(bird_rast)[4])
))

canada_crop <- sf::st_transform(canada, rast.crs )

canada_crop <- sf::st_crop(canada_crop, c(xmin=as.numeric(terra::ext(bird_rast)[1]), 
                                xmax=as.numeric(terra::ext(bird_rast)[2]), 
                                ymin=as.numeric(terra::ext(bird_rast)[3]), 
                                ymax=as.numeric(terra::ext(bird_rast)[4])
))
#rm(canada, USA)
can_us_crop <- sf::st_union(canada_crop, usa_crop)

#NA_rast.crs_crop <- terra::mask(NA_rast.crs, can_us_crop)

#create ocean ploygon for loop plotting later
poly <- can_us_crop |>
  sf::st_as_sf(coords = c("lon", "lat")) |> 
  sf::st_bbox() |> 
  sf::st_as_sfc() |> 
  sf::st_transform(rast.crs)


### produce a key to clean present distribution rasters, will work on RDS files, before rasterizing it to plot, 
###### but not needed for me
#NAKey<-paste(NAKey$x,NAKey$y,sep=".") #raster squares with complete covariates only

# Import key of masked AK regions (AK portion of BCR 3 + BCR 2) -------------------------------------
AK_remove<-read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/AK_removalregion.csv")
AK_remove<-terra::rast(AK_remove[,2:4] , type="xyz")
AKrast <- AK_remove
AKrast<- terra::ifel(is.na(AKrast), 10, NA) # assign NA to the coordinates for Alsaka bad-modelled areas, to mask out later
#AKrast <- terra::ifel(is.na(AKrast), 1, NA) # this is not needed as terra::mask will use the NAs, independent of the value
terra::crs(AKrast) <- rast.crs
AKrast <-terra::crop(AKrast, NA_rast)
rm(AK_remove)



#AA<-ggplot()+
  #geom_sf(data = usa_can_crop)+
 # geom_spatraster(data = AK_remove)

#BB<-ggplot()+
 # geom_spatraster(data = AK_remove)+
  #geom_sf(data = usa_can_crop, fill = "yellow", alpha = 0.3)
#cowplot::plot_grid(AA, BB, nrow = 1)         
#AK_Key<-paste(AK_remove$x,AK_remove$y,sep=".")

### adding calculation of BCR 4.1 and 4.0 area
BCR4.1_4.0 <- sf::st_union(BCR4.1_USACAN, BCR4.0_USACAN)
BCR4.1_4.0 <- sf::st_union(BCR4.1_4.0)
area.bcr<-sf::st_area(BCR4.1_4.0)

units(area.bcr) <- units::as_units("km2")

bcr <- BCR4.1_4.0[1]
bcr <- sf::st_as_sf(bcr)
bcr <- sf::st_transform(bcr, crs =rast.crs) # shapefile of the 2 ecoregions of interest


taigacord.crs<- NA_CEC_Eco_Level2|>
  dplyr::filter(NameL2_En == "Taiga Cordillera")|>
  sf::st_transform(rast.crs)

borealcord.crs<- NA_CEC_Eco_Level2|>
  dplyr::filter(NameL2_En == "Boreal Cordillera")|>
  sf::st_transform(rast.crs)

#area.bcr
###



alaska.pas <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/CEC_NA_PA_GEO_09_09_USA_ALASKA_1/CEC_NA_PA_GEO_09_09_USA_ALASKA_1.shp")


#only IUCN categorized PAs
alaska.pas_crs<-alaska.pas|>
  dplyr::filter(IUCNCAT != "Unkno")|>
  dplyr::filter(IUCNCAT != "VI")|>
  sf::st_transform(rast.crs)|>
  dplyr::select(CEC_NA_ID, PA_NAME, IUCNCAT, STATE_PROV)
#unique(alaska.pas_crs$PA_NAME)
#length(unique(alaska.pas_crs$PA_NAME))

CPCAD <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/CPCAD extracted/CPCAD_extracted.shp")

crs_CPCAD <-terra::crs(CPCAD)

cpcad_bird_rast <- bird_rast|>
  terra::project(crs_CPCAD)

#filter for only BC, YT and NWT
CPCAD1 <- CPCAD|>
  sf::st_crop(cpcad_bird_rast)|>
  dplyr::filter(IUCN_CAT %in% c("Ib" , "III", "II" , "Ia" , "IV" )) |>
  dplyr::filter(LOC_E %in% c("Yukon" , "British Columbia", "Northwest Territories")) |>
  sf::st_transform(rast.crs)

#unique(CPCAD1$NAME_E)
#length(unique(CPCAD1$NAME_E))

single_sf <- dplyr::bind_rows(list(alaska.pas_crs, CPCAD1))
dissolve_sf <- sf::st_union(single_sf)
dissolve_sf<-sf::st_intersection(dissolve_sf , bcr)
dissolve_sf <-sf::st_union(dissolve_sf)
area.yukon_PAs_crs <- dissolve_sf
yukon_PAs_crs <- dissolve_sf
yukon_PAs_crs <- sf::st_transform(yukon_PAs_crs, rast.crs)
yukon_PAs_crs<- sf::st_as_sf(yukon_PAs_crs)

area.yukon_PAs_crs<-sf::st_area(area.yukon_PAs_crs)
units(area.yukon_PAs_crs) <- units::as_units("km2")
#area.yukon_PAs_crs

yukon <- canada_crop|>
   dplyr::filter(NAME_1 == "Yukon")|>
  sf::st_transform(rast.crs)
}
##########################################################################
############ for leaflet plotting only
#####
shapeData1 <- sf::st_transform(BCR4.1_USACAN, "EPSG:4326")
shapeData2 <- sf::st_transform(BCR4.0_USACAN, "EPSG:4326")

leaflet::leaflet()|>
  addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_terrain_background/{z}/{x}/{y}{r}.png",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                        '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                        '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                        '&copy; <a href="https://www.openstreetmap.org/about" target="_blank">OpenStreetMap</a> contributors'),
    options = tileOptions(variant='stamen_toner_lite', apikey = '76a5a6d3-2a88-4129-a816-849bdbaebd56') )|>
  addPolygons(data = shapeData1)|>
  addPolygons(data = shapeData2)


#  76a5a6d3-2a88-4129-a816-849bdbaebd56


################################################################################################
################################################################################################
### 2) PRODUCE STACK GRAPHS FOR CURRENT SUITABILITY, REFUGIA, FUTURE SUITABILITY, and SUITABLE REFUCIA
### including calculation of high value areas surface coverage of ecoregions and  PAs
### for GROUPING categories 
#########################################################################
{
#Curent loop runs over all predefined groups
#par(mfrow = c(3,2))

  
  
  group_area_values<- NULL # dataframe, will hold the q75 surface area values for each migratory group, per raster stack 
  pa_group_area_values <- NULL # df will hold PA values the p75 surface area values for each migra group per raster stack
  all.group.rasters<-NULL # list, will hold 3 lists, one per migra group (three cats), with 4 stacked raster each
  all.groups.sd.rasters<-NULL
  stacks.mean.vals <- NULL
  high.pa.vals <- NULL
  p75s<- NULL
  startnum<-NULL
  categoryCol <- NULL
  
  #loop for to run all groups
  begin.time <- Sys.time()
  for (k in groupings_labs) {
    
    #for one group
    
    group_spp <- get(k)
    
    three.cats <- NULL# resets in each grouping run, will hold the 4 rasters per migra group 
    #three.cats.vals <- NULL # not needed anymore, just throw directly into the group_area_values list
    three.cats.names <- NULL# resets in each grouping run, will hold the 4 rasters Names per migra group
    

    #skipping this object
    #three.cats.vals <- NULL # will be a list, will hold the 4 surface area values for each migra iteration (curr, ref, suit, ref x suit)
    
    curr.mean.list <- NULL # holds the spp rasters per migra group, resets for each grouping run - k iteration in the loop
    ref.mean.list <- NULL # resets for each grouping run - k iteration in the loop
    suit.mean.list <- NULL # resets for each grouping run - k iteration in the loop
    refxsuit.mean.list <- NULL# resets for each grouping run - k iteration in the loop
    
    print(paste("Processing", k, "species"))
    for (i in c(group_spp)) {
      startnum[[i]]<-i
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "current dist", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      #load current distributions RDS
      rast1 <- readRDS(files.vect[grep(i, files.vect)] )
      Max95<-quantile(rast1$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
      
      rast1 <- rast1 |>
        dplyr::mutate(
          std.mean = Mean/Max95
        )
      
      rast1<-terra::rast(rast1, crs =  rast.crs)  
     
       rast1 <- rast1[[4]] # dplyr::select only the Mean values layer, not the standardized/scaled anymore [[4]]
     
      rast1 <- terra::ifel(rast1 >=1, 1, rast1 ) # cap to a max of 1
      #terra::crs(rast1) <- rast.crs # assign a crs to avoid warning
      curr.mean.list[[i]] <- rast1
      rm(rast1)
      
      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "refugia", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      #loads the refugia rasters
      #rasters are sometimes tif or tiff, so the grep() solves it
      ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
      terra::crs(ref.ras1) <- rast.crs # assign crs to avoid warning
      ref.mean.list[[i]] <- ref.ras1
      rm(ref.ras1)

      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "future dist", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
      terra::crs(suit.ras1) <- rast.crs # assign CRS to avoid warning
      suit.mean.list[[i]] <- suit.ras1
      rm(suit.ras1)
      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) ,"ref x future", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
      terra::crs(refxsuit.ras1) <- rast.crs # assign CRS to avoid warning
      refxsuit.mean.list[[i]] <- refxsuit.ras1
      rm(refxsuit.ras1)
    }
    
    
    ######
    ######
    ######
    # STACKING 
    # for all RES, LDM, SDM did sum
    #for SDM also did average to compare that visually look the same, Diana agrees they are the same
    #par(mfrow = c(1,3))
    print(paste("Working stacked",k, "current", format(Sys.time(), "%X") ))
    
    curr.sum <- terra::app(terra::rast(curr.mean.list), "sum")#/length(ref.mean.list)
    

      
    terra::crs(curr.sum) <- rast.crs
    
    curr.sum <- terra::crop(curr.sum, NA_rast.crs)
    curr.sum <- terra::mask(curr.sum, NA_rast.crs)
    
    curr.sum <- terra::mask(curr.sum, AKrast)
    curr.sum.y <- terra::mask(curr.sum, bcr[1])
    
    curr.sd <- terra::app(terra::rast(curr.mean.list), "sd")#/length(ref.mean.list)
    curr.sd <- terra::crop(curr.sd, NA_rast.crs)
    curr.sd <- terra::mask(curr.sd, NA_rast.crs)
    
    curr.sd <- terra::mask(curr.sd, AKrast)
    
    
    # quantile may always be selecting the same number of cells???????
    #q75 <- quantile(terra::values(curr.sum.y), probs=c(0.75), na.rm=TRUE)
    
    #try the 75% of high values threshold
    p75 <- ceiling((.50*length(group_spp))) # always choose a whole number up (even if 0.1 over the integer)
    #max(terra::values(curr.sum.y), na.rm = TRUE) - max(terra::values(curr.sum.y), na.rm = TRUE)/4
    curr.sum.test <- terra::ifel(curr.sum.y>=p75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.curr <-sum(terra::values(curr.sum.test), na.rm = TRUE)
    

    #plot(curr.sum.test)
    
    four_area_values<-NULL
    four_area_values <- cbind( four_area_values, area.curr)
    rm(area.curr)
    
   
    ######
    ######
    #stack cropped to PAs only
    cropped_rast_yt <- terra::mask(curr.sum.y, yukon_PAs_crs)
    
    
    sum.y.mean <-mean(terra::values(curr.sum.y), na.rm = TRUE)
    sum.y.sd <-sd(terra::values(curr.sum.y), na.rm = TRUE)
    sum.y.min <- min(terra::values(curr.sum.y), na.rm = TRUE)
    sum.y.max <- max(terra::values(curr.sum.y), na.rm = TRUE)
    sum.y.PAmean <-mean(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAsd <-sd(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmin <- min(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmax <- max(terra::values(cropped_rast_yt), na.rm = TRUE)
    stacks.mean.vals <- rbind( stacks.mean.vals, data.frame(sum.y.mean, sum.y.sd, sum.y.min, sum.y.max, sum.y.PAmean, sum.y.PAsd, sum.y.PAmin, sum.y.PAmax))
    
    #use the same p75 of the overall ecoregion to calculate this high qual area
    area.cropped_rast_ytSDM <- terra::ifel(cropped_rast_yt>=p75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.cropped_rast_ytSDM <-sum(terra::values(area.cropped_rast_ytSDM), na.rm = TRUE)
    
    PA_four_area_values<-NULL
    PA_four_area_values <- cbind( PA_four_area_values, area.cropped_rast_ytSDM)
    
    
    print(paste("Working stacked",k, "refugia",format(Sys.time(), "%X") ))
    
    ref.sum <- terra::app(terra::rast(ref.mean.list), "sum")#/length(ref.mean.list)
    ref.sum <- terra::mask(ref.sum, NA_rast.crs)#need to apply the NA mask to refugia, because it has zeros where NA should be
    ref.sum <- terra::mask(ref.sum, AKrast)
    
    terra::crs(ref.sum) <- rast.crs
    
    ref.sum.y <- terra::mask(ref.sum, bcr[1])
    
    
    ref.sd <- terra::app(terra::rast(ref.mean.list), "sd")#/length(ref.mean.list)
    ref.sd <- terra::crop(ref.sd, NA_rast.crs)
    ref.sd <- terra::mask(ref.sd, NA_rast.crs)
    
    ref.sd <- terra::mask(ref.sd, AKrast)
   
    
    #quantile may always be selecting the same number of cells??????
    #q75 <- quantile(terra::values(ref.sum.y), probs=c(0.75), na.rm=TRUE)  
    
    #try the 75% of high values threshold
    #p75 <- max(terra::values(ref.sum.y), na.rm = TRUE) - max(terra::values(ref.sum.y), na.rm = TRUE)/4
  
    ref.sum.test <- terra::ifel( ref.sum.y >= p75 , 1 , NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    #plot(ref.sum.test)
    area.ref <-sum(terra::values(ref.sum.test), na.rm = TRUE)
    
    four_area_values <- cbind( four_area_values, area.ref)
    rm(area.ref )


    #stack cropped to PAs only
    cropped_rast_yt <- terra::mask(ref.sum.y, yukon_PAs_crs)
    
    sum.y.mean <-mean(terra::values(ref.sum.y), na.rm = TRUE)
    sum.y.sd <-sd(terra::values(ref.sum.y), na.rm = TRUE)
    sum.y.min <- min(terra::values(ref.sum.y), na.rm = TRUE)
    sum.y.max <- max(terra::values(ref.sum.y), na.rm = TRUE)

    sum.y.PAmean <-mean(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAsd <-sd(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmin <- min(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmax <- max(terra::values(cropped_rast_yt), na.rm = TRUE)
    stacks.mean.vals <- rbind( stacks.mean.vals, data.frame(sum.y.mean, sum.y.sd, sum.y.min, sum.y.max, sum.y.PAmean, sum.y.PAsd, sum.y.PAmin, sum.y.PAmax))
    
    
    area.cropped_rast_ytREF <- terra::ifel(cropped_rast_yt>=p75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.cropped_rast_ytREF <-sum(terra::values(area.cropped_rast_ytREF), na.rm = TRUE)
    PA_four_area_values <- cbind( PA_four_area_values, area.cropped_rast_ytREF)
    
    
    print(paste("Working stacked",k, "suitability",format(Sys.time(), "%X") ))
    
    #suitability
    suit.sum <- terra::app(terra::rast(suit.mean.list), "sum")#/length(suit.mean.list)
    terra::crs(suit.sum) <- rast.crs
    suit.sum.y <- terra::mask(suit.sum, NA_rast.crs)
    suit.sum.y <- terra::mask(suit.sum.y, AKrast)
    suit.sum.y <- terra::mask(suit.sum.y, bcr[1])
    
    suit.sd <- terra::app(terra::rast(suit.mean.list), "sd")#/length(ref.mean.list)
    suit.sd <- terra::crop(suit.sd, NA_rast.crs)
    suit.sd <- terra::mask(suit.sd, NA_rast.crs)
    
    suit.sd <- terra::mask(suit.sd, AKrast)
    
  
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    #q75 <- quantile(terra::values(suit.sum.y), probs=c(0.75), na.rm=TRUE)
    #try the 75% of high values threshold
    #p75 <- max(terra::values(suit.sum.y), na.rm = TRUE) - max(terra::values(suit.sum.y), na.rm = TRUE)/4
    
    suit.sum.test <- terra::ifel(suit.sum.y >=p75 , 1 , NA)
    #plot(suit.sum.test)
    area.suit <-sum(terra::values(suit.sum.test), na.rm = TRUE)
    four_area_values <- cbind( four_area_values, area.suit)
    rm( area.suit )
    
   #stack cropped to PAs only
   cropped_rast_yt <- terra::mask(suit.sum.y, yukon_PAs_crs)
   
   sum.y.mean <-mean(terra::values(suit.sum.y), na.rm = TRUE)
   sum.y.sd <-sd(terra::values(suit.sum.y), na.rm = TRUE)
   sum.y.min <- min(terra::values(suit.sum.y), na.rm = TRUE)
   sum.y.max <- max(terra::values(suit.sum.y), na.rm = TRUE)
   sum.y.PAmean <-mean(terra::values(cropped_rast_yt), na.rm = TRUE)
   sum.y.PAsd <-sd(terra::values(cropped_rast_yt), na.rm = TRUE)
   sum.y.PAmin <- min(terra::values(cropped_rast_yt), na.rm = TRUE)
   sum.y.PAmax <- max(terra::values(cropped_rast_yt), na.rm = TRUE)
   stacks.mean.vals <- rbind( stacks.mean.vals, data.frame(sum.y.mean, sum.y.sd, sum.y.min, sum.y.max, sum.y.PAmean, sum.y.PAsd, sum.y.PAmin, sum.y.PAmax))
   
   
   
   area.cropped_rast_ytSUIT <- terra::ifel(cropped_rast_yt>=p75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
   area.cropped_rast_ytSUIT <-sum(terra::values(area.cropped_rast_ytSUIT), na.rm = TRUE)
   
   PA_four_area_values <- cbind( PA_four_area_values, area.cropped_rast_ytSUIT)
   
    print(paste("Working stacked",k, "ref x suitability",format(Sys.time(), "%X") ))
    
    refxsuit.sum <- terra::app(terra::rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)
    terra::crs(refxsuit.sum) <- rast.crs
    refxsuit.sum.y <- terra::mask(refxsuit.sum, NA_rast.crs)
    refxsuit.sum.y <- terra::mask(refxsuit.sum.y, AKrast)
    refxsuit.sum.y <- terra::mask(refxsuit.sum.y, bcr[1])
    
    
    refxsuit.sd <- terra::app(terra::rast(refxsuit.mean.list), "sd")#/length(ref.mean.list)
    refxsuit.sd <- terra::crop(refxsuit.sd, NA_rast.crs)
    refxsuit.sd <- terra::mask(refxsuit.sd, NA_rast.crs)
    
    refxsuit.sd <- terra::mask(refxsuit.sd, AKrast)
    
    
    #q75 <- quantile(terra::values(refxsuit.sum.y$sum), probs=c(0.75), na.rm=TRUE)
   # p75 <- max(terra::values(refxsuit.sum.y), na.rm = TRUE) - max(terra::values(refxsuit.sum.y), na.rm = TRUE)/4
    
    refxsuit.test <- terra::ifel( refxsuit.sum.y >= p75 , 1 , NA )
    area.refxsuit <-sum(terra::values(refxsuit.test), na.rm = TRUE)
    four_area_values <- cbind( four_area_values, area.refxsuit)
    rm( area.refxsuit )
    
    #stack cropped to PAs only
    cropped_rast_yt <- terra::mask(refxsuit.sum.y, yukon_PAs_crs)
    
    sum.y.mean <-mean(terra::values(refxsuit.sum.y), na.rm = TRUE)
    sum.y.sd <-sd(terra::values(refxsuit.sum.y), na.rm = TRUE)
    sum.y.min <- min(terra::values(refxsuit.sum.y), na.rm = TRUE)
    sum.y.max <- max(terra::values(refxsuit.sum.y), na.rm = TRUE)
    sum.y.PAmean <-mean(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAsd <-sd(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmin <- min(terra::values(cropped_rast_yt), na.rm = TRUE)
    sum.y.PAmax <- max(terra::values(cropped_rast_yt), na.rm = TRUE)
    stacks.mean.vals <- rbind( stacks.mean.vals, data.frame(sum.y.mean, sum.y.sd, sum.y.min, sum.y.max, sum.y.PAmean, sum.y.PAsd, sum.y.PAmin, sum.y.PAmax))
    
    area.cropped_rast_ytREFxSUIT <- terra::ifel(cropped_rast_yt>=p75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.cropped_rast_ytREFxSUIT <-sum(terra::values(area.cropped_rast_ytREFxSUIT), na.rm = TRUE)
    
    PA_four_area_values <- cbind( PA_four_area_values, area.cropped_rast_ytREFxSUIT)
    
    p75s <- cbind(p75s, p75)
    
    #make sets of plots of only the 4 categories (curr, ref, suit, ref x suit) per species grouping (res, ldm, sdm)
    three.cats  <- list(curr.sum, ref.sum, suit.sum, refxsuit.sum) # list of sacked rasters for each group iteration TO PLOT
    
    four.sds.cats  <- list(curr.sd, ref.sd, suit.sd, refxsuit.sd)
    
    #need the names for ggplots, may ditch later for pub
  
    category <- c(paste0(k,".current.sum"), paste0(k,".refugia.sum"),paste0(k,".future.sum"),paste0(k,".refugia x future.sum")) # list of names
    
   
    
    categoryCol <- cbind(categoryCol, category)    
    
    
    
    all.group.rasters[[k]]<- three.cats # list of 4 groups raster, with all 3 migra groups
    
    all.groups.sd.rasters[[k]] <- four.sds.cats
    
    class(all.group.rasters)
    #group_area_values[[k]]<- list(area.curr, area.ref, area.suit, area.refxsuit ) # compile values of areas for calculations of percentage table
   #switching this list for a rbind
    group_area_values <- rbind(group_area_values, four_area_values)
    #rm( four_area_values )
    pa_group_area_values <- rbind(pa_group_area_values, PA_four_area_values)
    
    
    # make a cbind ones I get all the protected areas to USE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #high.pa.vals[[k]] <- list(area.cropped_rast_ytSDM, area.cropped_rast_ytREF, area.cropped_rast_ytSUIT, area.cropped_rast_ytRxS)

  end.time <- Sys.time()
  
  print(paste("total duration of raster processing", round(difftime(end.time, begin.time, units = "mins"),2), "mins"))


  }# end of group loop
  
  
  } # end of calculation run
# Calculate percentages for surface of high quality areas over all ecoregion and YT Protected area system

saved_means_stacks<-stacks.mean.vals

stacks.mean.vals$category <- c(categoryCol[,1],categoryCol[,2] ,categoryCol[,3])

#mean pixel value and descriptive stats per stack
stacks.mean.vals|>
  dplyr::mutate_at(1:length(stacks.mean.vals)-1, round, 2)|>
  dplyr::select(category, sum.y.mean, sum.y.sd, sum.y.min, sum.y.max, sum.y.PAmean, sum.y.PAsd, sum.y.PAmin, sum.y.PAmax)
  

group_area_values
pa_group_area_values

###
###
###

 #Produce dataframes with high quality areas overall and withing protected areas, per migra group
  {
    df <- as.data.frame(group_area_values)
 
#produce table with area coverage of good refugia
df$Category <- groupings_labs
names(df) <- c( "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit", "Category")

high.areas<-df[ , c("Category", "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit")]


# re calculating BCR are based on the actual modelled area within the BCR (excluded the NAd pixels to avoid over estimation of the %)
cropped_rast_bcr <- terra::mask(bird_rast, bcr)# grab one raster, to get the modeled area using each pixel

#plot(cropped_rast_bcr)

for_calc_bcr <- cropped_rast_bcr
for_calc_bcr <-  terra::crop(for_calc_bcr, NA_rast.crs) #crop to final area
for_calc_bcr <- terra::mask(for_calc_bcr, NA_rast.crs)#mask using NAs from env variables 
for_calc_bcr <- terra::mask(for_calc_bcr, AKrast)
for_calc_bcr <- terra::ifel(for_calc_bcr >0, 1, NA)
#plot(for_calc_bcr)
area.bcr_for_calc<-sum(terra::values(for_calc_bcr), na.rm = TRUE)

high.areas$Pres.Percent <- high.areas$Present/ as.numeric(area.bcr_for_calc)*100
high.areas$Ref.Percent <- high.areas$Refugia/ as.numeric(area.bcr_for_calc)*100
high.areas$Fut.Suitable.Perc <- high.areas$Fut.Suitable/ as.numeric(area.bcr_for_calc)*100
high.areas$RefxFut.Perc <- high.areas$RefxFut.Suit/ as.numeric(area.bcr_for_calc)*100
high.areas
#write.csv(high.areas, "data/high.areasv2.csv")



###
### FIX the PA section making a rbind
###
for_calc <- cropped_rast_yt # grab the last raster produced of a stack for only PA masks

for_calc <- terra::ifel(for_calc >0, 1, NA)
#plot(for_calc)
area.for_calc<-sum(terra::values(for_calc), na.rm = TRUE) # exclude the NAs for models and Alaska

df2<-as.data.frame(pa_group_area_values)
df2$Category <- groupings_labs

names(df2) <- c( "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit", "Category")

high.pa.area <- df2[, c( "Category", "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit")]

high.pa.area$Pres.Percent <- high.pa.area$Present/ as.numeric(area.for_calc)*100
#high.pa.area$Ref.Percent <- high.pa.area$Refugia/ as.numeric(area.for_calc)*100
#high.pa.area$Fut.Suitable.Perc <- high.pa.area$Fut.Suitable/ as.numeric(area.for_calc)*100
high.pa.area$RefxFut.Perc <- high.pa.area$RefxFut.Suit/ as.numeric(area.for_calc)*100



high.pa.area$Pres.Percent <- high.pa.area$Present/ as.numeric(area.for_calc)*100
high.pa.area$Pres.Percent_ECO <- high.pa.area$Present/ as.numeric(area.bcr_for_calc)*100
##high.pa.area$Pres.Dif_Percent_PAECO <- (high.pa.area$Present-high.areas$Present)/ high.areas$Present*100

#high.pa.area$Ref.Percent <- high.pa.area$Refugia/ as.numeric(area.for_calc)*100
#high.pa.area$Fut.Suitable.Perc <- high.pa.area$Fut.Suitable/ as.numeric(area.for_calc)*100

high.pa.area$RefxFut.Perc <- high.pa.area$RefxFut.Suit/ as.numeric(area.for_calc)*100
high.pa.area$RefxFut.Perc_ECO <- high.pa.area$RefxFut.Suit/ as.numeric(area.bcr_for_calc)*100

##high.pa.area$RefxFut.Suit.Dif_Percent_PAECO <- (high.pa.area$RefxFut.Suit-high.areas$RefxFut.Suit)/ high.areas$RefxFut.Suit*100



print(high.areas)
print(high.pa.area)

#write.csv(high.pa.area, "data/high.pa.areav2.csv")

}

###########################################################################
###########################################################################
############### PLOTS #####################################################

# Make the plots for curr, ref, suit, and ref x suit, per migra group 
# all in one 3 x 4 panel


#LOOP for plots of stacks 
{ begin.time <- Sys.time()

rast.cat.names<- c("Sum Current Suitable Habitat","Sum Refugia Probability","Sum Future Suitable Habitat","Sum Future Suitable Refugia")
group_plots<- NULL # list, will hold the  3 cowplots (of 4 plots) per migratory iteration, for plotting and saving at the end

three.cat.list<- NULL # for the 3 plots of each iteration
#groupings_labs

turbo_pal <- c(viridis::turbo(n = 1000, direction = -1))

for (m in 1:length(all.group.rasters)) {
  
  three.cats <- all.group.rasters[[m]]
  
  print(paste("Plotting", groupings_labs[m]))
  
  current<-three.cats[[1]]
  
  max.val <- max(terra::values(current), na.rm = TRUE)
  min.val <- min(terra::values(current), na.rm = TRUE)
  
  for (j in 1:length(three.cats)) {
    
    sum.raster <- three.cats[[j]]# a raster
    
    # if (j == 1) {
    
    plot.one <-    ggplot2::ggplot()+
      ggplot2::geom_sf(data = poly, fill = "grey") +
      ggplot2::geom_sf(data = usa_crop, fill = "white")+
      ggplot2::geom_sf(data = canada_crop, fill = "white")+
      tidyterra::geom_spatraster(data =sum.raster)+
      ggplot2::geom_sf(data = usa_crop, alpha =0)+
      ggplot2::geom_sf(data = canada_crop, alpha =0)+       
      ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
      ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
      ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                        ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                        expand = FALSE)+
      ggplot2::theme_bw()+
      #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
      ggplot2::scale_fill_gradientn(
        na.value = "transparent",
        colors = c(
          turbo_pal 
        ),
        values = scales::rescale(
          sort(c(range(terra::values(sum.raster)), c(0,  num.spp[m]))),
          to = c(0, 1)
        ),
        oob = scales::squish,
        limits = c(0, num.spp[m])
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
      )+
      ggplot2::ggtitle(paste(groupings_labs[m], rast.cat.names[[j]]))
    
    print(paste("Plotting",groupings_labs[m], rast.cat.names[[j]], format(Sys.time(), "%X") ))
    
    
    
    three.cat.list[[j]] <- plot.one # lists the four plots per category
    
  }
  print(paste("Grouping three sets plots",groupings_labs[m], format(Sys.time(), "%X") ))
  
  #cowplot object with 4 maps of each migra group iteration 
  
  one_group_4plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 4 )
  ggplot2::ggsave(one_group_4plots, filename = paste0("one_group_plot",groupings_labs[m], ".png") ,
                 path = "plots/", units = "in", width = 30, height = 6.5, dpi = 300, bg = "white")
  print(paste("Saving plots to disk",groupings_labs[m], format(Sys.time(), "%X") ))
  
  group_plots[[m]]<- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 4 )
  
} 



print(paste("Grouping all plots", format(Sys.time(), "%X") ))

group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )

print(paste("Saving plots to disk", format(Sys.time(), "%X") ))

ggplot2::ggsave(group_plots.png, filename = "group_plots.v15FORCED3.png", path = "plots/", units = "in", width = 30, height = 20, dpi = 300, bg = "white")
end.time <- Sys.time()
print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))


}

## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## 
##  plot only Current and Ref x Suit for presentation
## 3 individual 1 x 2 panels (and a final grouped 3 x 2)

{ begin.time <- Sys.time()
  
  group_plots<- NULL # list, will hold the  3 cowplots (of 4 plots) per migratory iteration, for plotting and saving at the end
  
  three.cat.list<- NULL # for the 3 plots of each iteration
  #groupings_labs
  
  rast.cat.names<- c("Sum Current Suitable Habitat","Sum Refugia Probability","Sum Future Suitable Habitat","Sum Future Suitable Refugia")
  
  turbo_pal <- c(viridis::turbo(n = 1000, direction = -1))
  
  rast.cat.namesShort <-  rast.cat.names[c(1,4)]
  
  for (m in 1:length(all.group.rasters)) {
    
    three.cats <- all.group.rasters[[m]][c(1,4)]
    current<-three.cats[[1]]
    
    max.val <- max(terra::values(current), na.rm = TRUE)
    min.val <- min(terra::values(current), na.rm = TRUE)
    
    print(paste("Plotting", groupings_labs[m]))
    
    for (j in 1:length(three.cats)) {
      
      sum.raster <- three.cats[[j]]# a raster
      
      
      plot.one<-ggplot2::ggplot()+
        ggplot2::geom_sf(data = poly, fill = "grey") +
        ggplot2::geom_sf(data = usa_crop, fill = "white")+
        ggplot2::geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =sum.raster)+
        ggplot2::geom_sf(data = usa_crop, alpha =0)+
        ggplot2::geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                          ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                          expand = FALSE)+
        ggplot2::theme_bw()+
        #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
        ggplot2::scale_fill_gradientn(
          na.value = "transparent",
          colors = c(
            turbo_pal 
          ),
          values = scales::rescale(
            sort(c(range(terra::values(sum.raster)), c(0,  num.spp[m]))),
            to = c(0, 1)
          ),
          oob = scales::squish,
          limits = c(0,  num.spp[m])
        ) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::ggtitle(paste(groupings_labs[m], rast.cat.namesShort[[j]]))
      
      print(paste("Plotting",groupings_labs[m], rast.cat.namesShort[[j]], format(Sys.time(), "%X") ))
      
      three.cat.list[[j]] <- plot.one # lists the four plots per category
      
    }
    print(paste("Grouping three sets plots", format(Sys.time(), "%X") ))
    
    #cowplot object with 4 maps of each migra group iteration 
    
    one_group_4plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 2 )
    #ggplot2::ggsave(one_group_4plots, filename = paste0("one_group_plotSHORTv3",groupings_labs[m], ".png") ,
     #               path = "plots/", units = "in", width = 7.5, height = 3.75, dpi = 300, bg = "white")
    
    #group_plots[[m]]<- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 2 )
    #rm(three.plots)
    
  } 
  
  
  
  #print(paste("Grouping all plots", format(Sys.time(), "%X") ))
  
  #group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )
  
  #print(paste("Saving plots to disk", format(Sys.time(), "%X") ))
  
  
  #ggsave(group_plots.png, filename = "group_plots.v13TRIALS.png", path = "plots/", units = "in", width = 30, height = 20, dpi = 300, bg = "white")
  end.time <- Sys.time()
  print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
  
}

## PAs version 2 (masking all out except whats inside the PAs, keep the scale, and set it along the 2 plots from current)
## sets to plot only Current and Ref x Suit for presentation
## 3 individual 1 x 2 panels 

{ begin.time <- Sys.time()
  ##Masked RASTER with PAs
  maskedPATest <- terra::mask(  (all.group.rasters[[1]])[[4]] , yukon_PAs_crs  )
  #terra::plot(maskedPATest)
  #terra::plot(maskedPATest)
  er <- terra::rast(terra::ext(maskedPATest), resolution=terra::res(maskedPATest), crs = rast.crs)
  terra::values(er) <- 1
  #terra::plot(er)
  xx <- terra::ifel( maskedPATest > 0, NA, er ) # use this to cover the raster
  #terra::plot(xx)
  xxx <- terra::as.polygons(xx) #make a polygon out of the NAing raster for PAs
  #terra::plot(xxx)

  sample_plot_list2<- NULL
  
  
  turbo_pal <- c(viridis::turbo(n = 1000, direction = -1))
  
  rast.cat.namesShort <-  rast.cat.names[c(1,4)]
  
  for (m in 1:length(all.group.rasters)) {
    
    three.cats <- all.group.rasters[[m]][c(1,4)]
    current<-three.cats[[1]]
    
    max.val <- max(terra::values(current), na.rm = TRUE)
    min.val <- min(terra::values(current), na.rm = TRUE)
    
    print(paste("Plotting", groupings_labs[m]))
    three.cat.list<-NULL
    
    for (j in 1:length(three.cats)) {
      
      sum.raster <- three.cats[[j]]# a raster
      
      plot.one<-ggplot2::ggplot()+
        ggplot2::geom_sf(data = poly, fill = "grey") +
        ggplot2::geom_sf(data = usa_crop, fill = "white")+
        ggplot2::geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =sum.raster)+
        ggplot2::geom_sf(data = usa_crop, alpha =0)+
        ggplot2::geom_sf(data = canada_crop, alpha =0)+
        ggplot2::theme_bw()+
        #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
        ggplot2::scale_fill_gradientn(
          na.value = "transparent",
          colors = c(
            turbo_pal 
          ),
          values = scales::rescale(
            sort(c(range(terra::values(sum.raster)), c(0,  num.spp[m]))),
            to = c(0, 1)
          ),
          oob = scales::squish,
          limits = c(0,  num.spp[m])
        ) +
        tidyterra::geom_spatvector(
          data = xxx, fill = "white"#ggplot2::aes(color = "white")
        )+
        ggplot2::geom_sf(data = yukon_PAs_crs, alpha = 0 )+
        ggplot2::theme(
          plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::ggtitle(paste(groupings_labs[m], rast.cat.namesShort[[j]]))+
        ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0) +
        ggplot2::geom_sf(data = usa_crop, alpha = 0)+
        ggplot2::geom_sf(data = canada_crop, alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(xxx)[1], terra::ext(xxx)[2]),
                          ylim = c(terra::ext(xxx)[3], terra::ext(xxx)[4]),
                          expand = FALSE)
      
      print(paste("Plotting",groupings_labs[m], rast.cat.namesShort[[j]], format(Sys.time(), "%X") ))
      
      three.cat.list[[j]] <- plot.one # lists the four plots per category
       
    } 
    
    one.group.PAS <- cowplot::plot_grid(plotlist = three.cat.list, ncol = 2)
    
    ggplot2::ggsave(one.group.PAS, filename = paste0("one.group.PAS_",groupings_labs[m],".png"), path = "plots/", units = "in", width = 15, height = 7.5, dpi = 300, bg = "white")
   
    print(paste("Grouping three sets plots", format(Sys.time(), "%X") ))
    
    sample_plot_list2[[m]]<-cowplot::plot_grid(plotlist = three.cat.list, ncol = 2)
    
  } 
  
  sample_plot_PAs_refxsuit <- cowplot::plot_grid(plotlist = sample_plot_list2, nrow = 3)

  ggplot2::ggsave(sample_plot_PAs_refxsuit, filename = "sample_plot_PAs_refxsuitForcedv4.png", path = "plots/", units = "in", width = 15, height = 20, dpi = 300, bg = "white")
  
  end.time <- Sys.time()
  
  print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
  
}

#get one plot.one to just show alone
#ggplot2::ggsave(plot.one, filename = "one_sample_plot_PAs_refxsuitv3.png", path = "plots/", units = "in", width = 7.5, height = 7.5, dpi = 300, bg = "white")





########## Plot standard deviation (sd) of stacks maps
########## in one 3 x4 panel

{ begin.time <- Sys.time()
  
  rast.cat.names<- c("current.SD","refugia.SD","future.SD","refugia x future.SD")
  group_plots<- NULL # list, will hold the  3 cowplots (of 4 plots) per migratory iteration, for plotting and saving at the end
  
  three.cat.list<- NULL # for the 3 plots of each iteration
  #groupings_labs
  for (m in 1:length(all.groups.sd.rasters)) {
    
    four.sd.cats <- all.groups.sd.rasters[[m]]
    
    print(paste("Plotting SD", groupings_labs[m]))
    
    for (j in 1:length(four.sd.cats)) {
      
      sum.raster <- four.sd.cats[[j]]# a raster
      
      # if (j == 1) {
      
      # truncate values for better viz FOR NOW DECIDED NOT TO DO IT WITH THE STACKS
      #mean.val <- mean(terra::values(rast1), na.rm = TRUE)
      #low.val<- min(terra::values(rast1), na.rm = TRUE)
      #zmin <- max(mean.val, 0.001, na.rm = TRUE)
      #zmin <- max(zmin, 0.01, na.rm = TRUE)
      #zmax <- max(terra::values(rast1), na.rm = TRUE)
      #q99 <- quantile(terra::values(rast1), probs=c(0.999), na.rm=TRUE)
      
      plot.one<-ggplot2::ggplot()+
        ggplot2::geom_sf(data = poly, fill = "grey") +
        ggplot2::geom_sf(data = usa_crop, fill = "white")+
        ggplot2::geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =sum.raster)+
        ggplot2::geom_sf(data = usa_crop, alpha =0)+
        ggplot2::geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                          ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                          expand = FALSE)+
        ggplot2::theme_bw()+
        ggplot2::scale_fill_viridis_c( option = "cividis", # should be low SD is light colour and high is dark
                                       direction = -1, 
                                       na.value="transparent")+ ### DIANA's paper style?
        ggplot2::theme(
          plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
        )+
      ggplot2::ggtitle(paste(groupings_labs[m], rast.cat.names[[j]]))#+
    
      print(paste("Plotting",groupings_labs[m], rast.cat.names[[j]], format(Sys.time(), "%X") ))
      
      
      
      three.cat.list[[j]] <- plot.one # lists the four plots per category
      
    }
    print(paste("Grouping three sets plots", format(Sys.time(), "%X") ))
    
    #cowplot object with 4 maps of each migra group iteration 
    
    
    #one_group_4plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 4 )
    #ggplot2::ggsave(one_group_4plots, filename = paste0("one_group_plot",groupings_labs[m], ".png") ,
     #               path = "plots/", units = "in", width = 30, height = 6.5, dpi = 300, bg = "white")
    
    #print(paste("Saving plots to disk", format(Sys.time(), "%X") ))
    
    group_plots[[m]]<- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 4 )

  } 
  
  
  
  print(paste("Grouping all plots", format(Sys.time(), "%X") ))
  
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )
  
  print(paste("Saving plots to disk", format(Sys.time(), "%X") ))
  
  
  ggplot2::ggsave(group_plots.png, filename = "group_SDs.v1.png", path = "plots/", units = "in", width = 30, height = 20, dpi = 300, bg = "white")
  end.time <- Sys.time()
  print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
  
}



#########################  To share with larger team
#########################  
#########################  
### 5) Plot INDIVIDUAL SPP current distributions, densities capped to maxq 95, or over total birds (proportion to standardize and stack groups)

#files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)

{
  
  Mode <- function(x) {
    x<-x[!is.na(x)]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  begin.time <- Sys.time()
  
  #check color palette to assign highest colour correctly
  #vir_pal <- c(viridis::viridis(n = 1000, direction = -1))
  #find a darker shade of the darkest color in the palette
  #darker<-colorspace::darken("#440154FF", 0.2)
  #darker
  
  startnum<-NULL
  the.birds <- (c(RES,LDM,SDM))
  the.birds <- sort(the.birds) # for 1 ordered full spp plot
  
  NA_rast1 <-terra::crop(NA_rast,bird_rast)
  AKrast1 <-terra::crop(AKrast,bird_rast)
  
  pixel.vals<-NULL
  
  all.plots<-NULL

#loop for to run all groups
for (k in groupings_labs) {
  
  #for one group
  group_plots<- list()
  
  group_spp <- get(k)
  
  #for the all the species in the group 
  for (i in c(group_spp)){
    startnum[[i]]<-i
    sample1 <- readRDS(files.vect[grep(i, files.vect)] )
    rast1<-terra::rast(sample1)
    rm(sample1)
    rast1 <- rast1[[1]]
    terra::crs(rast1) <- rast.crs
    rast1 <-  terra::crop(rast1, NA_rast)
    rast1 <- terra::mask(rast1, NA_rast.crs)#mask using NAs from env variables 
    rast1 <- terra::mask(rast1, AKrast)
    q95 <- quantile(terra::values(rast1), probs=c(0.95), na.rm=TRUE)
    rast1 <-terra::ifel(rast1 >=q95, q95, rast1)
    names(rast1) <- i

    mean.val <- mean(terra::values(rast1), na.rm = TRUE)
    median.val <- median(terra::values(rast1), na.rm = TRUE)
    
    mode.val <- Mode(terra::values(rast1))
    
    max.val<- max(terra::values(rast1), na.rm = TRUE)
    zmin <- max(mean.val, 0.001, na.rm = TRUE)
    
    if (zmin > mean.val) { # we use this specially for WIWR that had really low densities, even lower than 0.001 and if brakes the scale
      zmin <- min(mean.val, 0.0001, na.rm = TRUE) # especially for WIWR, will assign a lower threshold 
      if (zmin>mean.val) { # if even 0.0001 is higher than the mean value
        print("Warning: Densities are too low") #this warning would only work with other birds, but not on ours
      }
    }else{zmin <- min(zmin, 0.01, na.rm = TRUE)
}
  
    lowest <- min(terra::values(rast1), na.rm = TRUE)

    q99 <- quantile(terra::values(rast1), probs=c(0.999), na.rm=TRUE) #after capping
    q90 <- quantile(terra::values(rast1), probs=c(0.9), na.rm=TRUE) #after capping
    
    if (zmin<lowest) {
      zmin <-lowest
    } #when the low threshold assigned is lower than lowest value of the raster, it assigns just the lowest value
    
    pixel.vals <- rbind( pixel.vals, c(k, i, mean.val, median.val, lowest, max.val, zmin, q90, q99))
    
   
  }
  
  print(paste("Grouping plots", k,"in 1 frame", format(Sys.time(), "%X") ))
  
  one.group <-cowplot::plot_grid(plotlist = group_plots, ncol=4 )
  
  print(paste("Saving", k, "plots to disk", format(Sys.time(), "%X") ))
  
  ggplot2::ggsave(one.group, filename = (paste0(k,"_currDensv6.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(group_spp)/4), limitsize = FALSE, dpi = 300, bg = "white")
  #no scaling plot excluded just used the raw values

 
  }
all.bird.plots <-cowplot::plot_grid(plotlist = all.plots, ncol=4 )

ggplot2::ggsave(all.bird.plots, filename = (paste0("all_currDensv6.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(the.birds)/4), limitsize = FALSE, dpi = 300, bg = "white")

  end.time <- Sys.time()
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
}


pixel.vals.df<- as.data.frame(pixel.vals)
names(pixel.vals.df) <- c("mig", "spp", "mean.val","median", "lowest", "max.val", "zmin","q90", "q99")

pixel.vals.df<-pixel.vals.df|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), as.numeric)|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), round, 7)

write.csv(pixel.vals.df, "data/pixel.vals.df.csv")



############################################################## NOT NEEDED, need to revise if so
############################################################## especially change the Current dis stack plotting
##############################################################
### 4) PLOT all species individually within a category in independent sets per group
### for all birds in a category #####
### no calculations
##############################################################

#Curent loop runs over all predefined groups
{begin.time <- Sys.time()

#files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)

all.3.sets<-list()

#loop for to run all groups
for (k in groupings_labs) {
  print(paste("Starting", k,"at", begin.time))
  
  group_plots<- list()### list of plots all spp 4 plots, for each category (to be saved with ggsave)
  
  group_spp <- get(k)
  
  #compile each set of graphs: ref, suit, ref x suit for all spp within a group (either LDM, SDM, or RES)
  curr.mean.list <- list() # resets for each grouping run - k iteration in the loop
  ref.mean.list <- list()
  suit.mean.list <- list()
  refxsuit.mean.list <- list()
  
  
  all.spp.plot.names <-  list()
  
  #ref.ras1<-NULL
  #suit.ras1<-NULL
  #refxsuit.ras1<-NULL
  
  for (i in c(group_spp)) {
    
    #Load present RDS distribution files and make rasters 
    print(paste("Working", k, "current dist", i,"at", format(Sys.time(), "%X") ))
    
    sample1 <- readRDS(files.vect[grep(i, files.vect)] )
    ###################
    
    Max95<-quantile(sample1$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
    #q99 <- quantile(terra::values(curr.sum), probs=c(0.999), na.rm=TRUE)
    
    #suitscale<-suit/Max95  # scaled against present 95% max 
    #values(suitscale)[values(suitscale) > 1] = 1  # assign areas that get more suitable (>1) to 1
    ###################
    ###################
    rast1 <- sample1 |>
      dplyr::mutate(
        std.mean = Mean/Max95
      )
    
    rast1<-terra::rast(rast1, crs =  rast.crs)  
    
    rast1 <- rast1[[4]] # dplyr::select only the Mean values layer, not the standardized/scaled anymore [[4]]
    
    rast1 <- terra::ifel(rast1 >=1, 1, rast1 ) # cap to a max of 1
    
    rast1 <-  terra::crop(rast1, NA_rast) # crop to the extent of the mask, only for current dists, because they are larger
    rast1 <- terra::mask(rast1, NA_rast.crs) #mask using NAs from env variables 
    rast1 <- terra::mask(rast1, AKrast) # mask for alaska
    
    names(rast1) <- i
    
    print(paste("Working", k, "refugia", i,"at", format(Sys.time(), "%X") ))
    
    #rasters are sometimes tif or tiff, so the grep() solves it
    ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
    names(ref.ras1)<-terra::varnames(ref.ras1)
    ref.ras1 <- terra::mask(ref.ras1, NA_rast)#mask using NAs from env variables 
    ref.ras1 <- terra::mask(ref.ras1, AKrast)
    
    
    print(paste("Working", k, "future dist", i,"at", format(Sys.time(), "%X") ))
    
    suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
    names(suit.ras1) <- terra::varnames(suit.ras1)
    suit.ras1 <- terra::mask(suit.ras1, NA_rast)#mask using NAs from env variables 
    suit.ras1 <- terra::mask(suit.ras1, AKrast)
    
    
    print(paste("Working", k,"ref x future", i,"at", format(Sys.time(), "%X") ))
    
    refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
    names(refxsuit.ras1) <- terra::varnames(refxsuit.ras1)
    refxsuit.ras1 <- terra::mask(refxsuit.ras1, NA_rast)#mask using NAs from env variables 
    refxsuit.ras1 <- terra::mask(refxsuit.ras1, AKrast)
    
    
    curr.mean.list[[i]] <- rast1
    ref.mean.list[[i]] <- ref.ras1
    suit.mean.list[[i]] <- suit.ras1
    refxsuit.mean.list[[i]] <- refxsuit.ras1
    
    #all.spp.plot.names[[i]] <- c(paste0(i,".refugia"),paste0(i,".suitability"),paste0(i,".refugia x suit"))
    
  }
  
  for (j in 1:length(names(ref.mean.list))) {
    
    #call each species plot from the  category lists
    current <- curr.mean.list[[j]]
    refugia <- ref.mean.list[[j]] 
    suitability <- suit.mean.list[[j]] 
    refxsuit <- refxsuit.mean.list[[j]] 
    
    setplots<-c("current", "refugia", "suitability", "refxsuit")
    
    one.spp.plot.cat.list<- list() # this will have the 4 plots for each species: a, b, c, d
    for (t in setplots) { # do it for each element of the 4 lists (each species) 
      
      rast1<-get(t)
      print(paste("Plotting", k, t, i,"at", format(Sys.time(), "%X") ))
      
      plot.one <- ggplot2::ggplot()+
        ggplot2::geom_sf(data = poly, fill = "grey") +
        ggplot2::geom_sf(data = usa_crop, fill = "white")+
        ggplot2::geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =rast1)+
        ggplot2::geom_sf(data = usa_crop, alpha =0)+
        ggplot2::geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(rast1)[1], terra::ext(rast1)[2]),
                          ylim = c(terra::ext(rast1)[3], terra::ext(rast1)[4]),
                          expand = FALSE)+
        ggplot2::theme_bw()+
        ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="transparent")+ ### DIANA's paper style?
        ggplot2::theme(
          plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::ggtitle(names(rast1))
      print(paste(j,"out of", length(names(ref.mean.list)),"Plotting", k, "species:",names(rast1)))
      one.spp.plot.cat.list[[t]]<- plot.one
      
    }
    
    three.plots <- cowplot::plot_grid(plotlist = one.spp.plot.cat.list, nrow = 1, ncol = 4 )
    #ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
    
    group_plots[[j]]<- three.plots # one spp plots go into the list per migra group
  } 
  
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = length(names(ref.mean.list)), ncol = 1 ) # rows set to the whole number of plots for now, may want to figure out how to save in chunks once this works
  #maybe cow plot by chunks? 
  
  #graph.rows <-  ceiling(length(list.bird.plots)/3) 
  
  ggplot2::ggsave(group_plots.png, filename = paste0(k,"_spp_plotsv3.png"), path = "plots/", units = "in", width = 16, height=  3*length(names(ref.mean.list)), limitsize = FALSE, dpi = 300, bg = "white")
  
  all.3.sets [[k]] <- group_plots.png
  
}

end.time <- Sys.time()

print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
}




