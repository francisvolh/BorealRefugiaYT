#################################################################################### 
################### Produce group classification to sum and plot ################### 

#preliminary objects (spp vectors, shapes, rasters, and limits)
system.time({
  ####### 1) prep files and folders
  
#load classification file and make make vectors with codes
#class_spp <- read.csv("data/SpeciesStatus.csv")

#unique(class_spp$Migration1)

  #### 
  #### CHSP, LISP, OCWA, TRES as short dist
  #### now excluded wiwr (as of May 25th 2024)
  
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

groupings_labs <- c("RES","SDM", "LDM"#,"NOM"#,"N_assig"
                    )
groupings <- c(RES,SDM,LDM#,NOM#,N_assig
               )

max.len <- max(length(LDM),length(SDM),length(RES))
SDM.spp = c(SDM, rep(NA, max.len - length(SDM)))
LDM.spp = c(LDM, rep(NA, max.len - length(LDM)))
RES.spp = c(RES, rep(NA, max.len - length(RES)))
cat.spp.df<-data.frame(SDM.spp, LDM.spp, RES.spp)
#write.csv(cat.spp.df, "data/cat.spp.df.csv")
num.spp <- c(length(RES), length(SDM), length(LDM))

#load maps

#previously extracted Ecoregions level 2 from CEC ecoregions
BCR4.1_USACAN<-sf::st_read("data/BCR4/BCR4.1_USACAN.shp")
BCR4.0_USACAN  <- sf::st_read("data/BCR4/BCR4.0_USACAN.shp")

canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_1.shp")
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")

#set up foder routes
####   "data/YT Boreal Refugia Drive/Jan2024_Rasters_1991_Normal_Refugia and Habitat Suitability/1991_Refugia_Suitabiltiy_Rasters_MEAN/ALFL_Refugia_RCPmean.tif"
files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)

#set refugia raster results directory
#the file list
files.to.read<-list.files("data/YT Boreal Refugia Drive/bootstrapped rasters/BootstrapRasters", full.names = TRUE)

#for increasing decreasing population -- WHEN NEEDED, not yet
#files.future <- list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds", full.names = TRUE )



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
#"data/YT Boreal Refugia Drive/bootstrapped rasters/BootstrapRasters"
bird_rast <- terra::rast("data/YT Boreal Refugia Drive/bootstrapped rasters/BootstrapRasters/ALFL_FutureRef.tif")
terra::crs(bird_rast)<-rast.crs
NA_rast <-terra::crop(NA_rast,bird_rast)


NA_rast.crs <- NA_rast
terra::crs(NA_rast.crs) <- rast.crs
rm(NAKey) 
rm("Norm1991")


#usa_can_crop <- sf::st_union(canada, USA) # dont do this, keep each country separate

usa_crop <- sf::st_transform(USA, rast.crs )
rm(USA) ### improve speed of this section by saving and just loading Alaska

usa_crop <- sf::st_crop(usa_crop, c(xmin=as.numeric(terra::ext(bird_rast)[1]), 
                                        xmax=as.numeric(terra::ext(bird_rast)[2]), 
                                        ymin=as.numeric(terra::ext(bird_rast)[3]), 
                                        ymax=as.numeric(terra::ext(bird_rast)[4])
))

canada_crop <- sf::st_transform(canada, rast.crs )
rm(canada) ### improve speed of this section by saving and just loading Provinces needed
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


#protected areas
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
rm(crs_CPCAD,CPCAD )
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
})

################################################################################################
################################################################################################
### 2) PRODUCE STACKS FOR CURRENT SUITABILITY, REFUGIA, FUTURE SUITABILITY, and SUITABLE REFUGIA rasters
### including calculation of high value areas surface coverage of ecoregions and  PAs
### for GROUPING categories 
#########################################################################
groupings_labs <- c("RES","SDM", "LDM") #be sure you have this labels for migratory stacks
#need to re run after running plots!!!!! or switching from pop based 
num.spp <- c(length(RES), length(SDM), length(LDM))


# RUN CHUNK tO CHANGE TO POP BASED STACKS
{ 
  decreasers <- c("ATTW","GRAJ","BOCH","ATSP","WCSP","SAVS","MOBL","FOSP","OCWA","BLPW",
                  "WIWA","GCTH")
  increasers <- c("RBNU","CORA","HAWO","BCCH","LISP","NOFL",
                  "VATH","GCKI","YBSA",
                  "CHSP","COYE","WEWP","YRWA","SWTH","NOWA","YEWA","LEFL","ALFL","HAFL",
                  "WETA" ,"TEWA" ,"WAVI")
  no_change <- setdiff(groupings,decreasers)
  no_change <- setdiff(no_change,increasers)
  
 
  
  num.spp <- c(length(decreasers), length(increasers), length(no_change))
  groupings_labs <- c("decreasers","increasers", "no_change")
  
}

#!!!!!!!!!!!!!!!!!!!!!!!
#remember!!!!!!!!!!!!!!! to re run num.spp and grouping_labs if want to run the respective stacks: migra after pop based, normally!
# !!!!!!!!!!!!! choose the right labels and stacks type above

#stacks run for migratory groups!!
{ 
  all.group.rasters<-NULL # list, will hold 3 lists, one per migra group (three cats), with 4 stacked raster each
  all.groups.sd.rasters<-NULL
  
  stacks.mean.vals <- NULL
  high.pa.vals <- NULL
  startnum<-NULL
  categoryCol <- NULL
  
  
  #loop for to run all groups
  begin.time <- Sys.time()
  
  #iterate per migratory group
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
    
    #loop to load each species into a list to be stacked within each iteration per group
    for (i in c(group_spp)) { 
      
      startnum[[i]]<-i
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "current dist", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      
      
      rast1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","PresentSuit.tif"), files.to.read)])
      terra::crs(rast1) <- rast.crs # assign crs to avoid warning
      curr.mean.list[[i]] <- rast1
      rm(rast1)
      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "refugia", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      
      ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","FutureRef.tif"), files.to.read)])
      terra::crs(ref.ras1) <- rast.crs # assign crs to avoid warning
      ref.mean.list[[i]] <- ref.ras1
      rm(ref.ras1)
      
      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "future dist", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"_","FutureSuit.tif"), files.to.read)])
      terra::crs(suit.ras1) <- rast.crs # assign CRS to avoid warning
      suit.mean.list[[i]] <- suit.ras1
      rm(suit.ras1)
      
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) ,"ref x future", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"_","FutureSuit_by_Ref.tif"), files.to.read)])
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
    #current
    curr.sum <- terra::app(terra::rast(curr.mean.list), "sum")#/length(ref.mean.list)
    
    terra::crs(curr.sum) <- rast.crs
    
    curr.sum.y <-curr.sum #lazy solution to not change all the names below
    
    curr.sd <- terra::app(terra::rast(curr.mean.list), "sd")#/length(ref.mean.list)
    
    print(paste("Working stacked",k, "refugia",format(Sys.time(), "%X") ))
    #refugia
    ref.sum <- terra::app(terra::rast(ref.mean.list), "sum")#/length(ref.mean.list)
    terra::crs(ref.sum) <- rast.crs
    ref.sum.y <- ref.sum
    ref.sd <- terra::app(terra::rast(ref.mean.list), "sd")#/length(ref.mean.list)
    
    print(paste("Working stacked",k, "suitability",format(Sys.time(), "%X") ))
    #suitability
    suit.sum <- terra::app(terra::rast(suit.mean.list), "sum")#/length(suit.mean.list)
    terra::crs(suit.sum) <- rast.crs
    suit.sum.y <-suit.sum
    suit.sd <- terra::app(terra::rast(suit.mean.list), "sd")#/length(ref.mean.list)
    
    
    print(paste("Working stacked",k, "ref x suitability",format(Sys.time(), "%X") ))
    #ref x suit
    refxsuit.sum <- terra::app(terra::rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)
    terra::crs(refxsuit.sum) <- rast.crs
    refxsuit.sum.y <-refxsuit.sum
    refxsuit.sd <- terra::app(terra::rast(refxsuit.mean.list), "sd")#/length(ref.mean.list)
    
    #make sets of rasters of only the 4 categories (curr, ref, suit, ref x suit) per species grouping (res, ldm, sdm)
    three.cats  <- list(curr.sum, ref.sum, suit.sum, refxsuit.sum) # list of sacked rasters for each group iteration TO PLOT
    
    four.sds.cats  <- list(curr.sd, ref.sd, suit.sd, refxsuit.sd)
    
    #need the names for ggplots, may ditch later for pub
    
    category <- c(paste0(k,".current.sum"), paste0(k,".refugia.sum"),paste0(k,".future.sum"),paste0(k,".refugia x future.sum")) # list of names
    
    categoryCol <- cbind(categoryCol, category)    
    
    
    
    all.group.rasters[[k]]<- three.cats # list of 4 groups raster, with all 3 migra groups
    
    all.groups.sd.rasters[[k]] <- four.sds.cats
    
    
    # make a cbind ones I get all the protected areas to USE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #high.pa.vals[[k]] <- list(area.cropped_rast_ytSDM, area.cropped_rast_ytREF, area.cropped_rast_ytSUIT, area.cropped_rast_ytRxS)
    
    end.time <- Sys.time()
    
    
    
    
  }# end of group loop
  
  print(paste("total duration of raster stacking", round(difftime(end.time, begin.time, units = "mins"),2), "mins"))
  
  qs <-  c(0.25, 0.50, 0.75)
  
  quantiles_store <- NULL
  
  for (q in qs) { 
    
    group_area_values<- NULL # dataframe, will hold the q75 surface area values for each migratory group, per raster stack 
    pa_group_area_values <- NULL # df will hold PA values the q75 surface area values for each migra group per raster stack
    
    quant<- q
    group_area_values<-NULL 
    
    for (n in 1:length(all.group.rasters)) {
      four_area_values<-NULL
      PA_four_area_values<-NULL
      quants_group <-NULL
      for (m in 1:length(all.group.rasters[[n]])) {
        
        # quantile of one raster (kept it named as q75 but it will change)
        if (m==1) {
          q75 <- quantile(terra::values(all.group.rasters[[n]][[m]]), probs=c(quant), na.rm=TRUE)
          quants_group<-c(groupings_labs[n], quant, as.numeric(q75) )
        }
        
       
        
        sum.test <- terra::ifel(all.group.rasters[[n]][[m]]>=q75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
        area <-sum(terra::values(sum.test), na.rm = TRUE)
        
        cropped_rast_yt <- terra::mask(all.group.rasters[[n]][[m]], yukon_PAs_crs)
        
        area.cropped <- terra::ifel(cropped_rast_yt>=q75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
        area.cropped <-sum(terra::values(area.cropped), na.rm = TRUE)
        
        four_area_values <- cbind( four_area_values, area)
        
        PA_four_area_values <- cbind( PA_four_area_values, area.cropped)
        
      }
      
      four_area_values<- cbind(four_area_values, names(all.group.rasters[n]))
      
      group_area_values <- rbind(group_area_values, four_area_values)
      
      
      PA_four_area_values<- cbind(PA_four_area_values, names(all.group.rasters[n]))
      
      pa_group_area_values<- rbind(pa_group_area_values, PA_four_area_values)
      
      quantiles_store <- rbind( quantiles_store, quants_group)
    }
    
    
    quants.df<- as.data.frame(quantiles_store)
    
    #write.csv(quants.df, "data/quantile.values_POP.csv", row.names = FALSE)
    
    df <- as.data.frame(group_area_values)
    
    #produce table with area coverage of good refugia
    #df$Category <- groupings_labs
    names(df) <- c( "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit", "Category")
    
    high.areas<-df[ , c("Category", "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit")]
    
    
    # re calculating BCR are based on the actual modelled area within the BCR (excluded the NAd pixels to avoid over estimation of the %)
    #cropped_rast_bcr <- terra::mask(bird_rast, bcr)# grab one raster, to get the modeled area using each pixel
    
    #plot(cropped_rast_bcr)
    
    for_calc_bcr <- bird_rast
    for_calc_bcr <-  terra::crop(for_calc_bcr, NA_rast.crs) #crop to final area
    for_calc_bcr <- terra::mask(for_calc_bcr, NA_rast.crs)#mask using NAs from env variables 
    for_calc_bcr <- terra::mask(for_calc_bcr, AKrast)
    for_calc_bcr <- terra::ifel(for_calc_bcr >0, 1, NA)
    #plot(for_calc_bcr)
    area.bcr_for_calc<-sum(terra::values(for_calc_bcr), na.rm = TRUE)
    
    high.areas$Pres.Percent <- as.numeric(high.areas$Present)/ as.numeric(area.bcr_for_calc)*100
    high.areas$Ref.Percent <- as.numeric(high.areas$Refugia)/ as.numeric(area.bcr_for_calc)*100
    high.areas$Fut.Suitable.Perc <- as.numeric(high.areas$Fut.Suitable)/ as.numeric(area.bcr_for_calc)*100
    high.areas$RefxFut.Perc <- as.numeric(high.areas$RefxFut.Suit)/ as.numeric(area.bcr_for_calc)*100
    high.areas
    
    #switch save name for MIGRA and POP based runs
  # write.csv(high.areas, paste0("data/POPhigh.areasv2q", quant*100,".csv") )
    
    
    
    ###
    ### FIX the PA section making a rbind
    ###
    cropped_rast_yt <- terra::mask(bird_rast, yukon_PAs_crs)
    for_calc <- cropped_rast_yt # grab the last raster produced of a stack for only PA masks
    
    for_calc <- terra::ifel(for_calc >0, 1, NA)
    #plot(for_calc)
    area.for_calc<-sum(terra::values(for_calc), na.rm = TRUE) # exclude the NAs for models and Alaska
    
    df2<-as.data.frame(pa_group_area_values)
    df2$Category <- groupings_labs
    
    names(df2) <- c( "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit", "Category")
    
    high.pa.area <- df2[, c( "Category", "Present", "Refugia", "Fut.Suitable", "RefxFut.Suit")]
    
    high.pa.area$Pres.Percent <- as.numeric(high.pa.area$Present)/ as.numeric(area.for_calc)*100
    #high.pa.area$Ref.Percent <- high.pa.area$Refugia/ as.numeric(area.for_calc)*100
    #high.pa.area$Fut.Suitable.Perc <- high.pa.area$Fut.Suitable/ as.numeric(area.for_calc)*100
    high.pa.area$RefxFut.Perc <- as.numeric(high.pa.area$RefxFut.Suit)/ as.numeric(area.for_calc)*100
    
    
    
    high.pa.area$Pres.Percent <- as.numeric(high.pa.area$Present)/ as.numeric(area.for_calc)*100
    high.pa.area$Pres.Percent_ECO <- as.numeric(high.pa.area$Present)/ as.numeric(area.bcr_for_calc)*100
    ##high.pa.area$Pres.Dif_Percent_PAECO <- (high.pa.area$Present-high.areas$Present)/ high.areas$Present*100
    
    #high.pa.area$Ref.Percent <- high.pa.area$Refugia/ as.numeric(area.for_calc)*100
    #high.pa.area$Fut.Suitable.Perc <- high.pa.area$Fut.Suitable/ as.numeric(area.for_calc)*100
    
    high.pa.area$RefxFut.Perc <- as.numeric(high.pa.area$RefxFut.Suit)/ as.numeric(area.for_calc)*100
    high.pa.area$RefxFut.Perc_ECO <- as.numeric(high.pa.area$RefxFut.Suit)/ as.numeric(area.bcr_for_calc)*100
    
    ##high.pa.area$RefxFut.Suit.Dif_Percent_PAECO <- (high.pa.area$RefxFut.Suit-high.areas$RefxFut.Suit)/ high.areas$RefxFut.Suit*100
    
    
    
    print(high.areas)
    print(high.pa.area)
    
    #switch save name for MIGRA and POP based runs
  # write.csv(high.pa.area, paste0("data/POPhigh.pa.areav2q",quant*100,".csv"))
    
  }
  end.time <- Sys.time()
  print(paste("total duration of raster stacking", round(difftime(end.time, begin.time, units = "mins"),2), "mins"))
  
}



###########################################################################
###########################################################################
############### PLOTS #####################################################

# Make the plots for curr, ref, NOT suit, and ref x suit, per migra group 
# all in one 3 x 3 panel

#prep hillshade if used
{
  topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"

#load elevation raster
elevation <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))

elevation <-  terra::crop(elevation, terra::project(bird_rast, "EPSG:4326" ))

elevation <-  terra::project(elevation, bird_rast )

mdt <-  terra::crop(elevation, bird_rast) 


sl <- terra::terrain(mdt, "slope", unit = "radians")
#terra::plot(sl)
asp <- terra::terrain(mdt, "aspect", unit = "radians")
#terra::plot(asp)


h<-terra::shade(sl, asp, angle = c(45, 45, 45, 80), direction = c(225, 270, 315, 135))


hill_single <- Reduce(mean, h)



hill_single<- terra::mask(hill_single, bcr) ###masking hillshade to keep the geom of maps white around


hilldf_single <- as.data.frame(hill_single, xy = TRUE)
names(hilldf_single)<-c("x", "y", "hillshade")
}


### RUN LABELS FOR PLOTS!!!!!!!!!!!!!!!!!!!


#re run labels for plotting Migratory status based stacks
groupings_labs <- c("Residents","Short-distance", "Long-distance")# for plotting only

#choose this if running the pop bases categories
groupings_labs <- c("Decreasers","Increasers", "No change")


              
#LOOP for plots of stacks 
{ begin.time <- Sys.time()
  
  rast.cat.names<- c("Current Suitable Habitat","Refugia Probability","Future Suitable Habitat"," Future Suitable Refugia")
  group_plots<- NULL # list, will hold the  3 cowplots (of 4 plots) per migratory iteration, for plotting and saving at the end
  
  three.cat.list<- NULL # for the 3 plots of each iteration
  #groupings_labs
  
  turbo_pal <- c(viridis::turbo(n = 1000, direction = -1))
  
  rast.cat.namesShort <-  rast.cat.names[c(1,2,4)] #### this subsets to only Curr, Ref, and Refxsuit
  
  for (m in 1:length(all.group.rasters)) {
    
    three.cats <- all.group.rasters[[m]][c(1,2,4)] # this subsets to only Curr, Ref, and Refxsuit
    #three.cats <- all.group.rasters[[m]] # full set if 3x4 is wanted
    
    print(paste("Plotting", groupings_labs[m]))
    
    current<-three.cats[[1]]
    
    max.val <- max(terra::values(current), na.rm = TRUE)
    min.val <- min(terra::values(current), na.rm = TRUE)
    
    if (m == 1) { #to include title only on first row
      for (j in 1:length(three.cats)) {
        sum.raster <- three.cats[[j]]# a raster
        
        plot.one <- ggplot2::ggplot()+
          ggplot2::geom_sf(data = poly, fill = "grey") +
          ggplot2::geom_sf(data = usa_crop, fill = "white")+
          ggplot2::geom_sf(data = canada_crop, fill = "white")+
          ggplot2::geom_tile(data = hilldf_single,
                             ggplot2::aes(x, y, fill = hillshade),
                             show.legend = FALSE) +
          ggplot2::scale_fill_distiller(palette = "Greys") +
          ggplot2::xlab(NULL)+
          ggnewscale::new_scale_fill()+
          tidyterra::geom_spatraster(data =sum.raster, alpha =0.7)+
          ggplot2::geom_sf(data = usa_crop, alpha =0)+
          ggplot2::geom_sf(data = canada_crop, alpha =0)+       
          ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
          ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
          ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                            ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                            expand = FALSE)+
          ggplot2::theme_bw()+
          #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
          ggplot2::scale_fill_gradientn(name = "score",
            labels = scales::label_number(accuracy = 1),
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
          ggplot2:: ylab(groupings_labs[m])+
          ggplot2::theme(
            #axis.title = ggplot2::element_blank(),
            text= ggplot2::element_text(size=20),
            axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
            #legend.position = "none",
            plot.margin = ggplot2::margin(0,0,0,0, "cm"),
            plot.title =  ggplot2::element_text(hjust = 0.5, face="bold"),
            axis.title.y = ggplot2::element_text(face="bold")
          )+
          ggplot2::ggtitle(paste(#groupings_labs[m], 
            rast.cat.names[[j]]))
        
        three.cat.list[[j]] <- plot.one # lists the four plots per category
        
        
      }
      
      ######      group_plots[[m]]<- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, 
      #######                                      ncol = length(three.cat.list), rel_widths = c(1.365,1.356,1.48)#, rel_heights = c(1.355,1.355,1.456)
      #)
      
      group_plots[[m]] <- three.cat.list |> 
        # Remove the y axis title and the plot title except for the first plot
        purrr::imap(\(x, y) if (!y == 1) x + ggplot2::labs(y = NULL) else x) |> 
        patchwork::wrap_plots(guides = "collect")
      
    }
    else{ #for all the other row, no title
      for (j in 1:length(three.cats)) {
        sum.raster <- three.cats[[j]]# a raster
        
        plot.one <-  ggplot2::ggplot()+
          ggplot2::geom_sf(data = poly, fill = "grey") +
          ggplot2::geom_sf(data = usa_crop, fill = "white")+
          ggplot2::geom_sf(data = canada_crop, fill = "white")+
          ggplot2::geom_tile(data = hilldf_single,
                             ggplot2::aes(x, y, fill = hillshade),
                             show.legend = FALSE) +
          ggplot2::scale_fill_distiller(palette = "Greys") +
          ggplot2::xlab(NULL)+
          ggnewscale::new_scale_fill()+
          tidyterra::geom_spatraster(data =sum.raster, alpha =0.7)+
          ggplot2::geom_sf(data = usa_crop, alpha =0)+
          ggplot2::geom_sf(data = canada_crop, alpha =0)+       
          ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
          ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
          ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                            ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                            expand = FALSE)+
          ggplot2::theme_bw()+
          #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
          ggplot2::scale_fill_gradientn(name = "score",
            labels = scales::label_number(accuracy = 1),
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
          ggplot2:: ylab(groupings_labs[m])+
          ggplot2::theme(
            axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
            #axis.title = ggplot2::element_blank(),
            #legend.position = "none",
            plot.margin = ggplot2::margin(0,0,0,0, "cm"),
            text= ggplot2::element_text(size=20),
            plot.title =  ggplot2::element_text(hjust = 0.5),
            axis.title.y = ggplot2::element_text(face="bold")
          )+
          ggplot2::ggtitle(paste(groupings_labs[m], rast.cat.names[[j]]))
        
        
        three.cat.list[[j]] <- plot.one # lists the four plots per category
      }
      ######group_plots[[m]]<- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, 
      #                                    ncol = length(three.cat.list), rel_widths = c(1.05,1,1.139)#, rel_heights = c(1,1,1) 
      #)
      
      group_plots[[m]] <- three.cat.list |> 
        purrr::imap(\(x, y) if (!y == 1) x + ggplot2::labs(y = NULL) else x) |> 
        patchwork::wrap_plots(guides = "collect") &
        # Get rif of the titles
        ggplot2::labs(title = NULL)
      
    }
    
    print(paste("Plotting",groupings_labs[m], rast.cat.names[[j]], format(Sys.time(), "%X") ))
    
    
    print(paste("Grouping three sets plots",groupings_labs[m], format(Sys.time(), "%X") ))
    
    #cowplot object with 4 maps of each migra group iteration 
    
    # if want to save 1 row (one migra group) as an independent png
    #one_group_4plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = length(three.cat.list) )
    #ggplot2::ggsave(one_group_4plots, filename = paste0("one_group_plotv19",groupings_labs[m], ".png") ,
    #              path = "plots/", units = "in", width = 30, height = 6.5, dpi = 300, bg = "white")
    
  }
  
  
  
  print(paste("Saving plots to disk",groupings_labs[m], format(Sys.time(), "%X") ))
  
  
  
  print(paste("Grouping all plots", format(Sys.time(), "%X") ))
  
  #group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 #, rel_heights = c(1, 1, 1)
  #)
  
  group_plots.png <- group_plots[[1]]/group_plots[[2]]/group_plots[[3]]
  
  print(paste("Saving plots to disk", format(Sys.time(), "%X") ))
  
#ggplot2::ggsave(group_plots.png, filename = "group_plots.v25POP.png", path = "E:/BorealRefugiaYT/plots" ,units = "in", width = 19, height = 21, dpi = 300, bg = "white")
  end.time <- Sys.time()
  print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
  
}

#v25 has hillshade and good sizing
## select the right saving name for the plot MIGRA or POP based


#v20FORCED3 has alpha for raster of 0.85
#v21FORCED3 has alpha for raster of 0.7


###########################################################
###########################################################
# Maps of high value areas and quantiles overlapping and PAs


# Version 1: Only 3 migra groups

# Maps of 25% or 50% q current suit and and future suit refugia over PAs 
#shaded PAs
#customized colours after reclassifying to Current only, Future only, and overlap

{ begin.time <- Sys.time()

quant<-0.75 ## switch quantile and name for saving!!!!!

groupings_labs <- c("Residents","Short-distance", "Long-distance")# for plotting only

three.cat.list<-NULL
for (m in 1:length(all.group.rasters)) { # for each migratory group
  
  three.cats <- all.group.rasters[[m]][c(1,4)]
  
  current<-three.cats[[1]]
  q75 <- quantile(terra::values(current), probs=c(quant), na.rm=TRUE)
  
  print(paste("Plotting", groupings_labs[m]))
  
  # extract and convert high qual areas for Current into 1 category (value 1)
  new.current <- terra::ifel(current >= q75, 1, 0)
  #terra::plot(new.current)
  # extract and convert high qual areas for Futur refugia into 1 category (value 1)
  new.futRefugia <- terra::ifel(three.cats[[2]] > q75, 2, 0)
  #terra::plot(new.futRefugia)
  
  sumrast <-new.current+new.futRefugia
  
  sumrast <- terra::ifel(sumrast > 0, sumrast, NaN)
  
  unique(terra::values(sumrast))
  #terra::plot(sumrast)
  #test <-terra::as.polygons(sumrast)
  
  #terra::plot(test)
  
  m1<-
    as.matrix(
      data.frame(
        x = c(#min(terra::values(raster_test), na.rm = TRUE)
          0,1,2),
        y = c(1,2,3
              #max(terra::values(raster_test), na.rm = TRUE)
        ),
        z = c(1,2,3) # 1 is present, 2 is future, 3 is overlap
      )  
    )
  
  m1
  
  rr1 <- terra::classify(sumrast, m1, others=NA) 
 
terra::writeRaster(rr1, paste0("data/",groupings_labs[m],".tiff"),  overwrite=TRUE )

  plot.one<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =terra::as.factor(rr1)#, alpha = 0.75
    )+
    #attempt other colours of viridis
    #ggplot2::scale_fill_viridis_c( option = "turbo",  na.value="transparent")+ ### DIANA's paper style?
    
    #ggplot2::scale_fill_manual( 
    #na.value="transparent",
    # values = c("#D55E00","#F0E442", "#0072B2"))+ 
    ggplot2::scale_fill_manual(na.translate = FALSE,
                                na.value="transparent", 
                               values = c("#D55E00","#0072B2","#F0E442"),
                               labels = c("Loss", "Gain", "Retained")
    )+
    #ggnewscale::new_scale_fill()+
    #tidyterra::geom_spatraster(data =new.futRefugia, alpha = 0.25)+
    
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::theme_bw()+
    #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    
    ggplot2::theme(
      legend.position = "bottom",
      axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
      #legend.position = "none",
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm"),
      plot.title =  ggplot2::element_text(hjust = 0.5, face="bold")
    )+
    ggplot2::ggtitle(paste(groupings_labs[m]))+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0) +
    ggplot2::geom_sf(data = usa_crop, alpha = 0)+
    ggplot2::geom_sf(data = canada_crop, alpha = 0)+
    ggplot2::geom_sf(data = yukon_PAs_crs, fill = "#073b4c", color = NA, alpha = 0.35 )+
    ggplot2::coord_sf(xlim=c(terra::ext(new.current)[1], terra::ext(new.current)[2]),
                      ylim = c(terra::ext(new.current)[3], terra::ext(new.current)[4]),
                      expand = FALSE)+
    ggplot2::labs(fill="")
  
  three.cat.list[[m]] <- plot.one # lists the four plots per category
  
} 
#one.group.PAS <- cowplot::plot_grid(plotlist = three.cat.list, ncol = 3, nrow = 1)

one.group.PAS<-three.cat.list |> 
  # Remove the y axis title and the plot title except for the first plot
  purrr::imap(\(x, y) if (!y == 1) x + ggplot2::labs(y = NULL) else x) |> 
  patchwork::wrap_plots(guides = "collect")  & ggplot2::theme(text= ggplot2::element_text(size=20), legend.position = 'bottom')

#ggplot2::ggsave(one.group.PAS, filename = paste0("one.group.PAS_25quantCurFutv8.png"), 
 #               path = "plots", units = "in", width = 15, height = 7.5, dpi = 300, bg = "white")

print(paste("Grouping two sets plots", format(Sys.time(), "%X") ))

#plot(one.group.PAS)

end.time <- Sys.time()

print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))


} 





# VERSION 2: 3 migra groups and the shared-gain-ret

# Maps of 25% or 50% q current suit and and future suit refugia over PAs 
#shaded PAs
#customized colours after reclassifying to Current only, Future only, and overlap

{ begin.time <- Sys.time()

quant<-0.75 ## switch quantile and name for saving!!!!!

groupings_labs <- c("Residents","Short-distance", "Long-distance")# for plotting only

three.cat.list<-NULL
for (m in 1:length(all.group.rasters)) { # for each migratory group
  
  three.cats <- all.group.rasters[[m]][c(1,4)]
  
  current<-three.cats[[1]]
  q75 <- quantile(terra::values(current), probs=c(quant), na.rm=TRUE)
  
  print(paste("Plotting", groupings_labs[m]))
  
  # extract and convert high qual areas for Current into 1 category (value 1)
  new.current <- terra::ifel(current >= q75, 1, 0)
  #terra::plot(new.current)
  # extract and convert high qual areas for Futur refugia into 1 category (value 1)
  new.futRefugia <- terra::ifel(three.cats[[2]] > q75, 2, 0)
  #terra::plot(new.futRefugia)
  
  sumrast <-new.current+new.futRefugia
  
  sumrast <- terra::ifel(sumrast > 0, sumrast, NaN)
  
  unique(terra::values(sumrast))
  #terra::plot(sumrast)
  #test <-terra::as.polygons(sumrast)
  
  #terra::plot(test)
  
  m1<-
    as.matrix(
      data.frame(
        x = c(#min(terra::values(raster_test), na.rm = TRUE)
          0,1,2),
        y = c(1,2,3
              #max(terra::values(raster_test), na.rm = TRUE)
        ),
        z = c(1,2,3) # 1 is present, 2 is future, 3 is overlap
      )  
    )
  
  m1
  
  rr1 <- terra::classify(sumrast, m1, others=NA) 
  
  terra::writeRaster(rr1, paste0("data/",groupings_labs[m],".tiff"),  overwrite=TRUE )
  
  plot.one<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =terra::as.factor(rr1)#, alpha = 0.75
    )+
    #attempt other colours of viridis
    #ggplot2::scale_fill_viridis_c( option = "turbo",  na.value="transparent")+ ### DIANA's paper style?
    
    #ggplot2::scale_fill_manual( 
    #na.value="transparent",
    # values = c("#D55E00","#F0E442", "#0072B2"))+ 
    ggplot2::scale_fill_manual(na.translate = FALSE,
                               na.value="transparent", 
                               values = c("#D55E00","#0072B2","#F0E442"),
                               labels = c("Loss", "Gain", "Retained")
    )+
    #ggnewscale::new_scale_fill()+
    #tidyterra::geom_spatraster(data =new.futRefugia, alpha = 0.25)+
    
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::theme_bw()+
    #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    
    ggplot2::theme(
      legend.position = "bottom",
      axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
      #legend.position = "none",
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm"),
      plot.title =  ggplot2::element_text(hjust = 0.5, face="bold")
    )+
    ggplot2::ggtitle(paste(groupings_labs[m]))+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0) +
    ggplot2::geom_sf(data = usa_crop, alpha = 0)+
    ggplot2::geom_sf(data = canada_crop, alpha = 0)+
    ggplot2::geom_sf(data = yukon_PAs_crs, fill = "#073b4c", color = NA, alpha = 0.35 )+
    ggplot2::coord_sf(xlim=c(terra::ext(new.current)[1], terra::ext(new.current)[2]),
                      ylim = c(terra::ext(new.current)[3], terra::ext(new.current)[4]),
                      expand = FALSE)+
    ggplot2::labs(fill="")
  
  three.cat.list[[m]] <- plot.one # lists the four plots per category
  
} 

joint_gain_ret <- terra::rast("data/Shared_retain_gain.tif")

joint_gain_ret <- terra::ifel(joint_gain_ret == 0, NA, joint_gain_ret)

share_ret_plot  <-
  ggplot2::ggplot()+
  ggplot2::geom_sf(data = poly, fill = "grey") +
  ggplot2::geom_sf(data = usa_crop, fill = "white")+
  ggplot2::geom_sf(data = canada_crop, fill = "white")+
  tidyterra::geom_spatraster(data =terra::as.factor(joint_gain_ret)#, alpha = 0.75
  )+
  #attempt other colours of viridis
  #ggplot2::scale_fill_viridis_c( option = "turbo",  na.value="transparent")+ ### DIANA's paper style?
  
  #ggplot2::scale_fill_manual( 
  #na.value="transparent",
  # values = c("#D55E00","#F0E442", "#0072B2"))+ 
  ggplot2::scale_fill_manual(na.translate = FALSE,
                             na.value="transparent", 
                             values = c("#CC79A7"),
                             labels = c("Shared retained/gained")
  )+
  #ggnewscale::new_scale_fill()+
  #tidyterra::geom_spatraster(data =new.futRefugia, alpha = 0.25)+
  
  ggplot2::geom_sf(data = usa_crop, alpha =0)+
  ggplot2::geom_sf(data = canada_crop, alpha =0)+
  ggplot2::theme_bw()+
  #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
  
  ggplot2::theme(
    legend.position = "bottom",
    axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
    #legend.position = "none",
    plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm"),
    plot.title =  ggplot2::element_text(hjust = 0.5, face="bold")
  )+
  ggplot2::ggtitle("Combined groups")+
  ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
  ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0) +
  ggplot2::geom_sf(data = usa_crop, alpha = 0)+
  ggplot2::geom_sf(data = canada_crop, alpha = 0)+
  ggplot2::geom_sf(data = yukon_PAs_crs, fill = "#073b4c", color = NA, alpha = 0.35 )+
  ggplot2::coord_sf(xlim=c(terra::ext(new.current)[1], terra::ext(new.current)[2]),
                    ylim = c(terra::ext(new.current)[3], terra::ext(new.current)[4]),
                    expand = FALSE)+
  ggplot2::labs(fill="")  

three.cat.list[[4]] <- share_ret_plot

#one.group.PAS <- cowplot::plot_grid(plotlist = three.cat.list, ncol = 3, nrow = 1)

new_one_group_PAS<-three.cat.list |> 
  # Remove the y axis title and the plot title except for the first plot
  purrr::imap(\(x, y) if (!y == 1) x + ggplot2::labs(y = NULL) else x) |> 
  patchwork::wrap_plots(guides = "collect", nrow = 1)  & ggplot2::theme(text= ggplot2::element_text(size=20), legend.position = 'bottom')

ggplot2::ggsave(new_one_group_PAS, filename = "new_one_group_PAS_25quantCurFutv8.png", 
                path = "plots", units = "in", width = 20, height = 7.5, dpi = 300, bg = "white")

print(paste("Grouping two sets plots", format(Sys.time(), "%X") ))

#plot(one.group.PAS)

end.time <- Sys.time()

print(paste("total duration of plotting", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))


} 

### Only for POP based and 
###
### Only for decreasers
###

{
  m<-1
  
  
  three.cats <- all.group.rasters[[m]][c(1,4)]
  
  current<-three.cats[[1]]
  q75 <- quantile(terra::values(current), probs=c(quant), na.rm=TRUE)
  
  print(paste("Plotting", groupings_labs[m]))
  
  # extract and convert high qual areas for Current into 1 category (value 1)
  new.current <- terra::ifel(current >= q75, 1, 0)
  #terra::plot(new.current)
  # extract and convert high qual areas for Futur refugia into 1 category (value 1)
  new.futRefugia <- terra::ifel(three.cats[[2]] > q75, 2, 0)
  #terra::plot(new.futRefugia)
  
  sumrast <-new.current+new.futRefugia
  
  sumrast <- terra::ifel(sumrast > 0, sumrast, NaN)
  
  #unique(terra::values(sumrast))
  #terra::plot(sumrast)
  #test <-terra::as.polygons(sumrast)
  
  #terra::plot(test)
  
  m1<-
    as.matrix(
      data.frame(
        x = c(#min(terra::values(raster_test), na.rm = TRUE)
          0,1,2),
        y = c(1,2,3
              #max(terra::values(raster_test), na.rm = TRUE)
        ),
        z = c(1,2,3) # 1 is present, 2 is future, 3 is overlap
      )  
    )
  
  #m1
  
  rr1 <- terra::classify(sumrast, m1, others=NA) 
  terra::writeRaster(rr1, paste0("data/",groupings_labs[m],".tiff"),  overwrite=TRUE )
  
  plot.one<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =terra::as.factor(rr1)#, alpha = 0.75
    )+
    #attempt other colours of viridis
    #ggplot2::scale_fill_viridis_c( option = "turbo",  na.value="transparent")+ ### DIANA's paper style?
    
    #ggplot2::scale_fill_manual( 
    #na.value="transparent",
    # values = c("#D55E00","#F0E442", "#0072B2"))+ 
    ggplot2::scale_fill_manual(na.translate = FALSE,
                               na.value="transparent", 
                               values = c("#D55E00","#0072B2","#F0E442"),
                               labels = c("Loss", "Gain", "Retained")
    )+
    #ggnewscale::new_scale_fill()+
    #tidyterra::geom_spatraster(data =new.futRefugia, alpha = 0.25)+
    
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::theme_bw()+
    #ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    
    ggplot2::theme(
      text= ggplot2::element_text(size=20),
      legend.position = "bottom",
      axis.text= ggplot2::element_blank(), axis.ticks= ggplot2::element_blank(),
      #legend.position = "none",
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm"),
      plot.title =  ggplot2::element_text(hjust = 0.5, face="bold")
    )+
    #ggplot2::ggtitle(paste(groupings_labs[m]))+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0) +
    ggplot2::geom_sf(data = usa_crop, alpha = 0)+
    ggplot2::geom_sf(data = canada_crop, alpha = 0)+
    ggplot2::geom_sf(data = yukon_PAs_crs, fill = "#073b4c", color = NA, alpha = 0.35 )+
    ggplot2::coord_sf(xlim=c(terra::ext(new.current)[1], terra::ext(new.current)[2]),
                      ylim = c(terra::ext(new.current)[3], terra::ext(new.current)[4]),
                      expand = FALSE)+
    ggplot2::labs(fill="")
  
  
  #ggplot2::ggsave(plot.one, filename = paste0("one.group.PAS_50POP.png"), 
   #               path = "plots", units = "in", width = 6, height = 7.5, dpi = 300, bg = "white")
  
  
  
}





#########################  To share with larger team
#########################  
#########################  
### 5) Plot INDIVIDUAL SPP current distributions densities 
#capped to maxq 95, 
# or over total birds (only for proportions to standardize and stack groups)

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
  the.birds <- (c(RES,SDM, LDM))
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
    
    plot.onebird<-ggplot2::ggplot()+
      ggplot2::geom_sf(data = poly, fill = "grey") +
      ggplot2::geom_sf(data = usa_crop, fill = "white")+
      ggplot2::geom_sf(data = canada_crop, fill = "white")+
      tidyterra::geom_spatraster(data =rast1)+
      ggplot2::geom_sf(data = usa_crop, alpha =0)+
      ggplot2::geom_sf(data = canada_crop, alpha =0)+       
      ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
      ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
      ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                        ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                        expand = FALSE)+
      ggplot2::theme_bw()+
      #ggplot2:: scale_fill_viridis_c( direction = -1, na.value="transparent")+ ### IF NO SCALING for viz
      ggplot2::scale_fill_gradientn(
        colors = c(
          "#F9FFAF",
          hcl.colors(100, palette = "viridis", rev = TRUE),
          "#2D0038FF"
        ),
        values = scales::rescale(
          sort(c(range(terra::values(rast1)), c(zmin, q99))),
          to = c(0, 1)
        ),
        oob = scales::squish,
        limits = c(zmin, q99) , na.value = "transparent"
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
      )+
      ggplot2::ggtitle(names(rast1))
    
    
    #print(paste(grep(i, the.birds),"out of", length(the.birds),", Plotting species:",i))
    print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "density plot", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
    
    
    group_plots[[i]] <- plot.onebird
    all.plots[[i]]<-plot.onebird
  }
  
  print(paste("Grouping plots", k,"in 1 frame", format(Sys.time(), "%X") ))
  
  one.group <-cowplot::plot_grid(plotlist = group_plots, ncol=4 )
  
  print(paste("Saving", k, "plots to disk", format(Sys.time(), "%X") ))
  
  #ggplot2::ggsave(one.group, filename = (paste0(k,"_currDensv6.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(group_spp)/4), limitsize = FALSE, dpi = 300, bg = "white")
  #no scaling plot excluded just used the raw values

 
  }
all.bird.plots <-cowplot::plot_grid(plotlist = all.plots, ncol=4 )

#ggplot2::ggsave(all.bird.plots, filename = (paste0("all_currDensv6.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(the.birds)/4), limitsize = FALSE, dpi = 300, bg = "white")

  end.time <- Sys.time()
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
}


pixel.vals.df<- as.data.frame(pixel.vals)
names(pixel.vals.df) <- c("mig", "spp", "mean.val","median", "lowest", "max.val", "zmin","q90", "q99")

pixel.vals.df<-pixel.vals.df|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), as.numeric)|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), round, 7)

#write.csv(pixel.vals.df, "data/pixel.vals.df.csv")



######### individual spp current suitabilities

{
 
  turbo_pal <- c(viridis::turbo(n = 1000, direction = -1))
  
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
  the.birds <- (c(RES,SDM, LDM))
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

      rast1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","PresentSuit.tif"), files.to.read)])
      
      terra::crs(rast1) <- rast.crs
      names(rast1) <- i
      
      mean.val <- mean(terra::values(rast1), na.rm = TRUE)
      median.val <- median(terra::values(rast1), na.rm = TRUE)
      
      mode.val <- Mode(terra::values(rast1))
      
      max.val<- max(terra::values(rast1), na.rm = TRUE)
      zmin <- max(mean.val, 0.001, na.rm = TRUE)
      
     lowest <- min(terra::values(rast1), na.rm = TRUE)
      
      q99 <- quantile(terra::values(rast1), probs=c(0.999), na.rm=TRUE) #after capping
      q90 <- quantile(terra::values(rast1), probs=c(0.9), na.rm=TRUE) #after capping
      
      pixel.vals <- rbind( pixel.vals, c(k, i, mean.val, median.val, mode.val, lowest, max.val, zmin, q90, q99))
      
      plot.onebird<-ggplot2::ggplot()+
        ggplot2::geom_sf(data = poly, fill = "grey") +
        ggplot2::geom_sf(data = usa_crop, fill = "white")+
        ggplot2::geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =rast1)+
        ggplot2::geom_sf(data = usa_crop, alpha =0)+
        ggplot2::geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=0.5 ,color = "black", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                          ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                          expand = FALSE)+
        ggplot2::theme_bw()+
        #ggplot2:: scale_fill_viridis_c( direction = -1, na.value="transparent")+ ### IF NO SCALING for viz
        ggplot2::scale_fill_gradientn(
          colors = c(
           turbo_pal
          ),
          values = scales::rescale(
            sort(c(range(terra::values(rast1)), c(0, 1))),
            to = c(0, 1)
          ),
          oob = scales::squish,
          limits = c(0, 1) , na.value = "transparent"
        ) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::ggtitle(names(rast1))
      
      
      #print(paste(grep(i, the.birds),"out of", length(the.birds),", Plotting species:",i))
      print(paste("Working", k,grep(i,group_spp),"/", length(group_spp) , "density plot", i,"at", format(Sys.time(), "%X"), length(startnum),"/",(length(c(LDM,SDM,RES)))   ))
      
      
      group_plots[[i]] <- plot.onebird
      all.plots[[i]]<-plot.onebird
    }
    
    print(paste("Grouping plots", k,"in 1 frame", format(Sys.time(), "%X") ))
    
    one.group <-cowplot::plot_grid(plotlist = group_plots, ncol=4 )
    
    print(paste("Saving", k, "plots to disk", format(Sys.time(), "%X") ))
    
    #ggplot2::ggsave(one.group, filename = (paste0(k,"_currSUITv8.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(group_spp)/4), limitsize = FALSE, dpi = 300, bg = "white")
    #no scaling plot excluded just used the raw values
    
    
  }
  all.bird.plots <-cowplot::plot_grid(plotlist = all.plots, ncol=4 )
  
  #ggplot2::ggsave(all.bird.plots, filename = (paste0("all_currSUITv8.png")), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(the.birds)/4), limitsize = FALSE, dpi = 300, bg = "white")
  
  end.time <- Sys.time()
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
  
  }


pixel.vals.df<- as.data.frame(pixel.vals)
names(pixel.vals.df) <- c("mig", "spp", "mean.val","median", "lowest", "max.val", "zmin","q90", "q99")

pixel.vals.df<-pixel.vals.df|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), as.numeric)|>
  dplyr::mutate_at(3:length(names(pixel.vals.df)), round, 7)

#write.csv(pixel.vals.df, "data/pixel.vals.df.csv")




