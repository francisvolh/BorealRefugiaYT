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

df.spp.names.merged <- read.csv("data/df.all.birds.merged.csv", #stringsAsFactors = TRUE, 
                                na.strings=c("NA","NaN", ""))
#names(df.spp.names.merged)
#summary(df.spp.names.merged)

#already merged Diana list of categories to the df.merged, need to assign some missing values
#spp.to.work <- df.spp.names.merged %>% 
 # filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #select(all.birds) %>% 
  #pull()
df.spp.names.merged <- df.spp.names.merged |>
  dplyr::filter(is.na(dropped))

LDM <- df.spp.names.merged %>%
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Neotropical migrant") %>% 
  select(all.birds) %>% 
  pull()

SDM <- df.spp.names.merged %>%
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Short distance migrant") %>% 
  select(all.birds) %>% 
  pull()

RES <- df.spp.names.merged %>%
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Resident") %>% 
  select(all.birds) %>% 
  pull()

NOM <- df.spp.names.merged %>%
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Nomadic") %>% 
  select(all.birds) %>% 
  pull()

N_assig <- df.spp.names.merged %>%
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  #filter(species_code %in% spp.to.work) %>% 
  filter(is.na(Migration1)) %>% 
  select(all.birds) %>% 
  pull()

groupings_labs <- c("RES","LDM","SDM"#,"NOM"#,"N_assig"
                    )
groupings <- c(RES,LDM,SDM#,NOM#,N_assig
               )

#max.len <- max(length(LDM),length(SDM),length(RES))
#SDM.spp = c(SDM, rep(NA, max.len - length(SDM)))
#LDM.spp = c(LDM, rep(NA, max.len - length(LDM)))
#RES.spp = c(RES, rep(NA, max.len - length(RES)))
#cat.spp.df<-data.frame(SDM.spp, LDM.spp, RES.spp)

#load maps
BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_1.shp")
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")

#set refugia raster results directory
#the file list
files.to.read<-list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN", full.names = TRUE)

#the directory route
ref.ras.dir <- "data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/"


# produce a raster to clean Ref, suit, and refxsuit rasters
rast.crs<- terra::crs(terra::rast("data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif")) ## from data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif
Norm1991<-readRDS("data/corrected_rasters_clim_topo/all_rasts_Correct_Ecozones.RDS") #test with my file
Norm1991<-readRDS("data/Norm1991_EcozoneNormals.rds") ## THE ECOZONES FILE FROM ANNA
NAKey<-subset(Norm1991,!is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,!is.na(NAKey$tri))
NA_rast<-terra::rast(NAKey , type="xyz")
NA_rast <-NA_rast[[1]]

NA_rast[!is.na(NA_rast)] <-1
#load a bird refugia raster
bird_rast <- terra::rast("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/ALFL_Refugia_RCPmean.tiff")
NA_rast <-terra::crop(NA_rast,bird_rast)


NA_rast.crs <- NA_rast
terra::crs(NA_rast.crs) <- rast.crs
rm(NAKey) 
rm("Norm1991")

#usa_can_crop <- sf::st_union(canada, USA) keep each country separate

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
rm(canada, USA)
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
AKrast[is.na(AKrast)] <- 10 #assign a random high value to NAs 
AKrast <- terra::ifel(AKrast < 9, NA, AKrast)
#terra::crs(AKrast) <- rast.crs
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
area.bcr<-sf::st_area(BCR4.1_4.0)
units(area.bcr) <- units::as_units("km2")

bcr <- BCR4.1_4.0[1]
bcr <- sf::st_transform(bcr, crs =rast.crs)

#area.bcr
###
}
##########################################################################
## for leaflet plotting only
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
#########################  
### 2) PRODUCE SUMMARY GRAPHS FOR REFUGIA, SUITABILITY, and REFxSUIT scores
# for GROUPING categories (STACKED not including CURRENT DISTRIBUTIONS)
######

#Curent loop runs over all predefined groups
{
  begin.time <- Sys.time()
  
  group_plots<- list()
  
  #loop for to run all groups
  for (k in groupings_labs) {
    
    #for one group
    
    group_spp <- get(k)
    
    ref.mean.list <- list()
    suit.mean.list <- list()
    refxsuit.mean.list <-  list()
    
    ref.ras1<-NULL
    suit.ras1<-NULL
    refxsuit.ras1<-NULL
    
    for (i in c(group_spp)) {
      
      #rasters are sometimes tif or tiff, so the grep() solves it
      ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
      name1<-terra::varnames(ref.ras1)
      ref.ras1 <- terra::mask(ref.ras1, NA_rast)#mask using NAs from env variables 
      ref.ras1 <- terra::mask(ref.ras1, AKrast)
      
      terra::varnames(ref.ras1)<-name1
      suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
      suit.ras1 <- terra::mask(suit.ras1, NA_rast)#mask using NAs from env variables 
      suit.ras1 <- terra::mask(suit.ras1, AKrast)
      
      
      refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, NA_rast)#mask using NAs from env variables 
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, AKrast)
      
      names(refxsuit.ras1) <- terra::varnames(refxsuit.ras1)
      names(suit.ras1) <- terra::varnames(suit.ras1)
      
      ref.mean.list[[i]] <- ref.ras1
      suit.mean.list[[i]] <- suit.ras1
      refxsuit.mean.list[[i]] <- refxsuit.ras1
      
    }
    
    
    # for all RES, LDM, SDM did sum
    #for SDM also did average to compare that visually look the same, Diana agrees they are the same
    
    ref.sum <- terra::app(terra::rast(ref.mean.list), "sum")#/length(ref.mean.list)
    #assign(paste0(k,"ref.sum"), ref.sum) #rename object according to loop cycle (spp grouping)
    
################attempt to estimate area size of 75% refugia index
#### incorporate into graphs--- rather than print()
   # ref.sum.val75 <- max(terra::values(ref.sum), na.rm = TRUE)-max(terra::values(ref.sum), na.rm = TRUE)/4
    
  #  ref.sum.test <- ifelse(ref.sum[]>=ref.sum.val75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
   # area.ref <-sum(ref.sum.test, na.rm = TRUE)
    
    #this should be the same as below, with cellSize
  
    #print(paste("area greater than 0.75.=",round(area.ref,2), "km^2"))
    #print(paste("Percentage relative to BCR4.0-4.1 is", round((area.ref/as.numeric(area.bcr))*100,2), "%"))

    ###################################### using cell size does work, and it may be more precise #########################
    #ref.sum.75.crs <- ref.sum
    #terra::crs(ref.sum.75.crs) <- rast.crs 
    
    #ref.sum.75.crs[ref.sum.75.crs[]<ref.sum.val75]<-NA
    
    #cell_size<-terra::cellSize(ref.sum.75.crs, 
            #                 unit = "km", #transform=TRUE
             #                 mask = TRUE )
    #b_sum <- terra::global(cell_size, fun = "sum", na.rm = TRUE)
    
    #######terra::expanse(ref.sum.75.crs, unit = "km") 
    ## using expanse after making the NA below the threshold
    #would yield the same but would need more time to compute, x9 times more time
    
    ################################################################################################################## 
    
    
    suit.sum <- terra::app(terra::rast(suit.mean.list), "sum")#/length(suit.mean.list)
    #assign(paste0(k,"suit.sum"), suit.sum)

    
    
    refxsuit.sum <- terra::app(terra::rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)
    #assign(paste0(k,"refxsuit.sum"), refxsuit.sum)
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    
    
    #make sets of plots of only the 3 categories (ref, suit, ref x suit) per species grouping (res, ldm, sdm)
    three.cats  <- c(ref.sum, suit.sum, refxsuit.sum)
    three.cats.names  <- c(paste0(k,".refugia.sum"),paste0(k,".suitability.sum"),paste0(k,".refugia x suit.sum"))
    three.cats.vals <- c(area.ref,area.suit, area.refxsuit )
    
  three.cat.list<- list()
    for (j in 1:length(names(three.cats))) {
      
      rast1 <- three.cats[[j]]
      
        plot.one <- ggplot()+
        geom_sf(data = poly, fill = "grey") +
        geom_sf(data = usa_crop, fill = "white")+
        geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =rast1)+
        geom_sf(data = usa_crop, alpha =0)+
        geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, aes(), linewidth=1.1 ,color = "black", fill = "blue", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, aes(), linewidth=1. ,color = "black", fill = "red", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                          ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                          expand = FALSE)+
        #scale_fill_terrain_c()+ ###WORKS! may be best because of white color on zeros,   na.value = NA not doing anything in this 
        #scale_fill_distiller()+
        #scale_fill_binned(type = "viridis")
        #scale_fill_continuous(type = "viridis")+
        #scale_fill_gradient(low="red", high="green")+
        ggplot2::theme_bw()+
        ggplot2::ggtitle(three.cats.names[[j]])+
        ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="transparent")+ ### DIANA's paper style?
        ggplot2::theme(
          plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
        )#+
        #ggplot2::annotate("text", label=paste("High val area ", round(three.cats.vals[j],2)),
         #                 x=(-2291000), 
          #                y=( 1680000))+
        #ggplot2::annotate("text", label=paste("High val areas rel to BCR", round((three.cats.vals[j]/as.numeric(area.bcr))*100,2), "%"),
         #                 x=(-2291000), 
          #                y=( 1630000))
      
      three.cat.list[[j]] <- plot.one
      
    }
    
    three.plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 3 )
    #print(three.plots)
    #ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
    
    group_plots[[k]]<- three.plots
  }
  
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )
  
 # ggsave(group_plots.png, filename = "group_plots.v6.png", path = "plots/", units = "in", width = 22, height = 20, dpi = 300, bg = "white")
  
  
  end.time <- Sys.time()
  
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
}

group_plots.png

########################
### 3) PRODUCE SUMMARY GRAPHS FOR CURRENT DISTRIBUTION, REFUGIA, SUITABILITY, 
# and REFxSUIT scores (STACKED, includes present distributions)
# for GROUPING categories 

#Curent loop runs over all predefined groups

{
  files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)
  
  
  begin.time <- Sys.time()
  
  group_plots<- list()
  
  #loop for to run all groups
  for (k in groupings_labs) {
    
    #for one group
    
    group_spp <- get(k)
    
    curr.rast.list <- list()
    ref.mean.list <- list()
    suit.mean.list <- list()
    refxsuit.mean.list <-  list()
    
    for (i in c(group_spp)) {
      
      #load current distributions RDS
      sample1 <- readRDS(files.vect[grep(i, files.vect)] )
      
      sample1 <- sample1 |>
        dplyr::mutate(
          std.mean = Mean/max(Mean)
        )
      rast1<-terra::rast(sample1)
      #terra::crs(rast1) <- rast.crs
      rm(sample1)
      rast1 <- rast1[[4]]
      rast1 <-  terra::crop(rast1, NA_rast)
      rast1 <- terra::mask(rast1, NA_rast)#mask using NAs from env variables 
      rast1 <- terra::mask(rast1, AKrast)
      
      names(rast1) <- i
      
   
      #loads the refugia rasters
      #rasters are sometimes tif or tiff, so the grep() solves it
      ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
      name1<-terra::varnames(ref.ras1)
      ref.ras1 <- terra::mask(ref.ras1, NA_rast)#mask using NAs from env variables 
      ref.ras1 <- terra::mask(ref.ras1, AKrast)
      terra::crs(ref.ras1) <- rast.crs
      
      terra::varnames(ref.ras1)<-name1
      suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
      
      suit.ras1 <- terra::mask(suit.ras1, NA_rast)#mask using NAs from env variables 
      suit.ras1 <- terra::mask(suit.ras1, AKrast)
      terra::crs(suit.ras1) <- rast.crs
      
      
      refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, NA_rast)#mask using NAs from env variables 
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, AKrast)
      terra::crs(refxsuit.ras1) <- rast.crs
      
      names(refxsuit.ras1) <- terra::varnames(refxsuit.ras1)
      names(suit.ras1) <- terra::varnames(suit.ras1)
      
      curr.rast.list[[i]] <- rast1
      ref.mean.list[[i]] <- ref.ras1
      suit.mean.list[[i]] <- suit.ras1
      refxsuit.mean.list[[i]] <- refxsuit.ras1
      
    }
    
    
    # for all RES, LDM, SDM did sum
    #for SDM also did average to compare that visually look the same, Diana agrees they are the same
    
    curr.sum <- terra::app(terra::rast(curr.rast.list), "sum")#/length(ref.mean.list)
    
    y <- mask(curr.sum, bcr[1])
    
    q10<-quantile(terra::values(y), probs=c(0.1), na.rm=TRUE)
    curr.sum.test <- ifelse(terra::values(y)>=q10, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    
    #curr.sum.val75 <- max(terra::values(y), na.rm = TRUE) - max(terra::values(y), na.rm = TRUE)/4
    #curr.sum.test <- ifelse(curr.sum[]>=curr.sum.val75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    
    
    #modifications to just ad up all cells with a current distribution that is not NA
    area.curr <-sum(curr.sum.test, na.rm = TRUE)
    
    
    ref.sum <- terra::app(terra::rast(ref.mean.list), "sum")#/length(ref.mean.list)
    #assign(paste0(k,"ref.sum"), ref.sum) #rename object according to loop cycle (spp grouping)
    y <- mask(ref.sum, bcr[1])
    ref.sum.val75 <- max(terra::values(y), na.rm = TRUE)-max(terra::values(y), na.rm = TRUE)/4
    ref.sum.test <- ifelse(terra::values(y)>=ref.sum.val75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.ref <-sum(ref.sum.test, na.rm = TRUE)
    
    
    suit.sum <- terra::app(terra::rast(suit.mean.list), "sum")#/length(suit.mean.list)
    y <- mask(suit.sum, bcr[1])
    
    #assign(paste0(k,"suit.sum"), suit.sum)
    
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    suit.sum.val75 <- max(terra::values(y), na.rm = TRUE)-max(terra::values(y), na.rm = TRUE)/4
    suit.sum.test <- ifelse(terra::values(y)>=suit.sum.val75, 1, NA)
    area.suit <-sum(suit.sum.test, na.rm = TRUE)
    
    
    
    refxsuit.sum <- terra::app(terra::rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)
    #assign(paste0(k,"refxsuit.sum"), refxsuit.sum)
    y <- mask(refxsuit.sum, bcr[1])
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    refxsuit.sum.val75 <- max(terra::values(y), na.rm = TRUE)-max(terra::values(y), na.rm = TRUE)/4
    refxsuit.test <- ifelse(refxsuit.sum[]>=refxsuit.sum.val75, 1, NA)
    area.refxsuit <-sum(refxsuit.test, na.rm = TRUE)
    
    
    #make sets of plots of only the 3 categories (ref, suit, ref x suit) per species grouping (res, ldm, sdm)
    three.cats  <- c(curr.sum, ref.sum, suit.sum, refxsuit.sum) # vector of rasters
    three.cats.names  <- c(paste0(k,".current.sum"), paste0(k,".refugia.sum"),paste0(k,".suitability.sum"),paste0(k,".refugia x suit.sum")) # vector of names
    three.cats.vals <- c(area.curr, area.ref, area.suit, area.refxsuit )
    
    three.cat.list<- list()
    for (j in 1:length(names(three.cats))) {
      
      rast1 <- three.cats[[j]]# a raster
      if (j == 1) {
        
        # truncate values for better viz
        mean.val <- mean(terra::values(rast1), na.rm = TRUE)
        low.val<- min(terra::values(rast1), na.rm = TRUE)
        zmin <- max(mean.val, 0.001, na.rm = TRUE)
        zmin <- max(zmin, 0.01, na.rm = TRUE)
        zmax <- max(terra::values(rast1), na.rm = TRUE)
        q99 <- quantile(terra::values(rast1), probs=c(0.999), na.rm=TRUE)
        
        plot.one<-ggplot2::ggplot()+
          ggplot2::geom_sf(data = poly, fill = "grey") +
          ggplot2::geom_sf(data = usa_crop, fill = "white")+
          ggplot2::geom_sf(data = canada_crop, fill = "white")+
          tidyterra::geom_spatraster(data =rast1)+
          ggplot2::geom_sf(data = usa_crop, alpha =0)+
          ggplot2::geom_sf(data = canada_crop, alpha =0)+       
          ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
          ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
          ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                            ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                            expand = FALSE)+
          ggplot2::theme_bw()+
          #ggplot2:: scale_fill_viridis_c( direction = -1, na.value="transparent")+ ### DIANA's paper style?
          scale_fill_gradientn(
            na.value = "transparent",
            colors = c(
              "#F9FFAF",
              hcl.colors(100, palette = "viridis", rev = TRUE),
              "#255668"
            ),
            values = scales::rescale(
              sort(c(range(terra::values(rast1)), c(zmin, q99))),
              to = c(0, 1)
            ),
            oob = scales::squish,
            limits = c(zmin, q99)
          ) +
          
          ggplot2::theme(
            plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
          )+
          ggplot2::ggtitle(three.cats.names[[j]])+
          ggplot2::annotate("text", label=paste("High val area ", round(three.cats.vals[j],2)),
                            x=(-2291000), 
                            y=( 1680000))+
          ggplot2::annotate("text", label=paste("High val areas rel to BCR", round((three.cats.vals[j]/as.numeric(area.bcr))*100,2), "%"),
                            x=(-2291000), 
                            y=( 1630000))
        
        print(paste("Plotting",three.cats.names[[j]], format(Sys.time(), "%X") ))
        
      }else{plot.one <- ggplot()+
        geom_sf(data = poly, fill = "grey") +
        geom_sf(data = usa_crop, fill = "white")+
        geom_sf(data = canada_crop, fill = "white")+
        tidyterra::geom_spatraster(data =rast1)+
        geom_sf(data = usa_crop, alpha =0)+
        geom_sf(data = canada_crop, alpha =0)+       
        ggplot2::geom_sf(data = BCR4.1_USACAN, aes(), linewidth=1.1 ,color = "black", fill = "blue", alpha = 0)+
        ggplot2::geom_sf(data = BCR4.0_USACAN, aes(), linewidth=1. ,color = "black", fill = "red", alpha = 0)+
        ggplot2::coord_sf(xlim=c(terra::ext(ref.sum)[1], terra::ext(ref.sum)[2]),
                          ylim = c(terra::ext(ref.sum)[3], terra::ext(ref.sum)[4]),
                          expand = FALSE)+
        #scale_fill_terrain_c()+ ###WORKS! may be best because of white color on zeros,   na.value = NA not doing anything in this 
        #scale_fill_distiller()+
        #scale_fill_binned(type = "viridis")
        #scale_fill_continuous(type = "viridis")+
        #scale_fill_gradient(low="red", high="green")+
        ggplot2::theme_bw()+
        ggplot2::ggtitle(three.cats.names[[j]])+
        ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="transparent")+ ### DIANA's paper style?
        ggplot2::theme(
          plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::annotate("text", label=paste("High val area ", round(three.cats.vals[j],2)),
                          x=(-2291000), 
                          y=( 1680000))+
        ggplot2::annotate("text", label=paste("High val areas rel to BCR", round((three.cats.vals[j]/as.numeric(area.bcr))*100,2), "%"),
                          x=(-2291000), 
                          y=( 1630000))
      
      print(paste("Plotting",three.cats.names[[j]], Sys.time() ))
      
      }
      three.cat.list[[j]] <- plot.one
      
    }
    
    print(paste("Grouping three sets plots", format(Sys.time(), "%X") ))
    
    three.plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 4 )
    #print(three.plots)
    #ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
    
    group_plots[[k]]<- three.plots
  }
  
  print(paste("Grouping all plots", format(Sys.time(), "%X") ))
  
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )
  
  print(paste("Saving plots to disk", format(Sys.time(), "%X") ))
  
  ggsave(group_plots.png, filename = "group_plots.v10.png", path = "plots/", units = "in", width = 30, height = 20, dpi = 300, bg = "white")
  
  
  end.time <- Sys.time()
  
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
}





##############################################################
##############################################################
##############################################################
# 4) PLOT all species within a category in independent sets per group
###### for all birds in a category #####


#Curent loop runs over all predefined groups
{begin.time <- Sys.time()
  
  all.3.sets<-list()
  
  #loop for to run all groups
  for (k in groupings_labs) {
    print(paste("Starting", k,"at", begin.time))
    group_plots<- list()### list of plots for each category (to be saved with ggsave)
    #for one group
    
    group_spp <- get(k)
    
    #compile each set of graphs: ref, suit, ref x suit for all spp within a group (either LDM, SDM, or RES)
    ref.mean.list <- list()
    suit.mean.list <- list()
    refxsuit.mean.list <- list()
    
    
    all.spp.plot.names <-  list()
    
    #ref.ras1<-NULL
    #suit.ras1<-NULL
    #refxsuit.ras1<-NULL
    
    for (i in c(group_spp)) {
      
      #rasters are sometimes tif or tiff, so the grep() solves it
      ref.ras1 <- terra::rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
      names(ref.ras1)<-terra::varnames(ref.ras1)
      ref.ras1 <- terra::mask(ref.ras1, NA_rast)#mask using NAs from env variables 
      ref.ras1 <- terra::mask(ref.ras1, AKrast)
      
      suit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
      names(suit.ras1) <- terra::varnames(suit.ras1)
      suit.ras1 <- terra::mask(suit.ras1, NA_rast)#mask using NAs from env variables 
      suit.ras1 <- terra::mask(suit.ras1, AKrast)
      
      refxsuit.ras1 <- terra::rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
      names(refxsuit.ras1) <- terra::varnames(refxsuit.ras1)
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, NA_rast)#mask using NAs from env variables 
      refxsuit.ras1 <- terra::mask(refxsuit.ras1, AKrast)
      
    
      
      ref.mean.list[[i]] <- ref.ras1
      suit.mean.list[[i]] <- suit.ras1
      refxsuit.mean.list[[i]] <- refxsuit.ras1
      
      #all.spp.plot.names[[i]] <- c(paste0(i,".refugia"),paste0(i,".suitability"),paste0(i,".refugia x suit"))
    
      }
    
#need to calculate area and percent per bird?? I DONT THINK SO
    #three.cats.vals <- c(area.ref,area.suit, area.refxsuit )
    
    #make a vector of the lists of rasters (per grouping category)
    #three.graphs.all.spp  <- c(ref.mean.list, suit.mean.list, refxsuit.mean.list)
    
    
    for (j in 1:length(names(ref.mean.list))) {
     
      a<-ref.mean.list[[j]] 
      b<-suit.mean.list[[j]] 
      c<-refxsuit.mean.list[[j]] 
      
      setplots<-c("a", "b", "c")
      
      one.spp.plot.cat.list<- list() # this will have the 3 plots of each species: a, b, c
      for (t in setplots) { # do it for each element of the 3 lists (each species) 
         rast1<-get(t)
         plot.one <- ggplot()+
          geom_sf(data = poly, fill = "grey") +
          geom_sf(data = usa_crop, fill = "white")+
          geom_sf(data = canada_crop, fill = "white")+
          tidyterra::geom_spatraster(data =rast1)+
          geom_sf(data = usa_crop, alpha =0)+
          geom_sf(data = canada_crop, alpha =0)+       
          ggplot2::geom_sf(data = BCR4.1_USACAN, aes(), linewidth=1.1 ,color = "black", alpha = 0)+
          ggplot2::geom_sf(data = BCR4.0_USACAN, aes(), linewidth=1.1 ,color = "black", alpha = 0)+
          ggplot2::coord_sf(xlim=c(terra::ext(rast1)[1], terra::ext(rast1)[2]),
                            ylim = c(terra::ext(rast1)[3], terra::ext(rast1)[4]),
                            expand = FALSE)+
          ggplot2::theme_bw()+
          ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="transparent")+ ### DIANA's paper style?
          ggplot2::theme(
            plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
          )+
          ggplot2::ggtitle(names(rast1))
        print(paste(j,"out of", length(names(ref.mean.list)),"Plotting", k, "species:",names(rast1)))
         one.spp.plot.cat.list[[t]]<- plot.one
       }
      
    three.plots <- cowplot::plot_grid(plotlist = one.spp.plot.cat.list, nrow = 1, ncol = 3 )
    #ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
    
    group_plots[[j]]<- three.plots
    } 
    
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = length(names(ref.mean.list)), ncol = 1 ) # rows set to the whole number of plots for now, may want to figure out how to save in chunks once this works
  #maybe cow plot by chunks? 

      #graph.rows <-  ceiling(length(list.bird.plots)/3) 
  
  ggsave(group_plots.png, filename = paste0(k,"_spp_plots.png"), path = "plots/", units = "in", width = 12, height=  3*length(names(ref.mean.list)), limitsize = FALSE, dpi = 300, bg = "white")
  
  all.3.sets [[k]] <- group_plots.png
   
  }
  
  end.time <- Sys.time()
  
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
}


#saveRDS(all.3.sets, file = "data/plot.all.spp.3.sets.RDS") # saving freezes, file is about 28GB


#################################################################################
### 5) Plot INDIVIDUAL SPP current distributions, scaled as 0 and 1 only, or over total birds (proportion to standardize and stack groups)

files.vect <-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds/",  full.names = TRUE)
the.birds <- (c(RES,LDM,SDM))
the.birds <- sort(the.birds)


NA_rast1 <-terra::crop(NA_rast,bird_rast)
AKrast1 <-terra::crop(AKrast,bird_rast)

{
list.rasters.curr <- list()
begin.time <- Sys.time()
for (i in the.birds) {
  
  sample1 <- readRDS(files.vect[grep(i, files.vect)] )
  
  sample1 <- sample1 |>
    dplyr::mutate(
      std.mean = Mean/max(Mean)
    )
  rast1<-terra::rast(sample1)
  
  rm(sample1)
  rast1 <- rast1[[4]]
  rast1 <-  terra::crop(rast1, NA_rast)
  rast1 <- terra::mask(rast1, NA_rast)#mask using NAs from env variables 
  rast1 <- terra::mask(rast1, AKrast)
  names(rast1) <- i
  
  mean.val <- mean(terra::values(rast1), na.rm = TRUE)
  low.val<- min(terra::values(rast1), na.rm = TRUE)
  zmin <- max(mean.val, 0.001, na.rm = TRUE)
  zmin <- max(zmin, 0.01, na.rm = TRUE)
  zmax <- max(terra::values(rast1), na.rm = TRUE)
  q99 <- quantile(terra::values(rast1), probs=c(0.999), na.rm=TRUE)
  
  plot.onebird<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =rast1)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                      ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    #ggplot2:: scale_fill_viridis_c( direction = -1, na.value="transparent")+ ### DIANA's paper style?
    scale_fill_gradientn(
      colors = c(
        "#F9FFAF",
        hcl.colors(100, palette = "viridis", rev = TRUE),
        "#255668"
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
  print(paste(grep(i, the.birds),"out of", length(the.birds),", Plotting species:",i))
  
  
  list.rasters.curr[[i]] <- plot.onebird
}

print(paste("Grouping plots in 1 frame", format(Sys.time(), "%X") ))

all_birds_curr <-cowplot::plot_grid(plotlist = list.rasters.curr, ncol=4 )

print(paste("Saving plots to disk", format(Sys.time(), "%X") ))

ggsave(all_birds_curr, filename = ("all_birds_currv3.png"), path = "plots/", units = "in", width = 16, height=  3*ceiling(length(the.birds)/4), limitsize = FALSE, dpi = 300, bg = "white")

end.time <- Sys.time()
print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))

}



##############################################################
##############################################################
### ONE plot only of the set of ref, or suit, or ref x suit FOR TESTING ONLY
plot.sum<- ggplot()+
  geom_spatraster(data =SDM.ref.sum)+
  #geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0)+
  geom_sf(data = BCR4.1_USACAN, aes(), linewidth=1.5 ,color = "black", fill = "blue", alpha = 0)+
  geom_sf(data = BCR4.0_USACAN, aes(), linewidth=1.5 ,color = "black", fill = "red", alpha = 0)+
  coord_sf(xlim=c(ext(RES.ref.sum)[1], ext(RES.ref.sum)[2]),
           ylim = c(ext(RES.ref.sum)[3], ext(RES.ref.sum)[4]),
           expand = FALSE)+
  #scale_fill_terrain_c()+ ###WORKS! may be best because of white color on zeros,   na.value = NA not doing anything in this 
  #scale_fill_distiller()+
  #scale_fill_binned(type = "viridis")
  #scale_fill_continuous(type = "viridis")+
  #scale_fill_gradient(low="red", high="green")+
  theme_bw()+
  ggtitle("Sum SDM Birds")+
  scale_fill_viridis_c(option = "turbo", direction = -1)+ ### DIANA's paper style?
  theme(
    plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
  )


#################################################################################
#################### additional code to look at stuff######################
sample.birds.rast <- rast(file.choose())

crs(sample.birds.rast)<-crs(usa_can_crop)

usa_can_crop <- st_crop(usa_can_crop, sample.birds.rast)

ggplot()+
  geom_spatraster(data =sample.birds.rast)+
  scale_colour_viridis_c(na.value = NA)+
  geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0)+
  geom_sf(data = BCR4.1_USACAN, aes(), linewidth=2 ,color = "red", fill = "blue", alpha = 0)+
  geom_sf(data = BCR4.0_USACAN, aes(), linewidth=2 ,color = "red", fill = "red", alpha = 0)+
  coord_sf(xlim=c(ext(sample.birds.rast)[1], ext(sample.birds.rast)[2]),
           ylim = c(ext(sample.birds.rast)[3], ext(sample.birds.rast)[4]),
           expand = FALSE)+
  scale_fill_terrain_c(na.value = NA)+
  theme(
    plot.margin = margin(0,0,0,0, "cm")
  )+
  theme_bw()

#viridis paletes
#"magma" (or "A")
#"inferno" (or "B")
#"plasma" (or "C")
#"viridis" (or "D")
#"cividis" (or "E")
#"rocket" (or "F")
#"mako" (or "G")
#"turbo" (or "H")

ggplot()+
  geom_spatraster(data =sample.birds.rast)+
  geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0)+
  geom_sf(data = BCR4.1_USACAN, aes(), linewidth=2 ,color = "red", fill = "blue", alpha = 0)+
  geom_sf(data = BCR4.0_USACAN, aes(), linewidth=2 ,color = "red", fill = "red", alpha = 0)+
  coord_sf(xlim=c(ext(sample.birds.rast)[1], ext(sample.birds.rast)[2]),
           ylim = c(ext(sample.birds.rast)[3], ext(sample.birds.rast)[4]),
           expand = FALSE)+
  #scale_fill_terrain_c()+ ###WORKS! may be best because of white color on zeros,   na.value = NA not doing anything in this 
  #scale_fill_distiller()+
  #scale_fill_binned(type = "viridis")
  #scale_fill_continuous(type = "viridis")+
  #scale_fill_gradient(low="red", high="green")+
  theme_bw()+
  scale_fill_viridis_c(option = "turbo")+ ### DIANA's paper style?
  theme(
    plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
  )

