library(ggplot2)
library(dplyr)
library(terra)
library(sf)
library(tidyterra)


#################################################################################### 
################### Produce group classification to sum and plot ################### 


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

groupings_labs <- c("LDM","SDM","RES"#,"NOM"#,"N_assig"
                    )
groupings <- c(LDM,SDM,RES#,NOM#,N_assig
               )

max.len <- max(length(LDM),length(SDM),length(RES))
SDM.spp = c(SDM, rep(NA, max.len - length(SDM)))
LDM.spp = c(LDM, rep(NA, max.len - length(LDM)))
RES.spp = c(RES, rep(NA, max.len - length(RES)))
cat.spp.df<-data.frame(SDM.spp, LDM.spp, RES.spp)

#load maps
BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_0.shp")
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")

#set refugia raster results directory
#the file list
files.to.read<-list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN", full.names = TRUE)

#the directory route
ref.ras.dir <- "data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/"


# produce a raster to clean Ref, suit, and refxsuit rasters
rast.crs<- terra::crs(terra::rast("data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif")) ## from data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif
Norm1991<-readRDS(file.choose()) #test with my file
Norm1991<-readRDS("data/Norm1991_EcozoneNormals.rds") ## THE ECOZONES FILE FROM ANNA
NAKey<-subset(Norm1991,!is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,!is.na(NAKey$tri))
NA_rast<-terra::rast(NAKey , type="xyz")
NA_rast[!is.na(NA_rast)] <-1
#load a bird refugia raster
bird_rast <- terra::rast("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/ALFL_Refugia_RCPmean.tiff")
NA_rast <-terra::crop(NA_rast,bird_rast)
NA_rast <-NA_rast[[1]]

NA_rast.crs <- NA_rast
terra::crs(NA_rast.crs) <- rast.crs
rm(NAKey) 
rm("Norm1991")


ggplot()+
  geom_sf(data = usa_can_crop)+
  geom_spatraster(data = NA_rast.crs)

### produce a key to clean present distribution rasters, will work on RDS files, before rasterizing it to plot, 
###### but not needed for me
#NAKey<-paste(NAKey$x,NAKey$y,sep=".") #raster squares with complete covariates only

# Import key of masked AK regions (AK portion of BCR 3 + BCR 2) -------------------------------------
AK_remove<-read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/AK_removalregion.csv")
AK_remove<-terra::rast(AK_remove[,2:4] , type="xyz")
AKrast <- AK_remove
AKrast[is.na(AKrast)] <- 10 #assign a random high value to NAs 
AKrast <- ifel(AKrast < 9, NA, AKrast)
#terra::crs(AKrast) <- rast.crs
AKrast <-terra::crop(AKrast, NA_rast)


usa_can_crop <- sf::st_union(canada, USA) 
usa_can_crop <- sf::st_transform(usa_can_crop, rast.crs )
usa_can_crop <- st_crop(usa_can_crop, c(xmin=as.numeric(terra::ext(bird_rast)[1]), 
                                        xmax=as.numeric(terra::ext(bird_rast)[2]), 
                                        ymin=as.numeric(terra::ext(bird_rast)[3]), 
                                        ymax=as.numeric(terra::ext(bird_rast)[4])
                                        ))

AA<-ggplot()+
  #geom_sf(data = usa_can_crop)+
  geom_spatraster(data = AK_remove)

BB<-ggplot()+
  geom_spatraster(data = AK_remove)+
  geom_sf(data = usa_can_crop, fill = "yellow", alpha = 0.3)
cowplot::plot_grid(AA, BB, nrow = 1)         
#AK_Key<-paste(AK_remove$x,AK_remove$y,sep=".")

### adding calculation of BCR 4.1 and 4.0 area
BCR4.1_4.0 <- sf::st_union(BCR4.1_USACAN, BCR4.0_USACAN)
area.bcr<-sf::st_area(BCR4.1_4.0)
units(area.bcr) <- units::as_units("km2")
area.bcr
###
#########################    


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
#### incorporate into graphs??? rather than print()
    ref.sum.75 <- ref.sum
    ref.sum.val75 <- max(terra::values(ref.sum), na.rm = TRUE)-max(terra::values(ref.sum), na.rm = TRUE)/4
    
    ref.sum.test <- ifelse(ref.sum[]>=ref.sum.val75, 1, NA) #using lambert equal area allows for this assignment of 1 (sq km) to all cells
    area.ref <-sum(ref.sum.test, na.rm = TRUE)
    
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
    
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    suit.sum.val75 <- max(terra::values(suit.sum), na.rm = TRUE)-max(terra::values(suit.sum), na.rm = TRUE)/4
    suit.sum.test <- ifelse(suit.sum[]>=suit.sum.val75, 1, NA)
    area.suit <-sum(suit.sum.test[], na.rm = TRUE)
    
    
    
    refxsuit.sum <- terra::app(terra::rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)
    #assign(paste0(k,"refxsuit.sum"), refxsuit.sum)
    #calculate 75% and max values for each sum of categories per set (ref, suit, ref x suit per each RES, SDM and LDM) 
    refxsuit.sum.val75 <- max(terra::values(refxsuit.sum), na.rm = TRUE)-max(terra::values(refxsuit.sum), na.rm = TRUE)/4
    refxsuit.test <- ifelse(refxsuit.sum[]>=refxsuit.sum.val75, 1, NA)
    area.refxsuit <-sum(refxsuit.test[], na.rm = TRUE)
    
    
    #make sets of plots of only the 3 categories (ref, suit, ref x suit) per species grouping (res, ldm, sdm)
    three.cats  <- c(ref.sum, suit.sum, refxsuit.sum)
    three.cats.names  <- c(paste0(k,".refugia.sum"),paste0(k,".suitability.sum"),paste0(k,".refugia x suit.sum"))
    three.cats.vals <- c(area.ref,area.suit, area.refxsuit )
    
    #no need of this if running all in a loop
    #thre.cats  <- c(LDM.ref.sum,LDM.suit.sum,LDM.refxsuit.sum)
    #thre.cats.names  <- c("LDM.ref.sum","LDM.suit.sum","LDM.refxsuit.sum")
    
    #thre.cats  <- c(SDM.ref.sum,SDM.suit.sum,SDM.refxsuit.sum)
    #thre.cats.names  <- c("SDM.ref.sum","SDM.suit.sum","SDM.refxsuit.sum")
    
    #thre.cats  <- c(SDM.ref.ave,SDM.suit.ave,SDM.refxsuit.ave)
    #thre.cats.names  <- c("SDM.ref.ave","SDM.suit.ave","SDM.refxsuit.ave")
    
    #thre.cats  <- c(LDM.ref.ave,LDM.suit.ave,LDM.refxsuit.ave)
    #thre.cats.names  <- c("LDM.ref.ave","LDM.suit.ave","LDM.refxsuit.ave")
    
    
    three.cat.list<- list()
    for (j in 1:length(names(three.cats))) {
      
      rast1 <- three.cats[[j]]
      
      plot.one <- ggplot()+
        tidyterra::geom_spatraster(data =rast1)+
        ggplot2::geom_sf(data = usa_can_crop, aes(), alpha = 0 )+
       #geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0)+        
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
        ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="white")+ ### DIANA's paper style?
        ggplot2::theme(
          plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
        )+
        ggplot2::annotate("text", label=paste("High val area ", round(three.cats.vals[j],2)),
                          x=(-2291000), 
                          y=( 1680000))+
        ggplot2::annotate("text", label=paste("High val areas rel to BCR", round((three.cats.vals[j]/as.numeric(area.bcr))*100,2), "%"),
                          x=(-2291000), 
                          y=( 1630000))
      
      three.cat.list[[j]] <- plot.one
      
    }
    
    three.plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 3 )
    #print(three.plots)
    #ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
    
    group_plots[[k]]<- three.plots
  }
  
  group_plots.png <-cowplot::plot_grid(plotlist = group_plots, nrow = 3, ncol = 1 )
  
  ggsave(group_plots.png, filename = "group_plots.5.png", path = "plots/", units = "in", width = 22, height = 20, dpi = 300, bg = "white")
  
  
  end.time <- Sys.time()
  
  print(paste("total duration of run", round(difftime(end.time,begin.time, units = "mins"),2), "mins"))
}

group_plots.png


##############################################################

### ONE plot only of the set of ref, or suit, or ref x suit
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

###### for all birds in a category #####

list.bird.plots <- list()
for (j in 1:length((ref.mean.list))) { #use layer() for a spat Stack raster
  
  rast1 <- ref.mean.list[[j]]
  
  plot.one <- ggplot()+
    geom_spatraster(data =rast1)+
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
    ggtitle(varnames(rast1))+
    scale_fill_viridis_c(option = "turbo", direction = -1)+ ### DIANA's paper style?
    theme(
      plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
    )
  
  list.bird.plots[[j]] <- plot.one
  
}

list.bird.plots[[length(ref.mean.list)+1]] <- plot.sum
graph.rows <-  ceiling(length(list.bird.plots)/3) 
all.plots <- cowplot::plot_grid(plotlist = list.bird.plots, nrow = graph.rows, ncol = 3 )
ggsave(all.plots, filename = "all.plots.SDM.png", path = "plots/", units = "in", width = 10, height = 3*graph.rows, dpi = 300, bg = "white")

#########

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

