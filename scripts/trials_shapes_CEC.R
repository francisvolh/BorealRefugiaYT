library(dplyr)
library(ggplot2)
library(tidyterra)

sort(unique(NA_CEC_Eco_Level2$NA_L2NAME))


west.cord <-NA_CEC_Eco_Level2 %>% filter(NA_L1CODE == "6")

sample.ment <- NA_CEC_Eco_Level2 %>% filter(NA_L2NAME %in% c("ALASKA TUNDRA", 
                                                             "ALASKA TUNDRA",
                                                             "BOREAL PLAIN", 
                                                             "TEMPERATE PRAIRIES",
                                                             "WESTERN CORDILLERA" ))

  
#plot(west.cord)
sample_rast <- terra::rast(file.choose()) 
sample_rast.clim <-sample_rast
# CRs always shold follow the climate rasters, although the birds Ref do not have a CRS for some reason 
bird.rast.name <-terra::varnames(sample_rast)

shapeC <- sf::st_union(BCR4.1_USACAN, BCR4.0_USACAN)

rast.crs<- terra::crs(sample_rast.clim) ## from data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/Normal_1991_2020_bFFP.tif

shapeC<-sf::st_transform(shapeC, crs = rast.crs)

system.time({
  shapeC <- sf::st_buffer(shapeC, 400000) #for our tree dispersal est (fat-tailed dist) P of movement 400km within a century==0.0009. So limit of possibly founding populations
  
})



A <- ggplot()+
  geom_spatraster(data = sample_rast)+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  geom_sf(data = west.cord, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  scale_fill_terrain_c()+
  ggtitle(paste("Western cordillera - CEC 6", bird.rast.name))

B <- ggplot()+
  geom_spatraster(data = sample_rast)+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  geom_sf(data = InclusionShapefile, aes(), color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(),  alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  scale_fill_terrain_c()+
  ggtitle(paste("inclusion shapefile", bird.rast.name))


#cowplot::plot_grid(A, B, nrow =2)


C <- ggplot()+
  geom_spatraster(data = sample_rast)+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+ 
  scale_fill_terrain_c()+
  ggtitle(paste("BCR 4.1 & 4.0", bird.rast.name))

D <- ggplot()+
  geom_spatraster(data = sample_rast)+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  geom_sf(data = sample.ment, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+ 
  scale_fill_terrain_c()+
  ggtitle(paste("samples mentioned methods", bird.rast.name))

shapes.comp <- cowplot::plot_grid(C, A, B,  D, nrow =2)

#ggsave(shapes.comp, filename = "shapeswbuffv2.comp.png", path = "plots/", units = "in", width = 10, height = 10, dpi = 300, bg = "white")

#produce NA mask following somwhat Anna's method
sample <- terra::rast(file.choose())
plot(sample)

files.rasts <- list.files("data/corrected_rasters_clim_topo/", pattern = "gri", full.names = TRUE)
idx <- grep(".grd.aux.xml", files.rasts)
files.rasts<-files.rasts[-idx]

list.rasts <-list()
for (i in files.rasts) {
  rast1<- terra::rast(i)
  list.rasts[[i]]<-rast1
}

NormStack<- terra::rast(list.rasts)
Climate<-as.data.frame(NormStack, xy=TRUE)
Climate<-subset(Climate, !is.na(Climate$EMT)) # get rid of ocean data, from Anna's code

saveRDS(Climate, "data/corrected_rasters_clim_topo/all_rasts_Correct_Ecozones.RDS") #rasters also as an RDS

#still figuring out sampling region based on several elements:

E <- ggplot()+
  geom_spatraster(data = NormStack[[1]])+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  #geom_sf(data = west.cord, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  scale_fill_terrain_c()+
  ggtitle(paste("Clim var crop for data points"))+
  geom_spatvector(data = Clim_xy, aes(), size = 0.2)

Fg <- ggplot()+
  geom_spatraster(data = NormStack2[[1]])+
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+
  #geom_sf(data = InclusionShapefile, aes(), color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(),  alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  scale_fill_terrain_c()+
  ggtitle(paste("FVO buffered crop of BCR4.1 and BCR4.0"))+
  geom_spatvector(data = Clim_xy, aes(), size = 0.2)


shapes.comp.points <- cowplot::plot_grid(E, Fg, C, D, nrow =2)
#ggsave(shapes.comp.points, filename = "shapeswbuffNpoints.comp.png", path = "plots/", units = "in", width = 15, height = 10, dpi = 300, bg = "white")




# produce a raster to clean Ref, suit, and refxsuit rasters
Norm1991<-readRDS("data/corrected_rasters_clim_topo/all_rasts_Correct_Ecozones.RDS")
NAKey<-subset(Norm1991,!is.na(Norm1991$bFFP))
NAKey<-subset(NAKey,!is.na(NAKey$tri))
NA_rast<-terra::rast(NAKey , type="xyz")
NA_rast[!is.na(NA_rast)] <-1
rm("Norm1991")

### produce a key to clean present distribution rasters, will work on RDS files, before rasterizing it to plot, 
###### but not needed for me
NAKey<-paste(NAKey$x,NAKey$y,sep=".") #raster squares with complete covariates only
# Import key of masked AK regions (AK portion of BCR 3 + BCR 2) -------------------------------------
AK_remove<-read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/AK_removalregion.csv")
AK_Key<-paste(AK_remove$x,AK_remove$y,sep=".")


