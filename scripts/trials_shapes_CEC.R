library(dplyr)
library(ggplot2)
library(tidyterra)

list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_CEC_Eco_Level2/")

#solving conflict of regions

#https://www.epa.gov/eco-research/ecoregions-north-america
#BCR 4.0 and BCR 4.1 are actually from this link, Ecoregions 6.1 and 3.2 that may match NABC BCR



#load maps
BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
NA_CEC_Eco_Level2 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_CEC_Eco_Level2/NA_CEC_Eco_Level2.shp") #### HUGE SHAPEFILE
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")
#USA_reprpj <- sf::st_transform(USA, crs = rast.crs) #may need to reproject or not depending on ggplot

sort(unique(NA_CEC_Eco_Level2$NA_L2NAME))


boreal.cord<-NA_CEC_Eco_Level2 %>% filter(NA_L2NAME == "BOREAL CORDILLERA")

taiga.cord<-NA_CEC_Eco_Level2 %>% filter(NA_L2NAME == "TAIGA CORDILLERA")

west.cord <-NA_CEC_Eco_Level2 %>% filter(NA_L1CODE == "6")

sample.ment <- NA_CEC_Eco_Level2 %>% filter(NA_L2NAME %in% c("ALASKA TUNDRA", 
                                                             "ALASKA TUNDRA",
                                                             "BOREAL PLAIN", 
                                                             "TEMPERATE PRAIRIES",
                                                             "WESTERN CORDILLERA" ))
unique(sample.ment$NA_L2CODE)
  
#plot(west.cord)
sample_rast <- terra::rast(file.choose()) # bird REFUGIA sample raster
sample_rast.clim <- terra::rast(file.choose()) # CLIMATE sample raster
# CRSalways should follow the climate rasters, although the birds Ref do not have a CRS for some reason 
bird.rast.name <-terra::varnames(sample_rast)

cec_crop <- sf::st_crop(NA_CEC_Eco_Level2, terra::ext(Clim_xy))
name.vect <-unique(cec_crop$NA_L2NAME)

cec_data_lim <- NA_CEC_Eco_Level2 %>% 
  filter(NA_L2NAME %in% name.vect)

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

Normstack2 <- terra::rast("data/corrected_rasters_clim_topo/all_corrected_Norm1991.grd")


#plot observation points over some areas for visual
#need to load Clim_xy object for observation points
E <- ggplot()+
  geom_spatraster(data = Normstack2[[1]])+ # one climatic raster
  geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+ # buffer for BCR4.1 and 4.0
  #geom_sf(data = west.cord, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+ #basemap, USA for now
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


###trials for the region
plot.inclusion<-ggplot()+
  #geom_sf(data = BCR4.0_USACAN, aes(color = as.factor(subUnit)), alpha = 0, fill = "red")+
  #geom_sf(data = BCR4.1_USACAN, aes(color = as.factor(subUnit)), alpha = 0, fill = "red")+
  geom_sf(data = InclusionShapefile, aes(color = as.factor(subUnit)), alpha = 0, fill = "red")+
  #geom_spatraster(data = Normstack2[[1]])+ # one climatic raster
  #geom_sf(data = shapeC, aes(), color = "red", alpha = 0)+ # buffer for BCR4.1 and 4.0
  #geom_sf(data = west.cord, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = USA, aes(#color = NA_L2NAME
  ), alpha = 0.1, fill = "yellow")+
  geom_sf(data = canada, aes(#color = NA_L2NAME
    ), alpha = 0.1, fill = "yellow")+ #basemap, USA for now
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  #scale_fill_terrain_c()+
  ggtitle(paste("Inclusion shape with data"))+
  geom_spatvector(data = Clim_xy, aes(), alpha = 0.05, color= "blue", 
                  size = 0.2) + #bird observations
  coord_sf(xlim=c(terra::ext(InclusionShapefile)[1], terra::ext(InclusionShapefile)[2]),
           ylim = c(terra::ext(InclusionShapefile)[3], terra::ext(InclusionShapefile)[4]),
           expand = FALSE)+
  #guides(color=guide_legend(title="CEC Ecoregions Level 2"))+
  #scale_colour_discrete(labels = c("BCR4.0", "BCR4.1"))+
  theme_bw()
ggsave(plot.pointsBase, filename = "plot.pointsBase3.png", path = "plots/", units = "in", width = 10, height = 10, dpi = 300, bg = "white")

plots.check <-  cowplot::plot_grid(plot.inclusion, plot.cec.crop, 
                                   rel_widths = c(1.75, 2), rel_heights =  c(1.75,2))
ggsave(plots.check, filename = "plots.check.Inclusion.CEC.png", path = "plots/", units = "in", width = 20, height = 10, dpi = 300, bg = "white")


sort(unique(cec_crop$NA_L2NAME))
cec_labels <- sort(unique(cec_crop$NA_L2NAME))

bcr <- sf::st_read(file.choose()) #BCR terrestrial master

bcr.intl <- sf::st_read(file.choose()) # international file

ggplot()+
  geom_sf(data = bcr, aes())

names(bcr)
unique(bcr$BCRNAME)
unique(bcr$COUNTRY) #not for international file
unique(bcr$BCR)

#for master shapefile 
bcr.Can_US <- bcr %>% 
  filter(COUNTRY %in% c("USA", "CANADA"))

ggplot()+
  geom_sf(data = bcr.Can_US, aes())

bcr.Can_US.proj<-sf::st_transform(bcr.Can_US, crs = rast.crs)
ggplot()+
  geom_sf(data = bcr.Can_US.proj, aes())

bcr.Can_US.proj.bcr4 <- bcr.Can_US.proj %>% 
  filter(BCR %in% c("4"))

ggplot()+
  geom_sf(data = bcr.Can_US.proj.bcr4, aes())

names(bcr.Can_US.proj.bcr4)

bcr.Can_US.proj.bcr4$BCRNAME

ggplot()+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="black", alpha = 0, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="grey", alpha = 0, fill = "red")+
  geom_sf(data = bcr.Can_US.proj.bcr4, 
          aes(color = as.factor(bcr.Can_US.proj.bcr4$PROVINCE_S), 
              alpha = 0.2, fill = "red"),  alpha = 0.2)+
  theme_bw()
#fill and colored lines are NABC BCR 4, and black outline are the BCR 4.0 and 4.1 Anna uses, 
# the other 
  
bcr.Can_US.proj.bcr4$PROVINCE_S

bcr.bcr4_6 <- bcr.Can_US.proj %>% 
  filter(BCR %in% c("4", "6"))

ggplot()+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="black", alpha = 0.2, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="grey", alpha = 0.2, fill = "red")+
  geom_sf(data = bcr.bcr4_6, 
          aes(color = as.factor(bcr.bcr4_6$BCR)),  alpha = 0)+
  theme_bw()
ggplot()+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="black", alpha = 0.2, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="grey", alpha = 0.2, fill = "red")+
  geom_sf(data = taiga.cord, 
          aes(),  alpha = 0.4)+
  theme_bw()
  
#master international

bcr.bcr4proj<-sf::st_transform(bcr.bcr4, crs = rast.crs)

ggplot()+
  geom_sf(data = bcr.bcr4proj, aes(), color = "yellow")+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="blue", alpha = 0.2, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="blue", alpha = 0.2, fill = "red")+
  theme_bw()

ggplot()+
  geom_sf(data = InclusionShapefile, aes(), color = "yellow")+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="blue", alpha = 0.2, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="blue", alpha = 0.2, fill = "red")+
  theme_bw()

#plot comparison of Anna's BCR 4.0 and 4.1 vs real BCR4
ggplot()+
  geom_sf(data = dplyr::filter(InclusionShapefile, subUnit %in% c("40", "41", "42", "43")), aes(color = as.factor(subUnit)), alpha = 0, fill = "red"
          )+
geom_sf(data = bcr.Can_US.proj.bcr4, aes(), alpha = 0.1, fill = "red"
        )+
  geom_sf(data = USA, aes(#color = NA_L2NAME
  ), alpha = 0.1, fill = "yellow"
  )+
  geom_sf(data = canada, aes(#color = NA_L2NAME
  ), alpha = 0.1, fill = "yellow"
  )+ 
  coord_sf(xlim=c(terra::ext(InclusionShapefile)[1], terra::ext(InclusionShapefile)[2]),
           ylim = c(terra::ext(InclusionShapefile)[3], terra::ext(InclusionShapefile)[4]),
           expand = FALSE)

ggplot()+
  tidyterra::geom_spatraster(data = ref.sum.75, aes())+
  geom_sf(data = BCR4.0_USACAN, aes(),color ="blue", alpha = 0, fill = "red")+
  geom_sf(data = BCR4.1_USACAN, aes(),color ="blue", alpha = 0, fill = "red")+
  ggplot2::scale_fill_viridis_c(option = "turbo", direction = -1, na.value="white")+
  theme_bw()

