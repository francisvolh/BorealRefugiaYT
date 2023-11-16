library(dplyr)
library(ggplot2)
library(tidyterra)
library(terra)

list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/BorealTaigaCordillera.w.AK/")

#solving conflict of regions

#https://www.epa.gov/eco-research/ecoregions-north-america
#BCR 4.0 and BCR 4.1 are actually from this link, Ecoregions 6.1 and 3.2 that may match NABC BCR



#load maps
boreal_cord_AK <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/BorealTaigaCordillera.w.AK/BorealTaigaCordillera.w.AK.shp")
BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
NA_CEC_Eco_Level2 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_CEC_Eco_Level2/NA_CEC_Eco_Level2.shp") #### HUGE SHAPEFILE
USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")
#USA_reprpj <- sf::st_transform(USA, crs = rast.crs) #may need to reproject or not depending on ggplot

sort(unique(NA_CEC_Eco_Level2$NA_L2NAME))


boreal.cord<-NA_CEC_Eco_Level2 %>% filter(NA_L2NAME == "BOREAL CORDILLERA")

taiga.cord<-NA_CEC_Eco_Level2 %>% filter(NA_L2NAME == "TAIGA CORDILLERA")

cec_bt_cords<-sf::st_union(boreal.cord, taiga.cord)


west.cord <-NA_CEC_Eco_Level2 %>% filter(NA_L1CODE == "6")

sample.ment <- NA_CEC_Eco_Level2 %>% filter(NA_L2NAME %in% c("ALASKA TUNDRA", 
                                                             "ALASKA TUNDRA",
                                                             "BOREAL PLAIN", 
                                                             "TEMPERATE PRAIRIES",
                                                             "WESTERN CORDILLERA" ))
unique(sample.ment$NA_L2CODE)



#############################################################################################
##checking which shape files I have in the Drive

#1
borealtaigacordillera <-  sf::st_read(file.choose())
plot(borealtaigacordillera)
#2
alaskamask <- st_read(file.choose())
plot(alaskamask) ########################looks horrible, some proj issue???

#3
BCR4.0_USACAN  <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.0_USACAN.shp")
plot(BCR4.0_USACAN)

#4
BorealCordilleraCAN <- st_read(file.choose())
plot(BorealCordilleraCAN)

#5
InclusionShapefile <- sf::st_read(file.choose())
plot(InclusionShapefile)

#6
TaigaCordilleraCAN <- st_read(file.choose())
plot(TaigaCordilleraCAN)
#7
NA_CEC_Eco_Level2 <- sf::st_read(file.choose()) #### HUGE SHAPEFILE
plot(NA_CEC_Eco_Level2)

#8
BCR4.1_USACAN<-sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/cordillera breakdown/BCR4.1_USACAN.shp")
plot(BCR4.1_USACAN)


shapes_names <- c("borealtaigacordillera", 
                  "alaskamask",
                  "BCR4.0_USACAN", 
                  "BCR4.1_USACAN",
                  "BorealCordilleraCAN",
                  "InclusionShapefile",
                  "TaigaCordilleraCAN"
)
shapes_files <- list(borealtaigacordillera, 
                     alaskamask,
                     BCR4.0_USACAN, 
                     BCR4.1_USACAN,
                     BorealCordilleraCAN,
                     InclusionShapefile,
                     TaigaCordilleraCAN
)

sample_rast <- terra::rast(file.choose())


names(sample_rast)

plot_list <- list()

for (i in 1:length(shapes_names)) {
  
  
  p <- ggplot()+
    geom_spatraster(data = sample_rast, #aes(fill = "bigfile[, varname]")
    )+
    geom_sf(data = shapes_files[[i]], aes(),alpha = 0)+
    scale_fill_hypso_c()+
    ggtitle(shapes_names[i])+
    theme_bw()+
    theme(legend.position = "none")
  
  plot_list[[i]] <- p
  
}

cowplot::plot_grid(plotlist = plot_list, nrow = 2)

ggplot()+
  geom_spatraster(data =sample_rast #aes(fill = "bigfile[, varname]")
  )+
  geom_sf(data = BCR4.1_USACAN, aes())+
  #geom_sf(data = InclusionShapefile, aes())+
  geom_sf(data = BCR4.0_USACAN, aes())+
  
  scale_fill_hypso_c()+
  theme_bw()

plot(sample_rast)

#USA.Canada <- st_union(USA, canada)


USA <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_0.shp")
USA_reprpj <- sf::st_transform(USA, crs = rast.crs)
#plot(st_geometry(USA_reprpj), axes = TRUE)


#cropping is different than Anna because she used LAEA with a different origin

#USA.crop <- sf::st_crop(USA_reprpj, c(xmin = -3169846, ymin = 1116692, xmax = -746769, ymax = 3790154)) #from script G line 93 raw script Anna
USA.crop <- sf::st_crop(USA_reprpj, terra::ext(Clim_xy))

canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_0.shp")
canada_reprpj <- sf::st_transform(canada, crs= rast.crs)

#c(-3169846, -746769),ylim = c(1116692, 3790154)
#canada.crop <- sf::st_crop(canada_reprpj, c(xmin = -3169846, ymin = 1116692, xmax = -746769, ymax = 3790154)) #from script G line 93 raw script Anna
canada.crop <- sf::st_crop(canada_reprpj, terra::ext(Clim_xy))

usa_can_crop <- sf::st_union(canada.crop, USA.crop) #reprojection to the same ti (no anymore  to "EPSG:3573")




ggplot()+
  #geom_sf(data = BCR4.0_USACAN, aes())+
  #geom_sf(data = BCR4.1_USACAN, aes())+
  geom_sf(data = USA_reprpj, aes())+
  geom_sf(data = canada_reprpj, aes())

ggplot()+
  geom_sf(data = usa_can_crop, aes(), fill = "green", alpha = 0.3)+
  geom_sf(data = InclusionShapefile, aes())

ggplot()+
  geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0.3)+
  geom_sf(data = BCR4.1_USACAN, aes(), fill = "blue", alpha = 0.3)+
  geom_sf(data = BCR4.0_USACAN, aes(), fill = "red", alpha = 0.3)+
  theme_bw()



############################################################################################################################################

  
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



originalCEC <- ggplot()+
  #geom_spatraster(data = sample_rast)+
  #geom_sf(data = boreal.cord, aes(), color = "red", alpha = 0)+
  geom_sf(data = cec_bt_cords, aes(),color ="blue", alpha = 0, fill = "red")+
  #geom_sf(data = USA, aes(), alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  #scale_fill_terrain_c()+
  ggtitle(paste("CEC Boreal and Taiga Cord"))

Annasshapes <- ggplot()+
  #geom_spatraster(data = sample_rast)+
  geom_sf(data = boreal_cord_AK, aes(), color = "red", alpha = 0)+
  #geom_sf(data = BCR4.0_USACAN, aes(), color ="blue", alpha = 0, fill = "red")+
  #geom_sf(data = USA, aes(),  alpha = 0, fill = "yellow")+
  #scale_fill_viridis_c(option = "turbo", direction = -1)+
  #scale_fill_terrain_c()+
  ggtitle(paste("Anna's shapes"))

cowplot::plot_grid(originalCEC, Annasshapes)

ggplot()+
  geom_sf(data = cec_bt_cords, aes(color ="blue"), alpha = 0)+
  geom_sf(data = boreal_cord_AK, aes(color = "red") , alpha = 0)+
  scale_color_manual(name="maps", values = c( "blue", "red"),
                     labels = c("cec_bt_cords" , "boreal_cord_AK" ))

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
bcr4 <- bcr %>% 
  filter(BCR %in% c("4"))
bcr.intl <- sf::st_read(file.choose()) # international file
bcr.intl4 <- bcr.intl %>% 
  filter(BCR %in% c("4"))
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


#### CEC and Canada crops 
unique(NA_CEC_Eco_Level2$NA_L2NAME)


NA_CEC_Eco_Level2 %>% 
  filter(NA_L2NAME == "SOFTWOOD SHIELD") %>% 
  ggplot()+
  geom_sf(aes() )+
  geom_sf(dat = canada,  aes(), alpha = 0.2 )

object.size(canada)

canada.details <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_1.shp")

unique(canada.details$NAME_1)

ggplot(canada.details)+
  geom_sf(aes() )

NA_CEC_Eco_Level2 %>% 
  filter(NA_L2NAME == "SOFTWOOD SHIELD") %>% 
  ggplot()+
  geom_sf(data = canada.details, aes() , alpha = 0.2)

NA_CEC_Eco_Level2 %>% 
  #filter(NA_L2NAME == "SOFTWOOD SHIELD") %>% 
  ggplot()+
  geom_sf( aes(color = NA_L2NAME))+
  theme(legend.position = "none")+
  geom_sf(data = canada.details, aes() , alpha = 0.2)

cec_can_reproj <- sf::st_transform(canada, crs = sf::st_crs(NA_CEC_Eco_Level2))
cec_can_crop <- sf::st_crop(NA_CEC_Eco_Level2, cec_can_reproj)

cec_can_crop2 <- 
  NA_CEC_Eco_Level2 %>%
  sf::st_intersection(cec_can_reproj)


cec_can_crop %>% 
  #filter(NA_L2NAME == "SOFTWOOD SHIELD") %>% 
  ggplot()+
  #geom_sf( aes(color = NA_L2NAME))+
  theme(legend.position = "none")

ggplot(data = cec_can_crop2)+
  geom_sf( aes(fill = NA_L2NAME) , alpha = 0.2)+
  geom_sf(data =canada , aes(), alpha = 0.2)

###################################################
###plotting with canadian framework regions
###################################################

library(dplyr)
library(sf)
library(ggplot2)

path1 <-"https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/data_donnees/geoJSON/er/nef_ca_ter_ecoregion_v2_2.geojson"
path2 <-"https://agriculture.canada.ca/atlas/data_donnees/nationalEcologicalFramework/data_donnees/geoJSON/ez/nef_ca_ter_ecozone_v2_2.geojson"

canada.frame.er <- st_read(path1, drivers = "GeoJSON")
canada.frame.ez <- st_read(path2, drivers = "GeoJSON")

canada.frame.er<-sf::st_transform(canada.frame.er, crs = sf::st_crs(shapeC))
canada.frame.ez<-sf::st_transform(canada.frame.ez, crs = sf::st_crs(shapeC))


A <-ggplot2::ggplot()+
  ggplot2::geom_sf(data =  canada.frame.ez,  alpha = 0)+
  ggplot2::geom_sf(data =  shapeC, alpha = 0, color = "red")+
  ggtitle("Canadas Ecozones and CEC Level 2 Ecoregions (red)")

B <- ggplot2::ggplot()+
  ggplot2::geom_sf(data =  canada.frame.er,  alpha = 0)+
  ggplot2::geom_sf(data =  shapeC, alpha = 0, color = "red")+
  ggtitle("Canadas Framework Ecoregions and CEC Level 2 Ecoregions (red)")

plot.comparison <- cowplot::plot_grid(A, B)


canada.frame.ez %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])



sf::st_coordinates(st_centroid(canada.frame.ez))

ecozone.df <- as.data.frame(canada.frame.ez)


with_names <- ggplot() +
  stat_sf_coordinates(data = canada.frame.ez)+
  ggplot2::geom_sf(data =  canada.frame.ez,  alpha = 0)+
  geom_sf_text(data = canada.frame.ez, aes(label = ECOZONE_NAME_EN ))+
  ggplot2::geom_sf(data =  shapeC, alpha = 0, color = "red")+
  ggtitle("CEC ecozones and Framework Ecozones")


boreal_cord_AK_withEZ <-  ggplot() +
  stat_sf_coordinates(data = canada.frame.ez)+
  ggplot2::geom_sf(data =  canada.frame.ez,  alpha = 0)+
  geom_sf_text(data = canada.frame.ez, aes(label = ECOZONE_NAME_EN ))+
  ggplot2::geom_sf(data =  boreal_cord_AK, alpha = 0, color = "red") 

CEC_withEZ <-  ggplot() +
  stat_sf_coordinates(data = canada.frame.ez)+
  ggplot2::geom_sf(data =  canada.frame.ez,  alpha = 0)+
  geom_sf_text(data = canada.frame.ez, aes(label = ECOZONE_NAME_EN ))+
  ggplot2::geom_sf(data =  boreal.cord, alpha = 0, color = "red") +
  ggplot2::geom_sf(data =  taiga.cord, alpha = 0, color = "red") 

BCR_withEZ <-  ggplot() +
  stat_sf_coordinates(data = canada.frame.ez)+
  ggplot2::geom_sf(data =  canada.frame.ez, alpha = 0)+
  geom_sf_text(data = canada.frame.ez, aes(label = ECOZONE_NAME_EN ))+
  ggplot2::geom_sf(data =  bcr.intl4, alpha = 0, color = "red") +
  ggtitle("NABCI BCR and Framework Ecozones")

plot.comparison2 <- cowplot::plot_grid(with_names, BCR_withEZ)
plot.comparison2



###################################################
###plotting current dist RDS and resulting rasters
###################################################

current.rds <- readRDS(file.choose())
future.rds <- readRDS(file.choose())
class(current.rds)
class(future.rds)
future.rds <- as.data.frame(future.rds)

rast1<-terra::rast(current.rds, crs = rast.crs)

rast2<-terra::rast(future.rds, crs = rast.crs)

#plot.sampling <-# 
  a <-ggplot()+
    geom_spatraster(data = rast1$Mean)+
    geom_sf(data = shapeC, aes(), alpha = 0)+
    scale_fill_continuous(low = "yellow" ,high = "darkcyan")+
    coord_sf(xlim=c(terra::ext(rast1)[1], terra::ext(rast1)[2]),
             ylim = c(terra::ext(rast1)[3], terra::ext(rast1)[4]),
             expand = FALSE)+
    ggtitle("present distribution")
  b<-ggplot()+
    geom_spatraster(data = rast2$Mean)+
    geom_sf(data = shapeC, aes(), alpha = 0)+
    scale_fill_continuous(low = "yellow" ,high = "darkcyan")+
    coord_sf(xlim=c(terra::ext(rast1)[1], terra::ext(rast1)[2]),
             ylim = c(terra::ext(rast1)[3], terra::ext(rast1)[4]),
             expand = FALSE)+
    ggtitle("future distribution")
  
pres.vs.fut.dists.plots <-cowplot::plot_grid(a, b)

#ggplot2::ggsave(pres.vs.fut.dists.plots, filename = "pres.vs.fut.dists.plots.png", path = "plots/", units = "in", width = 20, height = 10, dpi = 300, bg = "white")

dis.png <- image_read(file.choose())

c<-cowplot::ggdraw()+
  cowplot::draw_image(dis.png)+
  ggtitle("plot of present dist")
  

distribution_plots <- cowplot::plot_grid(a, b, c, ncol = 3)
#ggplot2::ggsave(distribution_plots, filename = "distribution_plots.png", path = "plots/", units = "in", width = 20, height = 10, dpi = 300, bg = "white")

