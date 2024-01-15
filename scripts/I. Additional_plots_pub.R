library(dplyr)
library(sf)
library(ggplot2)

###############################################
### 1 i) Mapping of points and Ecoregions ---  of 1 spp 
###############################################

### run the required preliminary code for script H 


BOCH_full <- read.csv(("data/YT Boreal Refugia Drive/YK Refugia Code and material/subsamples for plots/BOCH_full.csv"))
BOCH_test <-  read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/subsamples for plots/BOCH_testSample.csv")
BOCH_train <-  read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/subsamples for plots/BOCH_trainingSample.csv")

xy_frame <- unique(BOCH_full[,c(2,10,11)])# 63681 PC Locations, avoiding "re sightings"


xy <- xy_frame[,c(2:3)]
xy<-dplyr::filter(xy, !is.na(lat))
xy_sf <- sf::st_as_sf(xy, coords = c("lon", "lat")) 
sf::st_crs(xy_sf) <- "EPSG:4326"
xy_sf_base <- xy_sf
xy_sf <- sf::st_transform(xy_sf, rast.crs )

xy_frame2 <- BOCH_full|>
  mutate(
    P_A = case_when( count > 0 ~ "presence", .default = "absence")
  )
xy2 <- xy_frame2[,c("lon","lat", "P_A")]
xy2 <- unique(xy2[,c("lon","lat", "P_A")])
xy2<-dplyr::filter(xy2, !is.na(lat))
xy_sf2 <- sf::st_as_sf(xy2, coords = c("lon", "lat")) 
sf::st_crs(xy_sf2) <- "EPSG:4326"
xy_sf2 <- sf::st_transform(xy_sf2, rast.crs )

xy_frame_test <- unique(BOCH_test[,c(2,10,11)]) # 63681 PC Locations, avoiding "re sightings"
xy_test <- xy_frame_test[,c(2:3)]
xy_test<-dplyr::filter(xy_test, !is.na(lat))
xy_sf_test <- sf::st_as_sf(xy_test, coords = c("lon", "lat")) 
sf::st_crs(xy_sf_test) <- "EPSG:4326"
xy_sf_test<- sf::st_transform(xy_sf_test, rast.crs )

xy_frame_train <- unique(BOCH_train[,c(2,10,11)]) # 63681 PC Locations, avoiding "re sightings"
xy_train <- xy_frame_train[,c(2:3)]
xy_train<-dplyr::filter(xy_train, !is.na(lat))
xy_sf_train <- sf::st_as_sf(xy_train, coords = c("lon", "lat")) 
sf::st_crs(xy_sf_train) <- "EPSG:4326"
xy_sf_train<- sf::st_transform(xy_sf_train, rast.crs )

xy_frame3 <- BOCH_train|>
  mutate(
    P_A = case_when( count > 0 ~ "presence", .default = "absence")
  )
xy3 <- xy_frame3[,c("lon","lat", "P_A")]
xy3 <- unique(xy3[,c("lon","lat", "P_A")])
xy3<-dplyr::filter(xy3, !is.na(lat))
xy_sf3 <- sf::st_as_sf(xy3, coords = c("lon", "lat")) 
sf::st_crs(xy_sf3) <- "EPSG:4326"
xy_sf3 <- sf::st_transform(xy_sf3, rast.crs )

canada_test<- sf::st_transform(canada, rast.crs)

canada_test <- sf::st_crop(canada_test, c(xmin=as.numeric(terra::ext(xy_sf)[1]), 
                                          xmax=as.numeric(terra::ext(xy_sf)[2]), 
                                          ymin=as.numeric(terra::ext(xy_sf)[3]), 
                                          ymax=as.numeric(terra::ext(xy_sf)[4])))

USA_test<- sf::st_transform(USA, rast.crs )

USA_test <- sf::st_crop(USA_test, c(xmin=as.numeric(terra::ext(xy_sf)[1]), 
                              xmax=as.numeric(terra::ext(xy_sf)[2]), 
                              ymin=as.numeric(terra::ext(xy_sf)[3]), 
                              ymax=as.numeric(terra::ext(xy_sf)[4])))

#provincial limits version
#all points, in black (could be split into the 3 subsets overplotted as well)
A <- ggplot() +
  #ggplot2::geom_sf(data = poly, fill = "grey") +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  #geom_sf(data = nwt.pas, fill = "pink" )+
  #geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
  #geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
  #ggplot2::geom_sf(data = usa_crop, alpha =0)+
  #ggplot2::geom_sf(data = canada_crop, alpha =0)+
  #ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  #ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf, size=0.0001, colour="black", alpha=0.05) +
  #ggplot2::geom_sf(data=xy_sf_train, size=0.0001,colour="blue")+
  #ggplot2::geom_sf(data=xy_sf_test, size=0.0001,colour="red")+
  ggplot2::coord_sf(expand = FALSE)


#all points presence-absence
B <- ggplot() +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], aes(color ="#F8766D"), size=0.1, alpha=0.05)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], aes(color="#619CFF"), size=0.1,  alpha=0.05)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('#619CFF'='#619CFF', "#F8766D"="#F8766D"), labels = c( 'presence', 'absence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(colour = guide_legend(override.aes = list(size=2.5)))

#training data, presence-absence
C <- ggplot() +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], aes(color ="#F8766D"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], aes(color="#619CFF"), size=0.1)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('#619CFF'='#619CFF', "#F8766D"="#F8766D"), labels = c( 'presence', 'absence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(colour = guide_legend(override.aes = list(size=2.5)))
#training data, presence-absence
D <- ggplot() +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], aes(color ="#F8766D"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], aes(color="#619CFF"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf_test, size=0.0001, aes(color="black"))+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('#619CFF'='#619CFF', '#F8766D'='#F8766D','black' = 'black'),
                               labels = c( 'presence', 'absence', 'testing data'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(color = guide_legend(override.aes = list(size=2.5)))

subsamples_plots <- cowplot::plot_grid(A, B, C, labels = c("A", "B", "C"), rel_widths = c(1.75, 2, 2),  nrow = 1 )
#ggsave(subsamples_plots, filename = "subsamples_plots.png", path = "plots/", units = "in", width = 30, height = 9, dpi = 300, bg = "white")

subsamples_plots2 <- cowplot::plot_grid(A, C, D, labels = c("A", "B", "C"), rel_widths = c(1.75, 2, 2),  nrow = 1 )
#ggsave(subsamples_plots2, filename = "subsamples_plotsOPTION2.png", path = "plots/", units = "in", width = 30, height = 9, dpi = 300, bg = "white")

#CEC ecoregions version
# filled
#all points, in black (could be split into the 3 subsets overplotted as well)
A <- ggplot() +
  geom_sf(data = cec_crop, aes(fill = NameL2_En), alpha=0.25)+
  #ggplot2::geom_sf(data = poly, fill = "grey") +
  #ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  #ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  #geom_sf(data = nwt.pas, fill = "pink" )+
  #geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
  #geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
  #ggplot2::geom_sf(data = usa_crop, alpha =0)+
  #ggplot2::geom_sf(data = canada_crop, alpha =0)+
  #ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  #ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf, size=0.0001, colour="black", alpha=0.05) +
  #ggplot2::geom_sf(data=xy_sf_train, size=0.0001,colour="blue")+
  #ggplot2::geom_sf(data=xy_sf_test, size=0.0001,colour="red")+
  ggplot2::coord_sf(expand = FALSE)


#training data, presence-absence
C <- ggplot() +
  geom_sf(data = cec_crop, aes(fill = NameL2_En), alpha=0.25)+
  #ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  #ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], aes(color ="#F8766D"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], aes(color="#619CFF"), size=0.1)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('#619CFF'='#619CFF', "#F8766D"="#F8766D"), labels = c( 'presence', 'absence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(fill = "none",
                  colour = guide_legend(override.aes = list(size=2.5)))

#training data, presence-absence
D <- ggplot() +
  geom_sf(data = cec_crop, aes(fill = NameL2_En), alpha=0.25)+
  #ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  #ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], aes(color ="#F8766D"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], aes(color="#619CFF"), size=0.1)+
  ggplot2::geom_sf(data=xy_sf_test, size=0.0001, aes(color="black"))+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('#619CFF'='#619CFF', '#F8766D'='#F8766D','black' = 'black'),
                               labels = c( 'presence', 'absence', 'testing data'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(fill = "none",
                  color = guide_legend(override.aes = list(size=2.5)))


subsamples_plots3 <- cowplot::plot_grid(A, C, D, labels = c("A", "B", "C"), rel_widths = c(2.1,1.75, 1.75),  nrow = 1 )

subsamples_plots3
#ggsave(subsamples_plots3, filename = "subsamples_plotsOPTION3.png", path = "plots/", units = "in", width = 30, height = 9, dpi = 300, bg = "white")

###############################################
### 1 ii) Mapping of points and Ecoregions --- of all spp
###############################################
#get location from QPAD bird abundance estimations
KEY<-readRDS("data/YT Boreal Refugia Drive/QPAD_Output/ALFL_Data.rds") #Should be 202712 records

Clim_xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations, avoiding "re sightings"

Clim_xy <- Clim_xy_frame[,c(2:3)] # only coordinates in lat lon

# produce a Spatial object
Clim_xy<-terra::vect(Clim_xy,  crs = "EPSG:4326") ## CRS is  for lat lon data coming from QPAD

# Match (reproject) points to the climate raster projection
clim.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Norm1991/"

rast.crs<- terra::crs(terra::rast(paste0(clim.folder,"Normal_1991_2020_bFFP.tif"))) ## get CRS object from climate rasters

Clim_xy <- terra::project(Clim_xy, rast.crs) #reproject from lat lon to climate rasters CRS


NA_CEC_Eco_Level2 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_Terrestrial_Ecoregions_v2_Level_II_Shapefile/NA_Terrestrial_Ecoregions_v2_level2.shp") #### new 2021 version HUGE SHAPEFILE
NA_CEC_Eco_Level2 <- sf::st_transform(NA_CEC_Eco_Level2, rast.crs)

cec_crop <- sf::st_crop(NA_CEC_Eco_Level2, terra::ext(Clim_xy))
#unique(cec_crop$NameL2_En)


Clim_xy_labs <- sf::st_as_sf(Clim_xy)

Clim_xy_labs<-sf::st_join(Clim_xy_labs,NA_CEC_Eco_Level2)

Clim_xy_labs <- Clim_xy_labs |>
  sf::st_drop_geometry()

unique(Clim_xy_labs$NameL2_En)

#list the all the ecoregions with points on them 
Clim_xy_labs|>
  group_by(NameL2_En)|>
  filter(row_number()==1)|>
  filter(!is.na(NameL2_En))

#list the all the ecoregions with points on them 
Clim_xy_labs%>%
  group_by(NameL2_En) %>% 
  filter(row_number()==1) %>% 
  filter(!is.na(NameL2_En)) %>% 
  filter(NameL2_En %in% c('Boreal Cordillera', 'Taiga Cordillera')) %>% 
  ungroup
 

selected.cec_crop1 <- cec_crop|>
  filter(NameL2_En == "Softwood Shield")
selected.cec_crop2 <- cec_crop|>
  filter(NameL2_En == "Northern Arctic")

name.vect <-unique(cec_crop$NameL2_En)


## CEC ecoregions (filled)
ggplot()+
  geom_sf(data = cec_crop, aes(fill = NameL2_En), alpha=0.5)+
  #geom_sf(data = selected.cec_crop1, fill = "red")+
  #geom_sf(data = selected.cec_crop2, fill = "blue")+
  tidyterra::geom_spatvector(data = Clim_xy, aes(), color = "black", size = 0.01)+
  ggplot2::coord_sf(expand = FALSE)+
  theme_bw()

## CEC ecoregions (oulined)
ggplot()+
  geom_sf(data = cec_crop, aes(color = NameL2_En), alpha=0.5)+
  #geom_sf(data = selected.cec_crop1, fill = "red")+
  #geom_sf(data = selected.cec_crop2, fill = "blue")+
  tidyterra::geom_spatvector(data = Clim_xy, aes(), color = "black", size = 0.01)+
  ggplot2::coord_sf(expand = FALSE)+
  theme_bw()

####################################################
#### 2) Mapping of Protected areas Alaska, Yukon, NWT
####################################################

#call dissolve_sf from pre- run of script H
dissolve_sf


ggplot2::ggplot()+
  ggplot2::geom_sf(data = poly, fill = "grey") +
  ggplot2::geom_sf(data = usa_crop, fill = "white")+
  ggplot2::geom_sf(data = canada_crop, fill = "white")+
  ggplot2::geom_sf(data = dissolve_sf, fill = "green" )+
  #ggplot2::geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
  #ggplot2:: geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
  ggplot2::geom_sf(data = usa_crop, alpha =0)+
  ggplot2::geom_sf(data = canada_crop, alpha =0)+
  ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                           terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                    ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                    expand = FALSE)+
  ggplot2::theme_bw()



#map a sample stack or the three RES, LDM, SDM for Ref x Suit



refxsuit_RES <- all.group.rasters[[1]][[4]]
refxsuit_LDM <- all.group.rasters[[2]][[4]]
refxsuit_SDM <- all.group.rasters[[3]][[4]]

refxsuit3rasts <- list(refxsuit_RES, refxsuit_LDM, refxsuit_SDM)

## VERSION 1
sample_plot_list <- NULL
for (n in 1:length(refxsuit3rasts)) {
  
  raster_stacked <- refxsuit3rasts[[n]]
  
  plot1<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data = raster_stacked)+
    ggplot2::geom_sf(data = dissolve_sf, alpha = 0 )+
    #ggplot2::geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
    #ggplot2:: geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                             terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                      ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                      expand = FALSE)+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style is turbo, viridis alone is for green colours?
    ggplot2::theme_bw()
  
  
  sample_plot_list[[n]]<-plot1
}
sample_plot_PAs_refxsuit <- cowplot::plot_grid(plotlist = sample_plot_list, ncol = 3)
ggplot2::ggsave(sample_plot_PAs_refxsuit, filename = "sample_plot_PAs_refxsuitver1.png", path = "plots/", units = "in", width = 22.5, height = 7, dpi = 300, bg = "white")


#VERSION 2
sample_plot_list2<- NULL
for (n in 1:length(refxsuit3rasts)) {
  
  raster_stacked <- refxsuit3rasts[[n]]
  raster_stacked <- terra::mask(raster_stacked, yukon_PAs_crs)
  
  plot1<-ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data = raster_stacked)+
    ggplot2::geom_sf(data = dissolve_sf, alpha = 0 )+
    #ggplot2::geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
    #ggplot2:: geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                             terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                      ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                      expand = FALSE)+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style is turbo, viridis alone is for green colours?
    ggplot2::theme_bw()
  plot1
  
  sample_plot_list2[[n]]<-plot1
}
sample_plot_PAs_refxsuit2 <- cowplot::plot_grid(plotlist = sample_plot_list2, ncol = 3)
ggplot2::ggsave(sample_plot_PAs_refxsuit2, filename = "sample_plot_PAs_refxsuitver2.png", path = "plots/", units = "in", width = 22.5, height = 7, dpi = 300, bg = "white")


#################
### 3) topo map -- attempts
#################
KEY<-readRDS("data/YT Boreal Refugia Drive/QPAD_Output/ALFL_Data.rds") #Should be 202712 records

Clim_xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations, avoiding "re sightings"

Clim_xy <- Clim_xy_frame[,c(2:3)] # only coordinates in lat lon

# produce a Spatial object
Clim_xy<-terra::vect(Clim_xy, crs = "EPSG:4326") ## CRS is  for lat lon data coming from QPAD

elevNA <- terra::rast(file.choose())
plot(elevNA)

elevNA_reproj <- terra::crop(elevNA, terra::ext(Clim_xy) )

elevNA_reproj <- terra::project(elevNA, rast.crs) ########## THIS IS TAKING WAAAAY TO LONG TO REPROJECT

ggplot2::ggplot()+
  #ggplot2::geom_sf(data = poly, fill = "grey") +
  #ggplot2::geom_sf(data = usa_crop, fill = "white")+
  #ggplot2::geom_sf(data = canada_crop, fill = "white")+
  #geom_sf(data = nwt.pas, fill = "pink" )+
  #geom_sf(data = yukon_PAs_crs, fill = "lightgreen" )+
  #geom_sf(data = alaska.pas_crs,fill = "lightblue" )+
  #ggplot2::geom_sf(data = usa_crop, alpha =0)+
  #ggplot2::geom_sf(data = canada_crop, alpha =0)+
  #ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  #ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
tidyterra::geom_spatraster(data = elevNA_reproj)+  
ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                           terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                    ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                    expand = FALSE)+
  ggplot2::theme_bw()
