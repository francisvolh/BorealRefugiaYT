library(dplyr)
library(sf)
library(ggplot2)

################################################################################
################################################################################
### 1 i) Mapping of subsample points and Provinces or Ecoregions ---  of 1 spp 
################################################################################
################################################################################

### run the required preliminary code for script H 


BOCH_full <- read.csv("data/YT Boreal Refugia Drive/BOCH_methods_example/BOCH_fullData.csv")
BOCH_fullSetAs <-  read.csv("data/YT Boreal Refugia Drive/BOCH_methods_example/BOCH_fullsetaside.csv")
BOCH_train <-  read.csv("data/YT Boreal Refugia Drive/BOCH_methods_example/BOCH_Training.csv")

xy_frame <- unique(BOCH_full[,c(2,4,5,6,7)])# 63681 PC Locations, avoiding "re sightings"


xy <- xy_frame[,c(3:4)]
xy<-dplyr::filter(xy, !is.na(lat))
xy_sf <- sf::st_as_sf(xy, coords = c("lon", "lat")) 
sf::st_crs(xy_sf) <- "EPSG:4326"
xy_sf_base <- xy_sf
xy_sf <- sf::st_transform(xy_sf, rast.crs ) ### FULL BOCH 

xy_frame2 <- BOCH_full|>
  dplyr::mutate(
    P_A = dplyr::case_when( count > 0 ~ "presence", .default = "absence")
  )

xy2 <- xy_frame2[,c("lon","lat", "P_A")]
xy2 <- unique(xy2[,c("lon","lat", "P_A")])
xy2<-dplyr::filter(xy2, !is.na(lat))
xy_sf2 <- sf::st_as_sf(xy2, coords = c("lon", "lat")) 
sf::st_crs(xy_sf2) <- "EPSG:4326"
xy_sf2 <- sf::st_transform(xy_sf2, rast.crs ) ### FULL PRESENCE ABSENCE CATEGORIZED

#xy_frame_test <- unique(BOCH_test[,c(2,10,11)]) # 63681 PC Locations, avoiding "re sightings"
#xy_test <- xy_frame_test[,c(2:3)]
#xy_test<-dplyr::filter(xy_test, !is.na(lat))
#xy_sf_test <- sf::st_as_sf(xy_test, coords = c("lon", "lat")) 
#sf::st_crs(xy_sf_test) <- "EPSG:4326"
#xy_sf_test<- sf::st_transform(xy_sf_test, rast.crs )

xy_frame_train <- unique(BOCH_train[,c(2,4,5,6,8)]) # 63681 PC Locations, avoiding "re sightings"
xy_train <- xy_frame_train[,c(3:4)]
xy_train<-dplyr::filter(xy_train, !is.na(lat))
xy_sf_train <- sf::st_as_sf(xy_train, coords = c("lon", "lat")) 
sf::st_crs(xy_sf_train) <- "EPSG:4326"
xy_sf_train<- sf::st_transform(xy_sf_train, rast.crs ) ### Training all dataset BOCH

xy_frame3 <- BOCH_train|>
  dplyr::mutate(
    P_A = dplyr::case_when( count > 0 ~ "presence", .default = "absence")
  )
xy3 <- xy_frame3[,c("lon","lat", "P_A")]
xy3 <- unique(xy3[,c("lon","lat", "P_A")])
xy3<-dplyr::filter(xy3, !is.na(lat))
xy_sf3 <- sf::st_as_sf(xy3, coords = c("lon", "lat")) 
sf::st_crs(xy_sf3) <- "EPSG:4326"
xy_sf3 <- sf::st_transform(xy_sf3, rast.crs ) ### TRAINING PRESENCE ABSENCE CATEGORIZED

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


KEY<-readRDS("data/YT Boreal Refugia Drive/QPAD_Output/ALFL_Data.rds") 

xy_frame <- unique(KEY[,c(1,6,7)]) # 63681 PC Locations, avoiding "re sightings"
xy <- xy_frame[,c(2:3)] # only coordinates in lat lon
xy_sf <-sf::st_as_sf(xy, coords = c(1,2))
sf::st_crs(xy_sf) <- "EPSG:4326"
xy_sf <- sf::st_transform(xy_sf, sf::st_crs(NA_CEC_Eco_Level2) )
CECgroup<-sf::st_join(xy_sf,NA_CEC_Eco_Level2)
names_cec <- dplyr::filter( CECgroup, NameL2_En != "NA") |> dplyr::distinct(NameL2_En) |>  dplyr::pull() |> sort()
NA_CEC_Eco_Level2_short <- dplyr::filter( NA_CEC_Eco_Level2, NameL2_En %in% names_cec)
NA_CEC_Eco_Level2_gray <- dplyr::filter( NA_CEC_Eco_Level2, NameL2_En %in% c("Softwood Shield"  ,  "Northern Arctic" ))
NA_CEC_Eco_Level2_gray<- sf::st_crop(NA_CEC_Eco_Level2_gray, c(xmin=as.numeric(terra::ext(xy_sf)[1]), 
                                                               xmax=as.numeric(terra::ext(xy_sf)[2]), 
                                                               ymin=as.numeric(terra::ext(xy_sf)[3]), 
                                                               ymax=as.numeric(terra::ext(xy_sf)[4])))
NA_CEC_Eco_Level2_short <-  sf::st_crop(NA_CEC_Eco_Level2_short, c(xmin=as.numeric(terra::ext(xy_sf)[1]), 
                                                                   xmax=as.numeric(terra::ext(xy_sf)[2]), 
                                                                   ymin=as.numeric(terra::ext(xy_sf)[3]), 
                                                                   ymax=as.numeric(terra::ext(xy_sf)[4])))


#turbo_pal <- c(viridis::viridis(n = 14, direction = -1))

########################
#CEC ecoregions version
########################

# filled

#all points, in black (could be split into the 3 subsets overplotted as well)
A <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_gray, fill =  "gray" , color = NA, alpha = 0.2)+
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_short, ggplot2::aes(fill = NameL2_En), color = NA)+
  ggplot2::scale_fill_manual( values =  c(  "Alaska Boreal Interior"= ggplot2::alpha("#FDE725FF", 0.2),
                                            "Alaska Tundra" = ggplot2::alpha("#CBE11EFF", 0.2),
                                            "Boreal Cordillera"= ggplot2::alpha("#97D83FFF", 1),
                                            "Boreal Plains"= ggplot2::alpha("#67CC5CFF", 0.2),
                                            "Brooks Range Tundra"= ggplot2::alpha("#40BC72FF", 0.2),
                                            "Cold Deserts" = ggplot2::alpha("#25AC82FF", 0.2),
                                            "Marine West Coast Forests"= ggplot2::alpha( "#1F998AFF", 0.2),
                                            #"Northern Arctic" = ggplot2::alpha("#39568CFF", 0.2),
                                            "Southern Arctic"= ggplot2::alpha("#24878EFF", 0.2),
                                            "Taiga Cordillera"= ggplot2::alpha("#2B748EFF", 1),
                                            "Taiga Shield"= ggplot2::alpha( "#34618DFF", 0.2),#"Softwood Shield"= ggplot2::alpha("#2A788EFF", 0.2), 
                                            "Taiga Plains"= ggplot2::alpha("#3D4D8AFF", 0.2),
                                            
                                            "Temperate Prairies"= ggplot2::alpha("#453581FF", 0.2),
                                            "West Central Semi-Arid Prairies"= ggplot2::alpha("#481D6FFF", 0.2),
                                            "Western Cordillera"= ggplot2::alpha("#440154FF", 0.2)))+
  ggplot2::geom_sf(data=BCR4.1_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  ggplot2::geom_sf(data=BCR4.0_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf, size=0.01, colour="black", alpha=0.05) +
  #ggplot2::geom_sf(data=xy_sf_train, size=0.0001,colour="blue")+
  #ggplot2::geom_sf(data=xy_sf_test, size=0.0001,colour="red")+
  ggplot2::coord_sf(expand = FALSE)+ 
  ggplot2::guides(fill = ggplot2::guide_legend(title = "CEC level 2 Ecoregions")) +
  ggplot2::ggtitle("Full dataset")+
  ggplot2::theme(
    legend.position = c(.15, .3),
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  )


#training data, presence-absence
C <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_gray, fill =  "gray" , color = NA, alpha = 0.2)+
  ggplot2::geom_sf(data = cec_crop, ggplot2::aes(fill = NameL2_En), color = NA, alpha=0.25)+
  #ggplot2::scale_fill_viridis_d(direction = -1)+ ######## PLAY WILL COLOUR FOR BETTER VIZ
  ggplot2::scale_fill_manual( values =  c(  "Alaska Boreal Interior"= ggplot2::alpha("#FDE725FF", 0.2),
                                            "Alaska Tundra" = ggplot2::alpha("#CBE11EFF", 0.2),
                                            "Boreal Cordillera"= ggplot2::alpha("#97D83FFF", 1),
                                            "Boreal Plains"= ggplot2::alpha("#67CC5CFF", 0.2),
                                            "Brooks Range Tundra"= ggplot2::alpha("#40BC72FF", 0.2),
                                            "Cold Deserts" = ggplot2::alpha("#25AC82FF", 0.2),
                                            "Marine West Coast Forests"= ggplot2::alpha( "#1F998AFF", 0.2),
                                            #"Northern Arctic" = ggplot2::alpha("#39568CFF", 0.2),
                                            "Southern Arctic"= ggplot2::alpha("#24878EFF", 0.2),
                                            "Taiga Cordillera"= ggplot2::alpha("#2B748EFF", 1),
                                            "Taiga Shield"= ggplot2::alpha( "#34618DFF", 0.2),#"Softwood Shield"= ggplot2::alpha("#2A788EFF", 0.2), 
                                            "Taiga Plains"= ggplot2::alpha("#3D4D8AFF", 0.2),
                                            
                                            "Temperate Prairies"= ggplot2::alpha("#453581FF", 0.2),
                                            "West Central Semi-Arid Prairies"= ggplot2::alpha("#481D6FFF", 0.2),
                                            "Western Cordillera"= ggplot2::alpha("#440154FF", 0.2)))+
  ggplot2::geom_sf(data=BCR4.1_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  ggplot2::geom_sf(data=BCR4.0_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  #ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  #ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], ggplot2::aes(color ="#F8766D"), size=0.1, alpha=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], ggplot2::aes(color="blue"), size=0.1)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('blue'='blue', "#F8766D"="#F8766D"), labels = c(  'absence', 'presence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(fill = "none",
                  colour = ggplot2::guide_legend(override.aes = list(size=2.5)))+
  ggplot2::ggtitle("Full dataset: presence/absence")+
  ggplot2::theme(
    legend.position = c(.15, .25),
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  )

#training data, presence-absence
D <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_gray, fill =  "gray" , color = NA, alpha = 0.2)+
  
  ggplot2::geom_sf(data = cec_crop, ggplot2::aes(fill = NameL2_En),  color = NA, alpha=0.25)+
  #ggplot2::scale_fill_viridis_d(direction = -1)+ ######## PLAY WILL COLOUR FOR BETTER VIZ
  ggplot2::scale_fill_manual( values =  c(  "Alaska Boreal Interior"= ggplot2::alpha("#FDE725FF", 0.2),
                                            "Alaska Tundra" = ggplot2::alpha("#CBE11EFF", 0.2),
                                            "Boreal Cordillera"= ggplot2::alpha("#97D83FFF", 1),
                                            "Boreal Plains"= ggplot2::alpha("#67CC5CFF", 0.2),
                                            "Brooks Range Tundra"= ggplot2::alpha("#40BC72FF", 0.2),
                                            "Cold Deserts" = ggplot2::alpha("#25AC82FF", 0.2),
                                            "Marine West Coast Forests"= ggplot2::alpha( "#1F998AFF", 0.2),
                                            #"Northern Arctic" = ggplot2::alpha("#39568CFF", 0.2),
                                            "Southern Arctic"= ggplot2::alpha("#24878EFF", 0.2),
                                            "Taiga Cordillera"= ggplot2::alpha("#2B748EFF", 1),
                                            "Taiga Shield"= ggplot2::alpha( "#34618DFF", 0.2),#"Softwood Shield"= ggplot2::alpha("#2A788EFF", 0.2), 
                                            "Taiga Plains"= ggplot2::alpha("#3D4D8AFF", 0.2),
                                            "Temperate Prairies"= ggplot2::alpha("#453581FF", 0.2),
                                            "West Central Semi-Arid Prairies"= ggplot2::alpha("#481D6FFF", 0.2),
                                            "Western Cordillera"= ggplot2::alpha("#440154FF", 0.2)))+
  ggplot2::geom_sf(data=BCR4.1_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  ggplot2::geom_sf(data=BCR4.0_USACAN, ggplot2::aes(fill = NameL2_En), color = NA)+
  #ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  #ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf3[which(xy_sf3$P_A == "absence"),], ggplot2::aes(color ="#F8766D"), size=0.1, alpha=0.1
                   )+
  ggplot2::geom_sf(data=xy_sf3[which(xy_sf3$P_A == "presence"),], ggplot2::aes(color="blue"), size=0.1)+
  #ggplot2::geom_sf(data=xy_sf_test, size=0.0001, aes(color="black"))+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('blue'='blue', '#F8766D'='#F8766D'#,'black' = 'black'
                               ),
                               labels = c( 'absence', 'presence'#, 'testing data'
                               ))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(fill = "none",
                  color = ggplot2::guide_legend(override.aes = list(size=2.5)))+
  ggplot2::ggtitle("Training dataset: presence/absence")+
  ggplot2::theme(
    legend.position = c(.15, .25),
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  )


subsamples_plots3 <- cowplot::plot_grid(A, C, 
                                        D, labels = c("A", "B","C"
                                                      ), #rel_widths = c(2,1.75, 1.75), 
                                        nrow = 1 )

#subsamples_plots3
#ggplot2::ggsave(subsamples_plots3, filename = "subsamples_plotsOPTION2v4.png", path = "plots/", units = "in", width = 25, height = 8, dpi = 300, bg = "white")


################################################
# Provincial boundary   version
################################################

#all points, in black (could be split into the 3 subsets overplotted as well)
A <- ggplot2::ggplot() +
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
  ggplot2::geom_sf(data=xy_sf, size=0.01, colour="black", alpha=0.05) +
  #ggplot2::geom_sf(data=xy_sf_train, size=0.0001,colour="blue")+
  #ggplot2::geom_sf(data=xy_sf_test, size=0.0001,colour="red")+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::ggtitle("Full dataset")


#all points presence-absence
B <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], ggplot2::aes(color ="#F8766D"), size=0.1, alpha=0.1)+
  ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], ggplot2::aes(color="blue"), size=0.1)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('blue'='blue', "#F8766D"="#F8766D"), labels = c( 'absence', 'presence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2.5)))+
  ggplot2::ggtitle("Full dataset: presence/absence")

#training data, presence-absence
C <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = USA_test, color = "grey", fill = "white")+
  ggplot2::geom_sf(data = canada_test, color = "grey", fill = "white")+
  ggplot2::theme_bw()+
  ggplot2::geom_sf(data=xy_sf3[which(xy_sf3$P_A == "absence"),], ggplot2::aes(color ="#F8766D"), size=0.1,  alpha = 0.1)+
  ggplot2::geom_sf(data=xy_sf3[which(xy_sf3$P_A == "presence"),], ggplot2::aes(color="blue"), size=0.1)+
  ggplot2::scale_colour_manual(name = 'Observations', 
                               values =c('blue'='blue', "#F8766D"="#F8766D"), labels = c( 'absence','presence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=2.5)))+
  ggplot2::ggtitle("Training dataset: presence/absence")





subsamples_plots <- cowplot::plot_grid(A, B, C, labels = c("A", #"B", 
                                                           "C"), 
                                       rel_widths = c(1.75, 2, 2),  
                                       nrow = 1 )

#ggplot2::ggsave(subsamples_plots, filename = "subsamples_plotsv2A.png", path = "plots/", units = "in", width = 20, height = 9, dpi = 300, bg = "white")

#subsamples_plots2 <- cowplot::plot_grid(A, C, D, labels = c("A", "B", "C"), rel_widths = c(1.75, 2, 2),  nrow = 1 )
#ggsave(subsamples_plots2, filename = "subsamples_plotsOPTION3.png", path = "plots/", units = "in", width = 30, height = 9, dpi = 300, bg = "white")


##################################################################
############ 2)  Bin  elevations and # of pixels with high quality in those bins
############  <250m, 250-500, 500-750...

#terra::plot(elevation)

elevbcr <- terra::mask(elevation, terra::vect(bcr))
#terra::plot(elevbcr)
#produce 6 bins
maskEle01<- terra::ifel(elevbcr <250, 1, NA)
maskEle01 <-terra::mask(maskEle01, terra::vect(bcr))

maskEle02<- terra::ifel(elevbcr >= 250, elevbcr, NA)
maskEle02<- terra::ifel(maskEle02 < 500, 1, NA)

maskEle03<- terra::ifel(elevbcr >= 500, elevbcr, NA)
maskEle03<- terra::ifel(maskEle03 < 750, 1, NA)

maskEle04<- terra::ifel(elevbcr >= 750, elevbcr, NA)
maskEle04<- terra::ifel(maskEle04 < 1000, 1, NA)

maskEle05<- terra::ifel(elevbcr >= 1000, elevbcr, NA)
maskEle05<- terra::ifel(maskEle05 < 1250, 1, NA)

maskEle06<- terra::ifel(elevbcr >= 1250, 1, NA)

masks_vect<-list(maskEle01, maskEle02, maskEle03, maskEle04, maskEle05, maskEle06)
masks_labs<-c("<250", "250-500", "500-750", "750-1000", "1000-1250", ">1250")




par(mfrow=c(1,1))
terra::plot(terra::mask(elevation, terra::vect(bcr)))

hist(terra::values(terra::mask(elevation, terra::vect(bcr))))

{
  
  #all.group.rasters  # list of rasters, one per migra group (three cats), with 4 stacked raster each
  rast.cat.names<- c("Current Suitable Habitat","Refugia Probability","Future Suitable Habitat"," Future Suitable Refugia")
  
  df_pixels<-NULL
  for (i in 1:length(all.group.rasters)) {
    
    oneset <- all.group.rasters[[i]]
    
    for (k in 1:length(oneset)) {
      
      onerast<- oneset[[k]]
      
      if (k ==1 ) {
        
        q75 <- quantile(terra::values(onerast), probs=0.5, na.rm=TRUE) #### for 50% only
      }
      
      hiqual <- terra::ifel(onerast >= q75, 1, NA) 
      
      for (m in 1:length(masks_vect)) {
        
        maskedhig <- terra::mask(hiqual, masks_vect[[m]])
        
        maskedall <- terra::mask(onerast,  masks_vect[[m]])
        maskedall <- terra::ifel(maskedall >= 0, 1, NA) 
        
        hipix <-terra::global(maskedhig, sum , na.rm=TRUE)
        all.px <- terra::global(maskedall, sum, na.rm=TRUE)
        
        df_pixels_one <- cbind(names(all.group.rasters)[i],rast.cat.names[k], masks_labs[m],all.px, hipix)
        
        df_pixels <- rbind(df_pixels, df_pixels_one)
        
      }
      
      
    }
    
  }
}


summary(df_pixels)
names(df_pixels) <- c("Migra", "Raster", "Elev", "total", "high")
df_pixels$not_high <- df_pixels$total-df_pixels$high


head(df_pixels)

#reorganizing because of 
df_pixels.fixed <- within(df_pixels, Raster <- factor(Raster, levels = rast.cat.names))
df_pixels.fixed <- within(df_pixels.fixed, Migra <- factor(Migra, levels = c("RES", "SDM", "LDM")))
df_pixels.fixed <- within(df_pixels.fixed, Elev <- factor(Elev, levels = masks_labs))


p<-df_pixels.fixed |>
  tidyr::pivot_longer(cols = high:not_high, names_to = "cat")|>
  #dplyr::group_by(Migra, Raster)|>
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x = Elev, y = value, fill = cat)#, position = "fill"
  ) +
  ggplot2::facet_grid(Migra~ Raster, labeller = "label_both", 
                      #scales = "free", 
                      switch = "y")+
  ggplot2::theme(
    legend.position = "bottom"
  )+
  ggplot2::scale_fill_manual(values=c("#56B4E9", 
                                      "#D55E00" 
  ),
  labels = c("High value", "Other"))+
  ggplot2::ylab(bquote("Extent  ("* km^2* ")"))+
  ggplot2::xlab("Elevation")

ggplot2::ggsave(p, filename = "elevationbinnsv3.png", path = "plots/", units = "in", width = 16, height = 11, dpi = 300, bg = "white")

#to visualize the bins


par(mfrow = c(1,6))
terra::plot(maskEle01, legend = FALSE)

terra::plot(terra::vect(bcr), add = TRUE)
terra::plot(maskEle02, legend = FALSE)
terra::plot(terra::vect(bcr), add = TRUE)

terra::plot(maskEle03, legend = FALSE)
terra::plot(terra::vect(bcr), add = TRUE)

terra::plot(maskEle04, legend = FALSE)
terra::plot(terra::vect(bcr), add = TRUE)

terra::plot(maskEle05, legend = FALSE)
terra::plot(terra::vect(bcr), add = TRUE)

terra::plot(maskEle06, legend = FALSE)
terra::plot(terra::vect(bcr), add = TRUE)

############## NOT USED
###############################################
### 1 ii) Mapping of points and Provinces and/or Ecoregions --- of all spp
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
#### 2) Mapping of Protected areas Alaska, Yukon, NWT #### ALREADY IN THE H script!!!!!!!!!!!!!!!!
####################################################

#call yukon_PAs_crs from pre- run of script H

#one plot as sample 

##FULL RASTER with PAs outlined
#model_PAs_ecoregs <- 
  ggplot2::ggplot()+
  ggplot2::geom_sf(data = poly, fill = "grey") +
  ggplot2::geom_sf(data = usa_crop, fill = "white")+
  ggplot2::geom_sf(data = canada_crop, fill = "white")+
 
  tidyterra::geom_spatraster(
    data = (all.group.rasters[[1]])[[4]]
)+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    
  ggplot2::geom_sf(data = yukon_PAs_crs, alpha = 0 )+
  ggplot2::geom_sf(data = usa_crop, alpha =0)+
  ggplot2::geom_sf(data = canada_crop, alpha =0)+
  ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
  ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                           terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                    ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                    expand = FALSE)+
  ggplot2::theme_bw()


#ggplot2::ggsave(model_PAs_ecoregs, filename = "model_PAs_ecoregs.png", path = "plots/", units = "in", width = 7.5, height = 7, dpi = 300, bg = "white")

#map a sample stack or the three RES, LDM, SDM for Ref x Suit

  
  
#one plot as sample 

##Masked RASTER with PAs
  maskedPATest <- terra::mask(
    (all.group.rasters[[1]])[[4]] , yukon_PAs_crs
    
  )
  terra::plot(maskedPATest)
  terra::plot(maskedPATest)
  er <- terra::rast(terra::ext(maskedPATest), resolution=terra::res(maskedPATest), crs = rast.crs)
  terra::values(er) <- 1
  terra::plot(er)
  xx <- terra::ifel( maskedPATest > 0, NA, er ) # use this to cover the raster
  terra::plot(xx)

  xxx <- terra::as.polygons(xx) #make a polygon out of the NAing raster for PAs
  terra::plot(xxx)
  
  

  
#one plot as sample 
  ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    
    tidyterra::geom_spatraster(
      data = (all.group.rasters[[1]])[[4]]
    )+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    tidyterra::geom_spatvector(
      data = xxx, fill = "white"#ggplot2::aes(color = "white")
    )+
    ggplot2::geom_sf(data = yukon_PAs_crs, alpha = 0 )+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(can_us_crop)[1], 
                             terra::ext(can_us_crop)[2]),########## needs the cropped shape as the Current rasters are larger
                      ylim = c(terra::ext(can_us_crop)[3], terra::ext(can_us_crop)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()


########################################################################
########################################################################
# samples of BOCH and BCCH
  
  {boch.lab<- "BOCH"
  bcch.lab <- "BCCH"
  
  
  #plot curr scaled, refugia, ref x suit and suit
  refxsuit.boch <- terra::rast(x =  files.to.read[grep(paste0(boch.lab,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
  terra::crs(refxsuit.boch) <- rast.crs # assign CRS to avoid warning
  refxsuit.boch <- terra::mask(refxsuit.boch, NA_rast.crs)
  refxsuit.boch <- terra::mask(refxsuit.boch, AKrast)
  
  refxsuit.bcch <- terra::rast(x =  files.to.read[grep(paste0(bcch.lab,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
  terra::crs(refxsuit.bcch) <- rast.crs # assign CRS to avoid warning
  refxsuit.bcch <- terra::mask(refxsuit.bcch, NA_rast.crs)
  refxsuit.bcch <- terra::mask(refxsuit.bcch, AKrast)
  
  ref.boch <- terra::rast(x = files.to.read[grep(paste0(boch.lab,"_","Refugia_RCPmean.tif"), files.to.read)])
  
  terra::crs(ref.boch) <- rast.crs # assign crs to avoid warning
  ref.boch <- terra::mask(ref.boch, NA_rast.crs)
  ref.boch <- terra::mask(ref.boch, AKrast)
  
  suit.boch <- terra::rast(x =  files.to.read[grep(paste0(boch.lab,"ScaledSuitability_RCPmean.tif"), files.to.read)])
  terra::crs(suit.boch) <- rast.crs # assign CRS to avoid warning
  suit.boch <- terra::mask(suit.boch, NA_rast.crs)
  suit.boch <- terra::mask(suit.boch, AKrast)
  
  
  boch.plot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =refxsuit.boch)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.boch)[1], terra::ext(refxsuit.boch)[2]),
                      ylim = c(terra::ext(refxsuit.boch)[3], terra::ext(refxsuit.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Suitable refugia for", boch.lab))
  
  bcch.plot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =refxsuit.bcch)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.bcch)[1], terra::ext(refxsuit.bcch)[2]),
                      ylim = c(terra::ext(refxsuit.bcch)[3], terra::ext(refxsuit.bcch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Suitable refugia for", bcch.lab))
  
  
  sample_boch_bcch <- cowplot::plot_grid(bcch.plot, boch.plot , labels = c("A", "B"
  ), 
  #rel_widths = c(1.75, 2, 2),  
  nrow = 1
  )
  
  ggplot2::ggsave(sample_boch_bcch, filename = paste0("sample_boch_bcch.png") ,
                  path = "plots/", units = "in", width = 10, height = 3.75, dpi = 300, bg = "white")
  
  
  
  boch.curr <- readRDS(files.vect[grep(boch.lab, files.vect)] )
  Max95<-quantile(boch.curr$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
  
  rast1 <- boch.curr |>
    dplyr::mutate(
      std.mean = Mean/Max95
    )
  rast1<-terra::rast(rast1, crs =  rast.crs)  
  rast1 <- terra::ifel(rast1 >=1, 1, rast1 ) # cap to a max of 1
  
  rast1 <- rast1[[4]] # dplyr::select only the Mean values layer, not the standardized/scaled anymore [[4]]
  terra::crs(rast1) <- rast.crs # assign CRS to avoid warning
  rast1 <- terra::crop(rast1, NA_rast.crs)
  
  rast1 <- terra::mask(rast1, NA_rast.crs)
  rast1 <- terra::mask(rast1, AKrast)
  
  
  
  bcch.curr <- readRDS(files.vect[grep(bcch.lab, files.vect)] )
  Max95<-quantile(bcch.curr$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
  
  rast2 <- bcch.curr |>
    dplyr::mutate(
      std.mean = Mean/Max95
    )
  rast2<-terra::rast(rast2, crs =  rast.crs)  
  
  rast2 <- terra::ifel(rast2 >=1, 1, rast2 ) # cap to a max of 1
  
  rast2 <- rast2[[4]] # dplyr::select only the Mean values layer, not the standardized/scaled anymore [[4]]
  terra::crs(rast2) <- rast.crs # assign CRS to avoid warning
  rast2<- terra::crop(rast2, NA_rast.crs)
  
  rast2 <- terra::mask(rast2, NA_rast.crs)
  rast2 <- terra::mask(rast2, AKrast)
  
  
  boch.curplot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =rast1)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.boch)[1], terra::ext(refxsuit.boch)[2]),
                      ylim = c(terra::ext(refxsuit.boch)[3], terra::ext(refxsuit.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Current suitable hab.", boch.lab))+
    ggplot2::scale_size_continuous(range = c(0, 1.00))
  
  bcch.curplot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =rast2)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.bcch)[1], terra::ext(refxsuit.bcch)[2]),
                      ylim = c(terra::ext(refxsuit.bcch)[3], terra::ext(refxsuit.bcch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Current suitable hab. for", bcch.lab))
  
  
  
  boch.refplot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =ref.boch)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.bcch)[1], terra::ext(refxsuit.bcch)[2]),
                      ylim = c(terra::ext(refxsuit.bcch)[3], terra::ext(refxsuit.bcch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Refugia probability for",boch.lab))
  
  sample_boch_bcchCUR <- cowplot::plot_grid(bcch.curplot, boch.curplot , labels = c("A", "B"
  ), 
  #rel_widths = c(1.75, 2, 2),  
  nrow = 1
  )
  
  boch.suit.plot <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =suit.boch)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.boch)[1], terra::ext(refxsuit.boch)[2]),
                      ylim = c(terra::ext(refxsuit.boch)[3], terra::ext(refxsuit.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( option = "turbo",direction = -1, na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Future suitability for", boch.lab))
  
  ggplot2::ggsave(sample_boch_bcchCUR, filename = paste0("sample_boch_bcchCUR.png") ,
                  path = "plots/", units = "in", width = 10, height = 3.75, dpi = 300, bg = "white")
  
  
  boch.plots <- cowplot::plot_grid(boch.curplot, boch.refplot, boch.suit.plot, boch.plot, rel_widths = c(1.95, 2, 2,2),  labels = c("A", "B","C", "D"), nrow = 1)
  
  ggplot2::ggsave(boch.plots, filename = paste0("sample_boch.plots.png") ,
                  path = "plots/", units = "in", width = 15, height = 3.75, dpi = 300, bg = "white")
  
  
### ONLY FOR BOCH
### Current and futer densities
  
#present
  boch.curr <- readRDS(files.vect[grep(boch.lab, files.vect)] )
  Max95<-quantile(boch.curr$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
  rast1<-terra::rast(boch.curr, crs =  rast.crs)  
  rast1<-rast1[[1]]
  rast1 <- terra::crop(rast1, NA_rast.crs)
  
  rast1 <- terra::mask(rast1, NA_rast.crs)
  rast1 <- terra::mask(rast1, AKrast)
  rast1 <- terra::ifel(rast1 >=Max95, Max95, rast1 ) # cap to a max of 1
  
  
#future
file.future <-list.files("C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/github/BorealRefugiaYT/data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds", full.names = TRUE)
future.spp <-file.future[grep(boch.lab, file.future)]# vector of future models RDS for 1 spp
  
futures.boch <- NULL  
  for (m in future.spp) { #loops over each future scenario pres abs and gets a mean alt and mean core area
     future1<- readRDS(m)
     #Max95<-quantile(future1$Mean, 0.95, na.rm=TRUE) #top 95% density of Mean
     future1<-terra::rast(future1)# make RDS a raster
     future1<-future1[[1]]
     future1 <- terra::crop(future1, NA_rast.crs)
     
     future1 <- terra::mask(future1, NA_rast.crs)
     future1 <- terra::mask(future1, AKrast)
     future1 <- terra::ifel(future1 >=Max95, Max95, future1 ) # cap to a max of 1
     
     #plot(future1)
     terra::crs(future1) <- rast.crs #assign crs to avoid warning
     #plot(future1)
     #future1 <- terra::mask(future1, bcr) #MASKING TO ONLY GET VALUES WITHIN THE ECOREGIONS
     #future1 <- terra::ifel(future1 ==1, 1, NA) # only the values of presence in the core area remain
     #altfut1 <- terra::mask(elevation, future1) 
     #mean.altfut1 <- mean(terra::values(altfut1$elevation_1KMmd_GMTEDmd), na.rm = TRUE)
     
     futures.boch[[m]]<- future1
     
 
   }
#get a mean of this list of rasters


one.future.boch <- terra::app(terra::rast(futures.boch), "mean")#/length(refxsuit.mean.list)


  
  
  boch.dens.cur <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =rast1)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(one.future.boch)[1], terra::ext(one.future.boch)[2]),
                      ylim = c(terra::ext(one.future.boch)[3], terra::ext(one.future.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( direction = -1,na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Current pres/abs", boch.lab))
  
  boch.dens.fut<- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =one.future.boch)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(one.future.boch)[1], terra::ext(one.future.boch)[2]),
                      ylim = c(terra::ext(one.future.boch)[3], terra::ext(one.future.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( na.value="transparent", direction = -1)+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Future pres/abs", boch.lab))
  
  
  
  
  
  
  boch.densplots <- cowplot::plot_grid(boch.dens.cur, boch.dens.fut, nrow = 1)
 
   ggplot2::ggsave(boch.densplots, filename = paste0("boch.densplots.png") ,
                  path = "plots/", units = "in", width = 8, height = 3.75, dpi = 300, bg = "white")
  
  
  
  
## ONLY FOR BOCH
### pres abs
  
  sample1<- terra::rast( file.presabs[grep("BOCH", file.presabs)][grep("Present",file.presabs[grep(i, file.presabs)])])
  
  terra::crs(sample1) <- rast.crs
  sample1 <- terra::crop(sample1, elevation)  # crop present raster, with elevation raster
  sample1<- terra::ifel(sample1 ==1,  1, NA)
  
  
  future.spp <-file.presabs[grep("BOCH", file.presabs)][-grep("Present",file.presabs[grep("BOCH", file.presabs)])] # vector of future models RDS for 1 spp
  
  future1<-terra::rast(future.spp[8]) # make RDS a raster
  #plot(future1)
  terra::crs(future1) <- rast.crs #assign crs to avoid warning
  #plot(future1)
  future1 <- terra::ifel(future1 ==1, 1, NA) # only the values of presence in the core area remain
  
  
  boch.pres.cur <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =sample1)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.boch)[1], terra::ext(refxsuit.boch)[2]),
                      ylim = c(terra::ext(refxsuit.boch)[3], terra::ext(refxsuit.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( direction = -1,na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Current pres/abs", boch.lab))+
    ggplot2::theme(legend.position = "none")
  
  boch.pres.fut<- ggplot2::ggplot()+
    ggplot2::geom_sf(data = poly, fill = "grey") +
    ggplot2::geom_sf(data = usa_crop, fill = "white")+
    ggplot2::geom_sf(data = canada_crop, fill = "white")+
    tidyterra::geom_spatraster(data =future1)+
    ggplot2::geom_sf(data = usa_crop, alpha =0)+
    ggplot2::geom_sf(data = canada_crop, alpha =0)+       
    ggplot2::geom_sf(data = BCR4.1_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::geom_sf(data = BCR4.0_USACAN, ggplot2::aes(), linewidth=1.1 ,color = "black", alpha = 0)+
    ggplot2::coord_sf(xlim=c(terra::ext(refxsuit.boch)[1], terra::ext(refxsuit.boch)[2]),
                      ylim = c(terra::ext(refxsuit.boch)[3], terra::ext(refxsuit.boch)[4]),
                      expand = FALSE)+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_viridis_c( na.value="transparent")+ ### DIANA's paper style?
    ggplot2::theme(
      plot.margin = ggplot2::margin(0.1,0.1,0.1,0.1, "cm")
    )+
    ggplot2::ggtitle(paste("Future pres/abs", boch.lab))+
    ggplot2::theme(legend.position = "none")
  
  boch_pas_plots <- cowplot::plot_grid(boch.pres.cur, boch.pres.fut, nrow = 1)
  ggplot2::ggsave(boch_pas_plots, filename = paste0("sample_boch_pas_plots.png") ,
                  path = "plots/", units = "in", width = 8, height = 3.75, dpi = 300, bg = "white")
  
  
  
  
  }
  
  

#################
#################
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
