library(dplyr)
library(sf)
library(ggplot2)

################################################################################
################################################################################
### 1 i) Mapping of sample points  and Ecoregions ---  
################################################################################
################################################################################

### run the required preliminary code for script H 

{
  
  mexico <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm41_MEX/gadm41_MEX_1.shp") #### new 2021 version HUGE SHAPEFILE
  
  NA_CEC_Eco_Level2 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/NA_Terrestrial_Ecoregions_v2_Level_II_Shapefile/NA_Terrestrial_Ecoregions_v2_level2.shp") #### new 2021 version HUGE SHAPEFILE
  
  canada <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_CAN_shp/gadm36_CAN_1.shp")
  USA1 <- sf::st_read("data/YT Boreal Refugia Drive/YK Refugia Code and material/mapping resources/gadm36_USA_shp/gadm36_USA_1.shp")
  USA1<-USA1|>
    dplyr::filter(NAME_1 != "Hawaii")
  
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

USA_test<- sf::st_transform(USA1, rast.crs )

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

}
#turbo_pal <- c(viridis::viridis(n = 14, direction = -1))


#Map and inset

E <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = hilldf_single,
                     ggplot2::aes(x, y, fill = hillshade),
                     show.legend = FALSE) +
  ggplot2::scale_fill_distiller(palette = "Greys") +
  ggplot2::xlab(NULL)+
  ggplot2::ylab(NULL)+
  ggnewscale::new_scale_fill()+
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_gray, fill =  "gray" , color = NA, alpha = 0.2)+
  ggplot2::geom_sf(data = NA_CEC_Eco_Level2_short, ggplot2::aes(fill = NameL2_En), color = NA, alpha=0.25)+
  #ggplot2::scale_fill_viridis_d(direction = -1)+ ######## PLAY WILL COLOUR FOR BETTER VIZ
  ggplot2::scale_fill_manual( values =  c(  "Alaska Boreal Interior"= ggplot2::alpha("#FDE725FF", 0.2),
                                            "Alaska Tundra" = ggplot2::alpha("#CBE11EFF", 0.2),
                                            "Boreal Cordillera"= ggplot2::alpha("#97D83FFF", 0.5),
                                            "Boreal Plains"= ggplot2::alpha("#67CC5CFF", 0.2),
                                            "Brooks Range Tundra"= ggplot2::alpha("#40BC72FF", 0.2),
                                            "Cold Deserts" = ggplot2::alpha("#25AC82FF", 0.2),
                                            "Marine West Coast Forests"= ggplot2::alpha( "#1F998AFF", 0.2),
                                            #"Northern Arctic" = ggplot2::alpha("#39568CFF", 0.2),
                                            "Southern Arctic"= ggplot2::alpha("#24878EFF", 0.2),
                                            "Taiga Cordillera"= ggplot2::alpha("#2B748EFF", 0.5),
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
  ggplot2::geom_sf(data=xy_sf, size=0.01, colour="black"#, alpha=0.05
  ) +
  
  #ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "absence"),], ggplot2::aes(color ="#F8766D"), size=0.1, alpha=0.1)+
  #ggplot2::geom_sf(data=xy_sf2[which(xy_sf2$P_A == "presence"),], ggplot2::aes(color="blue"), size=0.1)+
  #ggplot2::scale_colour_manual(name = 'Observations', 
  #                            values =c('blue'='blue', "#F8766D"="#F8766D"), labels = c(  'absence', 'presence'))+
  ggplot2::coord_sf(expand = FALSE)+
  ggplot2::guides(fill = "none",
                  colour = ggplot2::guide_legend(override.aes = list(size=2.5)))+
  #ggplot2::ggtitle("Full dataset: presence/absence")+
  ggplot2::theme(
    text= ggplot2::element_text(size=10),
    legend.position = "none",
    legend.margin = ggplot2::margin(6, 6, 6, 6)
  )



USAreproj <- sf::st_transform(USA1, rast.crs)

CANreproj <- sf::st_transform(canada, rast.crs)

Mexreproj <- sf::st_transform(mexico, rast.crs)


rect <- data.frame(
  x = c(-2791000, -2791000,  -1198000 , -1198000  ),
  y = c(1522000 , 3386000 , 3386000, 1522000)
)

country <- ggplot2::ggplot() +
  #geom_point(data = locsSIA, aes(x = Longitude, y = Latitude, color = Location), cex= 4) +
  ggplot2::xlab("Longitude")+
  ggplot2::ylab("Latitude")+  
  ggplot2::geom_sf(data = Mexreproj, ggplot2::aes(), color = NA, fill = "black" ) +
  
  ggplot2::geom_sf(data = USAreproj, ggplot2::aes(), color = NA, fill = "black" ) +
  ggplot2::geom_sf(data = CANreproj, ggplot2::aes(), color = NA, fill = "black")+#add basemap
  #geom_text(aes(x=LongLab, y=LatLab, label=Location), data=locsSIA, size=3, hjust=-0.15, col = "black")+
  #coord_sf(xlim = c(-135, -123), ylim = c(48, 56)) +#Choose coordinate limits
  #scale_color_manual(values = c("#999999", "#E69F00", 
  #                             "#56B4E9", "#009E73", 
  #                            "#F0E442", "#0072B2", 
  #                            "#D55E00", "#CC79A7"))+
  ggplot2::xlab(NULL)+
  ggplot2::ylab(NULL)+
  ggplot2::theme(
    plot.margin = ggplot2::margin(0,0,0,0, "cm"),
    legend.position = "none", 
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank())+
  #ggplot2::theme_bw()+
  ggplot2::geom_polygon(data = rect, ggplot2::aes(x, y, group = 1), 
                        colour = "white", fill = "transparent", linewidth = 2)

ecoregions_inset<- cowplot::ggdraw(E) +
  cowplot::draw_plot(
    { country 
      #theme(panel.border=element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.04, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.06,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.4, 
    height = 0.4)

#ecoregions_inset

ggplot2::ggsave(ecoregions_inset, filename = "ecoregions_inset5.png", path = "plots/", units = "in", width = 10, height = 10, dpi = 300, bg = "white")



##################################################################
##################################################################
############ 2)  Bin  elevations and # of pixels with high quality in those bins
############  <250m, 250-500, 500-750...
##################################################################


# be carefull with run: Migratory stacks or POP based stacks

#terra::plot(elevation)
{
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
  
  maskEle06<- terra::ifel(elevbcr >= 1250, elevbcr, NA)
  maskEle06<- terra::ifel(maskEle06 < 1500, 1, NA)
  
  maskEle07<- terra::ifel(elevbcr >= 1500, elevbcr, NA)
  maskEle07<- terra::ifel(maskEle07 < 1750, 1, NA)
  
  maskEle08<- terra::ifel(elevbcr >= 1750, elevbcr, NA)
  
  
  masks_vect<-list(maskEle01, maskEle02, maskEle03, maskEle04, maskEle05, maskEle06, maskEle07, maskEle08 )
  masks_labs<-c("<250", "250-500", "500-750", "750-1000", "1000-1250", "1250-1500", "1500-1750", ">1750")
}


{
  
  #all.group.rasters  # list of rasters, one per migra group (three cats), with 4 stacked raster each
  rast.cat.names<- c("Current Suitable Habitat","Refugia Probability","Future Suitable Habitat","Future Suitable Refugia")
  
  df_pixels<-NULL
  qs <-  c(0.50, 0.75)
  for (q in qs) {
    for (i in 1:length(all.group.rasters)) {
      
      oneset <- all.group.rasters[[i]]
      
      for (k in 1:length(oneset)) {
        
        onerast<- oneset[[k]]
        
        if (k ==1 ) {
          
          q75 <- quantile(terra::values(onerast), probs=q, na.rm=TRUE) #### for 50% only
        }
        
        hiqual <- terra::ifel(onerast >= q75, 1, NA) 
        
        for (m in 1:length(masks_vect)) {
          
          maskedhig <- terra::mask(hiqual, masks_vect[[m]])
          
          maskedall <- terra::mask(onerast,  masks_vect[[m]])
          maskedall <- terra::ifel(maskedall >= 0, 1, NA) 
          
          hipix <-terra::global(maskedhig, sum , na.rm=TRUE)
          all.px <- terra::global(maskedall, sum, na.rm=TRUE)
          
          df_pixels_one <- cbind(names(all.group.rasters)[i],rast.cat.names[k], masks_labs[m],q,all.px, hipix)
          
          df_pixels <- rbind(df_pixels, df_pixels_one)
          
        }
        
        
      }
      
    }
    
  }
  
 
}


head(df_pixels)

names(df_pixels) <- c("Migra", "Raster", "Elev", "quant", "total", "high")

########################################################################
### switch to pop based
#(df_pixels) <- c("Pop", "Raster", "Elev", "quant", "total", "high") # DONT DO THIS AS it requires to change the rest of the code

groupings_labs <- c("Decreasers","Increasers", "No change")


head(df_pixels)

##################new version with cummulative 25 and 50%

df_pixels.fixed<-df_pixels|>
  dplyr::mutate(quant =as.factor(quant))|>
  dplyr::mutate(
    high = dplyr::case_when( is.na(high) ~ 0, .default = high ) )|>
  tidyr::pivot_wider(id_cols =c("Migra", "Raster", "Elev", "total"), names_from = quant, values_from = high, names_expand = TRUE)|>
  dplyr::rename(high50 = "0.5", high25 = "0.75")|>
  dplyr::mutate(anew05 = high50- high25)|>
  dplyr::mutate(a_not_high = total - high50)|>
  dplyr::select(!c(high50, total))



#reorganizing because of 
df_pixels.fixed <- within(df_pixels.fixed, Raster <- factor(Raster, levels = rast.cat.names))
df_pixels.fixed <- within(df_pixels.fixed, Elev <- factor(Elev, levels = masks_labs))

# for migra based!!!!!!!!!!!!!!!!!!!!!!!!!!!!
df_pixels.fixed <- within(df_pixels.fixed, Migra <- factor(Migra, levels = c("RES", "SDM", "LDM")))

# for pop based
# no need to fix levels as will only work with decreasers


#to rename strips in facets (plots)
# New facet label names for dose variable
xlabs <- rast.cat.names[c(1,2,4)]
names(xlabs) <- xlabs

# New facet label names for supp variable
ylabs <- c("Residents","Short-distance", "Long-distance")# for plotting only
names(ylabs) <-  groupings_labs 

##########################################
## switch to pop based
ylabs <-   c("Decreasers","Increasers", "No change")
names(ylabs) <- c("decreasers","increasers", "no_change")
 


p<-
  df_pixels.fixed |>
  tidyr::pivot_longer(cols = high25:a_not_high, names_to = "Category")|>
  dplyr::select(Elev, value, Category, Raster, Migra)|>
  dplyr::filter(Raster != "Future Suitable Habitat")|>
  dplyr::filter(Raster != "Refugia Probability")|>
  
  #only for pop based ##### 
  #dplyr::filter(Migra  == "decreasers")|>
  dplyr::arrange(Category)|>
  ggplot2::ggplot()+
  ggplot2::geom_col(ggplot2::aes(x = Elev, y = value, fill = Category)#, position = "fill"
  ) +
  ggplot2::facet_grid(Migra~ Raster, labeller = ggplot2::labeller(Migra = ylabs, Raster = xlabs) ,
                      scales = "free", 
                      switch = "y")+
  ggplot2::theme(
    legend.position = "bottom",
    strip.text.x = ggplot2::element_text(size=15),
    strip.text.y = ggplot2::element_text(size=15),
    axis.text.x = ggplot2::element_text(size=10)
  )+
  ggplot2::scale_fill_manual(values=c("high25"="#56B4E9", 
                                      "anew05"= "#009E73",
                                      "a_not_high"="#D55E00"
                                      
  ),
  labels = c("Other", "High 50%","High 25%"))+
  ggplot2::ylab(bquote("Extent  ("* km^2* ")"))+
  ggplot2::xlab("Elevation")

#ggplot2::ggsave(p, filename = "elevationbinnsv6.png", path = "plots/", units = "in", width = 12, height = 11, dpi = 300, bg = "white")
#ggplot2::ggsave(p, filename = "elevationbinnsv6POP.png", path = "plots/", units = "in", width = 12, height = 5, dpi = 300, bg = "white")


