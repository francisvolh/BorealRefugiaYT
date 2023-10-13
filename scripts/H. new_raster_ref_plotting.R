library(ggplot2)
library(terra)
library(sf)
library(tidyterra)

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
              