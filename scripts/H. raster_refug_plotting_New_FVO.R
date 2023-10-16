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
              
################### Load groups ################### 
#load classification file and make make vectors with codes
class_spp <- read.csv("data/SpeciesStatus.csv")

unique(class_spp$Migration1)

df.spp.names.merged <- read.csv("data/df.all.birds.merged.csv")
names(df.spp.names.merged)

spp.to.work <- df.spp.names.merged %>% 
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  select(all.birds) %>% 
  pull()


LDM <- class_spp %>%
  filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Neotropical migrant") %>% 
  distinct(species_code)%>% 
  pull()

SDM <- class_spp %>%
  filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Short distance migrant") %>% 
  distinct(species_code)%>% 
  pull()

RES <- class_spp %>%
  filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Resident") %>% 
  distinct(species_code)%>% 
  pull()

NOM <- class_spp %>%
  filter(species_code %in% spp.to.work) %>% 
  filter(Migration1 == "Nomadic") %>% 
  distinct(species_code) %>% 
  pull()

N_assig <- class_spp %>%
  filter(species_code %in% spp.to.work) %>% 
  filter(is.na(Migration1)) %>% 
  distinct(species_code)%>% 
  pull()

#set refugia raster results directorie
files.to.read<-list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN", full.names = TRUE)

ref.ras.dir <- "data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN/"

#Pick vector to run in the loop!
ref.mean.list <- list()
suit.mean.list <- list()
refxsuit.mean.list <-  list()

for (i in SDM) {
  
  #rasters are sometimes tif or tiff, so the grep() solves it
  ref.ras1 <- rast(x = files.to.read[grep(paste0(i,"_","Refugia_RCPmean.tif"), files.to.read)])
  suit.ras1 <- rast(x =  files.to.read[grep(paste0(i,"ScaledSuitability_RCPmean.tif"), files.to.read)])
  refxsuit.ras1 <- rast(x =  files.to.read[grep(paste0(i,"Suitability_by_Refugia_RCPmean.tif"), files.to.read)])
  
  names(refxsuit.ras1) <- terra::varnames(refxsuit.ras1)
  names(ref.ras1) <- terra::varnames(ref.ras1)
  names(suit.ras1) <- terra::varnames(suit.ras1)
  
  ref.mean.list[[i]] <- ref.ras1
  suit.mean.list[[i]] <- suit.ras1
  refxsuit.mean.list[[i]] <- refxsuit.ras1
  
}

# for all RES, LDM, SDM did sum

#for SDM also ave to compare that visually look the same

SDM.ref.sum <- app(rast(ref.mean.list), "sum")#/length(ref.mean.list)
SDM.suit.sum <- app(rast(suit.mean.list), "sum")#/length(suit.mean.list)
SDM.refxsuit.sum <- app(rast(refxsuit.mean.list), "sum")#/length(refxsuit.mean.list)

###ONE plot only
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
  scale_fill_viridis_c(option = "turbo")+ ### DIANA's paper style?
  theme(
    plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
  )

#for all birds in a category
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
    scale_fill_viridis_c(option = "turbo")+ ### DIANA's paper style?
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


#make sets of plots of only the 3 categories (ref, suit, ref x suit) per species grouping (res, ldm, sdm)
thre.cats  <- c(RES.ref.sum,RES.suit.sum,RES.refxsuit.sum)
thre.cats.names  <- c("RES.ref.sum","RES.suit.sum","RES.refxsuit.sum")

#thre.cats  <- c(LDM.ref.sum,LDM.suit.sum,LDM.refxsuit.sum)
#thre.cats.names  <- c("LDM.ref.sum","LDM.suit.sum","LDM.refxsuit.sum")

#thre.cats  <- c(SDM.ref.sum,SDM.suit.sum,SDM.refxsuit.sum)
#thre.cats.names  <- c("SDM.ref.sum","SDM.suit.sum","SDM.refxsuit.sum")

#thre.cats  <- c(SDM.ref.ave,SDM.suit.ave,SDM.refxsuit.ave)
#thre.cats.names  <- c("SDM.ref.ave","SDM.suit.ave","SDM.refxsuit.ave")

#thre.cats  <- c(LDM.ref.ave,LDM.suit.ave,LDM.refxsuit.ave)
#thre.cats.names  <- c("LDM.ref.ave","LDM.suit.ave","LDM.refxsuit.ave")


three.cat.list<- list()
for (j in 1:length(names(thre.cats))) {
  
  rast1 <- thre.cats[[j]]
  
  plot.one <- ggplot()+
    geom_spatraster(data =rast1)+
    #geom_sf(data = usa_can_crop , aes(), fill = "green", alpha = 0)+
    geom_sf(data = BCR4.1_USACAN, aes(), linewidth=1.5 ,color = "black", fill = "blue", alpha = 0)+
    geom_sf(data = BCR4.0_USACAN, aes(), linewidth=1.5 ,color = "black", fill = "red", alpha = 0)+
    coord_sf(xlim=c(ext(RES.ref.sum)[1], ext(RES.ref.sum)[2]),
             ylim = c(ext(RES.ref.sum)[3], ext(RES.ref.sum)[4]),
             expand = FALSE)+
    scale_fill_terrain_c()+ ###WORKS! may be best because of white color on zeros,   na.value = NA not doing anything in this 
    #scale_fill_distiller()+
    #scale_fill_binned(type = "viridis")
    #scale_fill_continuous(type = "viridis")+
    #scale_fill_gradient(low="red", high="green")+
    theme_bw()+
    ggtitle(thre.cats.names[[j]])+
    #scale_fill_viridis_c(option = "turbo")+ ### DIANA's paper style?
    theme(
      plot.margin = margin(0.1,0.1,0.1,0.1, "cm")
    )
  
  three.cat.list[[j]] <- plot.one
  
}

three.plots <- cowplot::plot_grid(plotlist = three.cat.list, nrow = 1, ncol = 3 )
ggsave(three.plots, filename = "SDM.sum.TERR.png", path = "plots/", units = "in", width = 10, height = 3, dpi = 300, bg = "white")
