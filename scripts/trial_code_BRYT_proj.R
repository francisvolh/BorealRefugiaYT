
# check the species output files names / list of birds
setwd("C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/YT Boreal Refugia Drive/Plots_1991_Normal_Present Distributions")

list.files()

bird.names<-list.files()

bird.names1 <- substring(bird.names, 1,4)

df.names1  <- as.data.frame(x=bird.names1)
write.csv(df.names1, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/df.names1.csv")


setwd("C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds")
list.files()

bird.names<-list.files()

bird.names2 <- substring(bird.names, 1,4)

df.names2  <- as.data.frame(x=bird.names2)

write.csv(df.names2, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/df.names2.csv")

print(setdiff(bird.names2,bird.names1))

#get alpha codes  already extrated from AOS file
alpha.names <- read.csv(file.choose())

alpha.names$X4.LETTER.CODE <-substring(alpha.names$X4.LETTER.CODE, 1,4)

summary(alpha.names)

names.full <-merge(df.names2, alpha.names, by.x="bird.names2", by.y= "X4.LETTER.CODE", all.x=TRUE)
write.csv(names.full, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/names.full53.csv")


### looking into the cleaned bird data

all.birds <- read.csv(file.choose()) # AllBirdDataCleanedJul20.csv
names(all.birds)
unique(all.birds$spp) #there is a NULL and an NA row
length(unique(all.birds$spp))

all.birds[is.na(all.birds$spp),]
all.birds<-all.birds[!is.na(all.birds$spp),]
unique(all.birds$spp)
length(unique(all.birds$spp))

all.birds[which(all.birds$spp == "NULL"),] #### NULL

length(all.birds[which(all.birds$spp == "NULL"),]) ## 13 observations, do we care???

bird.data.names <- sort(unique(all.birds$spp), decreasing = FALSE)
bird.data.names

library(dplyr)
sub.birds <- all.birds %>% 
  filter(spp %in% unique(names.full$bird.names2))

summa.counts <- sub.birds %>% 
  group_by(spp) %>% 
  summarise(
    total = n()
    )

summa.counts %>% 
  filter(total < 4000)

library(ggplot2)

ggplot()+
  geom_bar(data = sub.birds, aes(x=spp, fill = spp))+
  theme_bw()+
  theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none"
  )
