library(dplyr)
library(ggplot2)
library(cowplot)
library(trelliscopejs)


#############################################################
# check the species output files names / list of birds
############################################################


#########
#SDM plots: Plots_1991_Normal_Present Distributions????
#list.files("data/YT Boreal Refugia Drive/Plots_1991_Normal_Present Distributions")

bird.names<-list.files("data/YT Boreal Refugia Drive/Plots_1991_Normal_Present Distributions")

bird.names1 <- substring(bird.names, 1,4)
length(bird.names1)

df.names1  <- as.data.frame(x=bird.names1)
names(df.names1) <- "x"

#write.csv(df.names1, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/df.names1.csv")
#51 species


#########
#Files_1991_Present_Mean90CI_rds
#list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds")

bird.names<-list.files("data/YT Boreal Refugia Drive/Files_1991_Present_Mean90CI_rds")

bird.names2 <- substring(bird.names, 1,4)
length(bird.names2)

df.names2  <- as.data.frame(x=bird.names2)

#write.csv(df.names2, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/df.names2.csv")
#53 species

#Files_1991_Future_Mean90CI_rd
list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds")
#folder contains 3 scenarios and 3 climate models, then 9 files per species

bird.names<-list.files("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds")
head(bird.names, n=12)

bird.names3 <- substring(bird.names, 1,4)
length(bird.names3)
length(bird.names3)/9

df3<-data.frame(codes = bird.names3, file_name = bird.names)

sum.df3 <- df3 %>%
  group_by(codes) %>% 
  summarise(
    File.Count = n()
  )

#View(sum.df3)

#pufi has 10 files
df3 %>%
  filter(codes ==  "PUFI")


#PUFIECEarth3__26_2071.1<-readRDS("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds/PUFIECEarth3__26_2071(1).rds")

#PUFIECEarth3__26_2071 <-readRDS("data/YT Boreal Refugia Drive/Files_1991_Future_Mean90CI_rds/PUFIECEarth3__26_2071.rds")

#summary(PUFIECEarth3__26_2071.1)
#summary(PUFIECEarth3__26_2071)

#confirmed they are copied RDS files, deleting extra (1)

bird.names3_cl <- unique(bird.names3)
length(bird.names3_cl)
bird.names3_cl
df.names3  <- as.data.frame(x=bird.names3_cl)

# 54 species listed, after cleaning up


print(setdiff(bird.names3_cl,bird.names1))

print(setdiff(bird.names3_cl,bird.names2))


#######
#Plots_1991_Normal_Refugia/Mean Plots

list.files("data/YT Boreal Refugia Drive/Plots_1991_Normal_Refugia/Mean Plots")

#folder contains 3 subfolders, using means folder

bird.names<-list.files("data/YT Boreal Refugia Drive/Plots_1991_Normal_Refugia/Mean Plots")

head(bird.names, n=12)

bird.names4 <- substring(bird.names, 1,4)
length(bird.names4)
df.names4  <- as.data.frame(x=bird.names4)

# 52 spp


print(setdiff(bird.names4,bird.names1))

print(setdiff(bird.names3,bird.names4))

print(setdiff(bird.names2, bird.names4))


#######

###Rasters_1991_Normal_Refugia and Habitat Suitability ----- discrepancy some duplicate files
#MEAN

list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN")
bird.names <- list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability MEAN")
length(bird.names)
length(bird.names)/3


bird.names5 <- substring(bird.names, 1,4)

df5<-data.frame(codes = bird.names5, file_name = bird.names)

sum.df5 <- df5 %>%
  group_by(codes) %>% 
  summarise(
    File.Count = n()
  )

#View(sum.df5)

#found a PDF named RegionMAT (a histogram overlap graph) moved into extra folder

bird.names5 <- unique(bird.names5)

df.names5  <- as.data.frame(x=bird.names5)

#51 sp in mean folder

print(setdiff(bird.names5,bird.names1))
print(setdiff(bird.names2, bird.names5))
print(setdiff(bird.names3, bird.names5))
print(setdiff(bird.names4, bird.names5))

print(setdiff(bird.names1, bird.names5))
print(setdiff(bird.names5, bird.names2))
print(setdiff(bird.names5, bird.names3))
print(setdiff(bird.names5, bird.names4))


#######
###Rasters_1991_Normal_Refugia and Habitat Suitability ----- discrepancy some duplicate files

#UPR90
list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability UPR90")
bird.names <- list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability UPR90")
length(bird.names)
length(bird.names)/3
bird.names6 <- substring(bird.names, 1,4)

bird.names6_cl <- unique(bird.names6)
length(bird.names6_cl)

df.bird.names6 <- data.frame(x=bird.names6_cl)

#LWR90
list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability LWR90")
bird.names <- list.files("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability LWR90")
length(bird.names)
length(bird.names)/3
bird.names7 <- substring(bird.names, 1,4)

bird.names7 <- unique(bird.names7)
#length(bird.names7_cl)
#52 spp but a bunch of repeated files

#idx<-grep("(1)", bird.names)

#to_delete <- bird.names[idx]

#file_name <- "data_file.txt"


#for (i in 1:length(to_delete)) {
 # if (file.exists(paste0("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability LWR90/",to_delete[i]))) {
  #  #unlink(file_name)
  #  print("File exists")
  #} else{
   # print("File not exists..")
#  } 
#}

#for (i in 1:length(to_delete)) {
 # if (file.exists(paste0("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability LWR90/",to_delete[i]))) {
  #  unlink((paste0("data/YT Boreal Refugia Drive/Rasters_1991_Normal_Refugia and Habitat Suitability/1991 Refugia Habitat Suitability LWR90/",to_delete[i])))
   # print(paste0("File  ", i, "was deleted"))
#  } else{
 #   print("File not exists..")
  #} 
#}


df.bird.names7 <- data.frame(x=bird.names7)

###
###BRT_output/GBMs_NoBarrier1991 ----- 152 files in folder
#data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1991


list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1991")

bird.names <- list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1991")

length(bird.names)
length(bird.names)/2


bird.names8 <- substring(bird.names, 1,4)

bird.names8 <- unique(bird.names8)

df.names8  <- as.data.frame(x=bird.names8)

#76 sp in BRT 1991 normals


###
###BRT_output/GBMs_NoBarrier1991 ----- 106files in folder
#data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1961


list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1961")

bird.names <- list.files("data/YT Boreal Refugia Drive/YK Refugia Code and material/BRT_output/GBMs_NoBarrier1961")

length(bird.names)
length(bird.names)/2


bird.names9 <- substring(bird.names, 1,4)

bird.names9 <- unique(bird.names9)

df.names9  <- as.data.frame(x=bird.names9)

#53 sp in BRT 1961 normals

##################################################################


#get alpha codes  already extrated from AOS file
alpha.names <- read.csv(file.choose())
class(alpha.names)
alpha.names$X4.LETTER.CODE <-substring(alpha.names$X4.LETTER.CODE, 1,4)

summary(alpha.names)

#names.full <-merge(df.names2, alpha.names, by.x="1991_Present_Mean", by.y= "X4.LETTER.CODE", all.x=TRUE)

#write.csv(names.full, "C:/Users/vanoordtlahozf/OneDrive - EC-EC/Documents/Boreal Refugia/names.full53.csv")

#############################################################
### looking into the cleaned bird data

all.birds <- read.csv("data/YT Boreal Refugia Drive/YK Refugia Code and material/AllBirdDataCleanedJul20.csv") # 
length(unique(all.birds$LocationID))
length(unique(all.birds$SurveyID))

all.birds.filtered <- all.birds %>% 
  filter(spp != "NULL")
length(unique(all.birds.filtered$LocationID))
length(unique(all.birds.filtered$SurveyID))


all.birds.spp <- unique(all.birds$spp)

df.all.birds.spp <- data.frame(all.birds =  all.birds.spp)
names(df.all.birds.spp)
names(alpha.names)

df.all.birds.spp <- df.all.birds.spp %>% 
  filter(!is.na(all.birds)) %>% ## note here all.birds is the variable name as well, and not the df
  filter(!all.birds == "NULL")## note here all.birds is the variable name as well, and not the df

df.all.birds.names <-merge(df.all.birds.spp, alpha.names, by.x="all.birds", by.y= "X4.LETTER.CODE", all.x=TRUE)



#### merge to check discrepancies in files
#92 spp in the all birds big data cleaned file

#merging
names(df.names1)

df.names1$Plots_1991_Distri <- "Plots_1991_Distri"

df.all.birds.merged <-merge(df.all.birds.names, df.names1, by.x = "all.birds", by.y ="x", all.x = TRUE)

df.all.birds.merged <- df.all.birds.merged %>% 
  select(!c("SCIENTIFIC.NAME", "X6.LETTER.CODE"))


df.names2$'1991_Present_Mean' <-"1991_Present_Mean"

df.all.birds.merged <-merge(df.all.birds.merged, df.names2, by.x = "all.birds", by.y ="bird.names2", all.x = TRUE)

df.names3  <- as.data.frame(x=bird.names3_cl)

df.names3$'1991_Future_Mean' <- "1991_Future_Mean"
df.all.birds.merged <-merge(df.all.birds.merged, df.names3, by.x = "all.birds", by.y ="bird.names3_cl", all.x = TRUE)


df.names4$'Plots_1991_Refugia_Mean' <- "Plots_1991_Refugia_Mean"
df.all.birds.merged <-merge(df.all.birds.merged, df.names4, by.x = "all.birds", by.y ="bird.names4", all.x = TRUE)





df.names5$Ref_x_Hab_Suit_Mean <- "Ref_x_Hab_Suit_Mean"
df.all.birds.merged <-merge(df.all.birds.merged, df.names5, by.x = "all.birds", by.y ="bird.names5", all.x = TRUE)

df.bird.names6

df.bird.names6$Ref_x_Hab_Suit_UPR <- "Ref_x_Hab_Suit_UPR"

df.all.birds.merged <-merge(df.all.birds.merged, df.bird.names6, by.x = "all.birds", by.y ="x", all.x = TRUE)



df.bird.names7$Ref_x_Hab_Suit_LWR <- "Ref_x_Hab_Suit_LWR"
df.all.birds.merged <-merge(df.all.birds.merged, df.bird.names7, by.x = "all.birds", by.y ="x", all.x = TRUE)

df.names8$BRT_1991 <- "BRT_1991"
df.all.birds.merged <-merge(df.all.birds.merged, df.names8, by.x = "all.birds", by.y ="bird.names8", all.x = TRUE)



df.names9$BRT_1961 <- "BRT_1961"

df.all.birds.merged <-merge(df.all.birds.merged, df.names9, by.x = "all.birds", by.y ="bird.names9", all.x = TRUE)

#adding fit statistics column
fitStats1991 <- read.csv(file.choose())
sppFit1991 <- fitStats1991$Spp
#length(sppFit1991)
#sort(sppFit1991)
#length(unique(sppFit1991))
#fitStats1991[which(fitStats1991$Spp == "OVEN"),]

sppFit1991 <- sort(unique(sppFit1991))
sppFit1991 <- as.data.frame(sppFit1991)
sppFit1991$sppFit1991val <- "sppFit1991"

#names(sppFit1991)

df.all.birds.merged <-merge(df.all.birds.merged, sppFit1991, by.x = "all.birds", by.y ="sppFit1991", all.x = TRUE)






####
#classification for groupings
class_spp <- read.csv("data/SpeciesStatus.csv")
names(class_spp)
unique(class_spp$Migration1)
class_spp <- dplyr::select(class_spp, c("species_code", "Migration1"))

names(df.all.birds.merged)
df.all.birds.merged <-merge(df.all.birds.merged, class_spp, by.x = "all.birds", by.y ="species_code", all.x = TRUE)






out_range <-c( "AMRE", 	"BAWW",	"BBWA",	"BHVI",	"CAWA",
               "CCSP",	"CMWA",	"EUST",	"EVGR",	"HOSP",
               "MAWA",	"MOWA",	"OVEN","PAWA",	"PIWO",	
               "RBGR", 	"REVI",	"SWSP",	 "VESP", 	"WIWR",
               "AMCR")
out_range_df<-data.frame( spp = out_range, dropped = "out_range")

bad_qpad<-c("BRBL","GCSP","RECR","TOWA","TOSO")
bad_qpad_df<-data.frame( spp = bad_qpad, dropped = "bad_qpad")

raptors <- c("BAEA", "OSPR", "AMKE", "RTHA",  "SOGR", "NOHA","MERL")
raptors_df <- data.frame( spp = raptors, dropped = "raptors")

irruptive <- c("CORE", "PISI", "WWCR", "CONI")
irruptive_df <- data.frame( spp = irruptive, dropped = "irruptive")

no_offsets <- c("VGSW", "MOCH", "MGWA", "CAHU", "BHGR", "CBCH", "NRWS", "PAWR", 
                "PSFL", "RBSA", "RUHU", "STJA", "WEME", "WIFL")
no_offsets_df <- data.frame( spp = no_offsets, dropped = "no_offsets")

colonial<-"BANS"
colonial_df <- data.frame( spp = colonial, dropped = "colonial")

unknown_drop <- c("AMPI", "BCCH", "BEKI", "BHCO", "BOWA", "LALO", "PHVI", "RUBL", "WETA", "WEWP", "WIPT")
unknown_drop_df <- data.frame( spp = unknown_drop, dropped = "unknown_drop")

dropped_spp_df <- rbind(out_range_df,bad_qpad_df,raptors_df,irruptive_df,no_offsets_df,colonial_df,unknown_drop_df)


df.all.birds.merged <- merge(df.all.birds.merged, dropped_spp_df, by.x = "all.birds", by.y = "spp", all.x = TRUE)



#write.csv(df.all.birds.merged, "data/df.all.birds.merged.csv", row.names= FALSE)

#df.all.birds.merged <- read.csv( "data/df.all.birds.merged.csv", na.strings = c("", "NA"))

df.all.birds.merged[ ,c("all.birds", "dropped_spp", "Ref_x_Hab_Suit_Mean" )] %>% 
  filter(is.na(Ref_x_Hab_Suit_Mean)) %>% 
  filter(is.na(dropped_spp)) %>% 
  pull(all.birds)

df.all.birds.merged[ ,c("all.birds", "sppFit1991val", "Ref_x_Hab_Suit_Mean" )] %>% 
  filter(!is.na(Ref_x_Hab_Suit_Mean)) %>% 
  filter(is.na(sppFit1991val)) %>% 
  pull(all.birds)

#####
#look at a the fit BRT stats of one bird
#AMPI has no model, to check fit
#is not posthoc removed



#ALFL
load(file.choose() )

summary(GBM)

summary(GBM[1])

first.list <- GBM[1]

firs.first <- first.list[[1]]
firs.first$fit
firs.first$initF


################################################
names(all.birds)
unique(all.birds$spp) #there is a NULL and an NA row
length(unique(all.birds$spp))

all.birds[is.na(all.birds$spp),] ##RANDOM NA observation row

all.birds<-all.birds[!is.na(all.birds$spp),] #get rid of NA row

unique(all.birds$spp)

length(unique(all.birds$spp))


all.birds[which(all.birds$spp == "NULL"),] #### NULL

nrow(all.birds[which(all.birds$spp == "NULL"),]) ## 69276 observations are NULL, do we care??? 
#they all have COUnt ZERO? maybe this is it?

all.birds %>% 
  group_by(spp, count) %>% 
  filter(count ==0) %>% 
  summarise(
    min = min(count, rm.na = TRUE),
    max = max(count, rm.na = TRUE),
    aver = mean(count, rm.na = TRUE),
    tot = n()
  ) %>% 
  View()



bird.names2 #Files_1991_Present_Mean90CI_rds

all.the.birds <- unique(all.birds$spp)

not.modelled <- setdiff(all.the.birds, bird.names2)

modelled <- tibble(all.the.birds) %>% 
  filter(!all.the.birds %in% not.modelled) %>% pull(all.the.birds)


## BEWARE to switch from modelled to not modelled vector, respectively
## change name TOO IN GGSAVE!!!!!!!
four_plots <- list()

#to save the plots!
for (i in sort(all.the.birds)) {
  onespp <- all.birds %>% 
    filter(spp == i)
  
  p <- ggplot(onespp)+
    geom_histogram(aes(count))+
    theme_bw()+
    ggtitle(i)+
    xlab(NULL)

  four_plots[[i]] <- p
}

plot_chunks <- split(four_plots, rep(1:ceiling(length(four_plots)/20), each = 20, length.out = length(four_plots)))

for (i in seq_along(plot_chunks)) {
  
  p<-cowplot::plot_grid(plotlist = plot_chunks[[i]], nrow = 4, ncol =5)
  
  print(p)
  
  #ggsave(paste0("plots/model_bird_plots_",i,".png"), p, units = 'in', width = 17, height = 9)
}



### to viz the plots more interactively


sample %>% 
  #select(spp, count) %>% ### need to select the specific columns if not give an error for breaks... not sure, send an issue?
  filter(!is.na(spp)) %>% 
  filter(!spp == "NULL") %>%
  #filter(spp %in% unique(all.birds$spp)) %>% 
  ggplot() +
  geom_histogram(aes(count))+
  theme_bw()+
  #xlim(1948, 2011) + ylim(10, 95) +
  facet_trelliscope(~ spp, nrow = 2, ncol = 4)###

unique(sample$spp)


sample<- sample %>%
  select(!X) %>% 
  mutate(
    spp = case_when( spp == "LALO" ~ "A",
                     spp == "SOSP" ~ "B",
                     spp == "CORA" ~ "C",
                     spp == "SAVS" ~ "C"),
    spp = case_when( spp == "LALO" ~ "A",
                     spp == "SOSP" ~ "B",
                     spp == "CORA" ~ "C",
                     spp == "SAVS" ~ "C")
  )
head(sample)
length(unique(sample$LocationID))

g <- sample(1:28, 28, replace=F)

sample %>% 
  #select(spp, count) %>% ### need to select the specific columns if not give an error for breaks... not sure, send an issue?
  filter(!is.na(spp)) %>% 
  filter(!spp == "NULL") %>%
  #filter(spp %in% unique(all.birds$spp)) %>% 
  ggplot() +
  geom_histogram(aes(count))+
  theme_bw()+
  #xlim(1948, 2011) + ylim(10, 95) +
  facet_wrap(~ spp, nrow = 2, ncol = 4, scales = "free")###

###################################################################
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

############ understanding file types

#Files_1991_Future_Mean90CI_rds

test1 <- readRDS(file.choose()) #ALFLCNRMESM21__26_2071.rds
class(test1) #"data.table" "data.frame"
nrow(test1)
#2116556

head(test1)
#X       Y   Mean Upper90_CI Lower90_CI
#1 -2617500 3385500 0.0561     0.1226     0.0106
#2 -2616500 3385500 0.0649     0.1140     0.0120
#3 -2615500 3385500 0.0643     0.1299     0.0067
#4 -2614500 3385500 0.0632     0.1190     0.0125
#5 -2613500 3385500 0.0542     0.1169     0.0047
#6 -2612500 3385500 0.0430     0.1120     0.0019

#Files_1991_Present_Mean90CI_rds
test1 <- readRDS(file.choose()) #ALFL1991NormPresent.rds
class(test1) #"data.frame"
nrow(test1)
#3605095

head(test1)
# X       Y   Mean  UPR90 LWR90
#3335 -2249500 3784500 0.0182 0.0517 0e+00
#3336 -2248500 3784500 0.0167 0.0503 0e+00
#3337 -2247500 3784500 0.0403 0.1136 5e-04
#5727 -2250500 3783500 0.0159 0.0441 0e+00
#5728 -2249500 3783500 0.0179 0.0481 0e+00
#5729 -2248500 3783500 0.0157 0.0441 0e+00

#Plots_1991_Normal_Present Distributions

#Plots_1991_Normal_Refugia --> Refugia, future suit, suit x refugia

#Rasters_1991_Normal_Refugia and Habitat Suitability - tif files, individually for Refugia, Suit, and SuitxRef

