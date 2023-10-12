###############################################################################
###   Part A. Clean PC data get density estimates using QPAD offsets (line 457)
###
###   see:  https://github.com/borealbirds/qpad-offsets
###   Using default static rasters rather than annual patch
###
###                   Anna Drake  Dec 2, 2022
####################################################################################################
####################################################################################################

### 12 files needed: "KeyUpdated.csv
### "WaterbirdSppCode.csv", "Spp_MasterList_YK&BC.csv", "CWS_WH_LANDBIRD_PCS_2017-2019_Tidy.csv",
### "CWS_WH_LANDBIRD_WILDTRAX_OUTPUT.csv", "BBS2BAM_data_output/bbs_ptCount.csv", "BBS2BAM_data_output/bbs_visit.csv",
### "BBS2BAM_data_output/bbs_XY.csv","BC_BBA_PCs.csv","New_QPAD_LLCData.csv", and Kathy Martin's central BC mountain survey data: "BCMnts_KM_CountTotals.csv","BC_MNT_KM_ARU.csv"

### Cleaning file output: "Cleaned_CWS_BMP_landbirds.csv", "CWS_Richness_TimeInterval_w.missing.rec.csv",
### "Cleaned_CWS_ARU_landbirds.csv", "Richness_CWS_ARU.w.missing.site.csv","BBS_year_restricted_clean_BCdat.csv",
### "Richness_BBS.csv","BC_BBA_clean_BCDataInc.csv", "Richness_BC_BBA.csv","BCData_Addition_March27.csv", "SppListOnePer.csv"

### QPAD file output: "AllRecordsForQPAD.csv","ModelingSubset_SppMasterList.csv"
### + species specific "*Spp*_Data.csv"

############################################################################

# add function
`%notin%` <- Negate(`%in%`)

########### Install QPAD ##################
## offsets

#if (!requireNamespace("QPAD")) {
#  if (!requireNamespace("remotes"))
#  install.packages("remotes")
#  remotes::install_github("psolymos/QPAD")

library(QPAD)
library(maptools)
library(intrval)
library(raster)
library(sp)

  load_BAM_QPAD(version = 3)
if (getBAMversion() != "3")
  stop("This script requires BAM version 3")
source("functions.R")


## load other packages
require(doBy)
library(rgdal)
library(dplyr)

setwd('/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main')

############################################################
##### PART 1. CLEAN DATASETS + GET RICHNESS OUTPUT
############################################################

### Files for filtering datasets
KEY<-read.csv("KeyUpdated.csv") #6024 sites

# Note 1: BC forest district data is currently omitted
# Note 2: Updated Key has appended the following YK CWS missing survey locations (see below, under CWS PC data)
#MissingCWS<-data.frame(MGHID=c(6019:6024),
#location_name=c("359586-NB4","323947-ND7","325590-NC10","325590-NC11","325590-NC12","YTBMS-KLP-325590-C10"), 
#Longitude=c(-138.699,-140.38453,-140.523228,-140.517268,-140.510085,-140.523228),
#Latitude=c(63.82772,61.94958,61.987613,61.987987,61.988393,61.987613),
#Program=c(rep("CWS_PC",5),"CWS_ARU"))

### Records for filtering data
waterbird<-read.csv("WaterbirdSppCode.csv") %>% as.list(.['Spp']) # YK waterbirds to exclude
waterbird<-waterbird$Spp
Masterlist<-read.csv("Spp_MasterList_YK&BC.csv") #Known breeders in the region
Masterlist<-Masterlist$Species_ID  #237 species listed

########################################################
#### 1 a) CWS PC - summary and cleaning - up to 2020 ###
########################################################

## CWS file was cleaned and where date/times (3 PCS) unknown, midpoint of range in comments used, or 5 min before cited time when "before X time was noted" - 1 case)
## Two false data entries also removed

CWS_PC<-read.csv("PC_Inputs/CWS_WH_LANDBIRD_PCS_2017-2019_Tidy.csv")
subset(CWS_PC,CWS_PC$loc_name=="Canol - A1")
subset(CWS_PC,CWS_PC$loc_name=="357452-B7")

### Add in 5 missing locations
CWS_PC$location.latitude<-ifelse(CWS_PC$loc_name=="359586-NB4",63.82772,CWS_PC$location.latitude) #this location was not accessible - likely PC was on other side of river but unclear actual location. Habitat looks similar, minor difference in aspect.
CWS_PC$location.latitude<-ifelse(CWS_PC$loc_name=="323947-ND7",61.94958,CWS_PC$location.latitude)  # originally 61.94894 but in the middle of a lake - est. based on notes. Shifted 100m back toward the road.
CWS_PC$location.latitude<-ifelse(CWS_PC$loc_name=="325590-NC10",61.987613,CWS_PC$location.latitude)
CWS_PC$location.latitude<-ifelse(CWS_PC$loc_name=="325590-NC11",61.987987,CWS_PC$location.latitude)
CWS_PC$location.latitude<-ifelse(CWS_PC$loc_name=="325590-NC12",61.988393,CWS_PC$location.latitude)

CWS_PC$location.longitude<-ifelse(CWS_PC$loc_name=="359586-NB4",-138.699,CWS_PC$location.longitude) # this location was not accessable - likely on other side of river but unclear actual locaiton. Habitat likely the same.
CWS_PC$location.longitude<-ifelse(CWS_PC$loc_name=="323947-ND7",-140.38453,CWS_PC$location.longitude) # originally -140.386 but in the middle of a lake - est. based on notes. Shifted 100m back toward the road.
CWS_PC$location.longitude<-ifelse(CWS_PC$loc_name=="325590-NC10",-140.523228,CWS_PC$location.longitude)
CWS_PC$location.longitude<-ifelse(CWS_PC$loc_name=="325590-NC11",-140.517268,CWS_PC$location.longitude)
CWS_PC$location.longitude<-ifelse(CWS_PC$loc_name=="325590-NC12",-140.510085,CWS_PC$location.longitude)

### Omit waterbirds
CWS_PC$species_id<-ifelse(CWS_PC$species_id %in% waterbird,"NULL", CWS_PC$species_id)
CWS_PC$species_id<-ifelse(CWS_PC$species_id == "","NULL", CWS_PC$species_id) # 3 blanks
CWS_PC<-unique(CWS_PC) # reduce to a single NULL record per survey
unique(CWS_PC$species_id) # 124 spp. 

### Split the data set by matched records in the Masterlist
CWSfine<-subset(CWS_PC, CWS_PC$species_id %in% Masterlist)
unique(CWSfine$species_id)# 

CWSchk<-subset(CWS_PC, CWS_PC$species_id %notin% Masterlist)
unique(CWSchk$species_id)# odd IDs

### Remove uncertain IDs, mammals, and nothing - don't delete as may lose point count locations where no focal species seen
remove<-c("SWAL","RAPT","WARB",'UNKN',"LONG","FLYC","FINC","WOOD","SFAL","UTBB","SPAR","SPPA","THRU","OWL","PASS","SCAU","SSHO","MAMO","VUVU","CALU","TAHU","ALAM","MUNI","MACA","LYPA","MYRU","OCCO","NOOO")  #NOOO - nothing observed, OCCO - Collared Pika, LYPA - Canadian Lynx, MYRU - Northern Red-backed Vole, SSHO - Small Shorebird sp.
CWSchk$species_id<-ifelse(CWSchk$species_id %in% remove,"NULL", CWSchk$species_id)
CWSchk<-unique(CWSchk) # reduce to a single NULL record per survey

### Fix species codes
CWSchk$species_id<-ifelse(CWSchk$species_id=="GRAJ","CAJA",CWSchk$species_id) #Canada Jay
CWSchk$species_id<-ifelse(CWSchk$species_id=="YWAR","YEWA",CWSchk$species_id) #Yellow warbler
CWSchk$species_id<-ifelse(CWSchk$species_id=="TTWO","ATTW",CWSchk$species_id) #Three toed woodpecker
CWSchk$species_id<-ifelse(CWSchk$species_id=="PMWA","PAWA",CWSchk$species_id) #Western Palm Warbler 2 records

subset(CWSchk, CWSchk$species_id=="FSPS") # record of GCSP fledges - keep as one adult
CWSchk$species_id<-ifelse(CWSchk$species_id=="FSPS","GCSP",CWSchk$species_id) #correct to GCSP

CWSClean<-rbind(CWSchk,CWSfine)

########################################
# 1 b) Add 2022 CWS BMP data
########################################
CWSBMP22<-read.csv("PC_Inputs/BMP_PointCountData_2022_checked.csv")

### Omit waterbirds & blanks
CWSBMP22$species_code<-ifelse(CWSBMP22$species_code%in% waterbird,"NULL", CWSBMP22$species_code)
CWSBMP22$species_code<-ifelse(CWSBMP22$species_code == "","NULL", CWSBMP22$species_code) # 3 blanks
CWSBMP22<-unique(CWSBMP22) # reduce to a single NULL record per survey
unique(CWSBMP22$species_code) # 90 spp. 

### Split the data set by matched records in the Masterlist
BMP22fine<-subset(CWSBMP22, CWSBMP22$species_code %in% Masterlist)
unique(BMP22fine$species_code)# 

BMP22chk<-subset(CWSBMP22, CWSBMP22$species_code %notin% Masterlist)
unique(BMP22chk$species_code)# odd IDs

### Set uncertain IDs, mammals, and nothing to NULL - don't delete as may lose point count locations where no focal species seen
remove<-c("UNSH","TAMI","SWAL","RAPT","WARB",'UNKN',"LONG","FLYC","FINC","WOOD","SFAL","UTBB","SPAR","SPPA","THRU","OWL","PASS","SCAU","SSHO","MAMO","VUVU","CALU","TAHU","ALAM","MUNI","MACA","LYPA","MYRU","OCCO","NOOO","DUCK") # as above with "DUCK" "UNSH" "TAMI"
BMP22chk$species_code<-ifelse(BMP22chk$species_code %in% remove,"NULL", BMP22chk$species_code)
BMP22chk<-unique(BMP22chk) # reduce to a single NULL record per survey

# Make remaining codes conform
BMP22chk$species_code<-ifelse(BMP22chk$species_code=="GRAJ","CAJA",BMP22chk$species_code) #Canada Jay
BMP22chk$species_code<-ifelse(BMP22chk$species_code=="YWAR","YEWA",BMP22chk$species_code) #Yellow warbler
BMP22chk$species_code<-ifelse(BMP22chk$species_code=="TTWO","ATTW",BMP22chk$species_code) #Three toed woodpecker

### Line up pre-2022 file with 2022 file and merge
BMP22Clean<-rbind(BMP22fine,BMP22chk)
BMP22Clean<-BMP22Clean[,-c(9,13,30:32)]
BMP22Clean<-rename(BMP22Clean, location.longitude = point_x)
BMP22Clean<-rename(BMP22Clean, location.latitude = point_y)

# Get rid of columns missing from 2022 file
CWSClean<-rename(CWSClean, species_code = species_id)
CWSClean_Reduced<-CWSClean[,-c(1:2,4,8,13,22,31:35,37,40:41)]

#Merge data
CWS_BMP<-rbind(CWSClean_Reduced,BMP22Clean)
write.csv(CWS_BMP,"Cleaned_PCs/Cleaned_CWS_BMP_landbirds.csv")

#####################################################
## b) CWS ARU up to 2020 - summary and cleaning    ##
#####################################################

CWS_ARU<-read.csv("PC_Inputs/CWS_WH_LANDBIRD_WILDTRAX_OUTPUT.csv")
#View(subset(CWS_ARU,CWS_ARU$location=="EG-CANOL-S3"))

### add missing location
CWS_ARU$latitude<-ifelse(CWS_ARU$location=="YTBMS-KLP-325590-C10",61.987613,CWS_ARU$latitude)
CWS_ARU$longitude<-ifelse(CWS_ARU$location=="YTBMS-KLP-325590-C10",-140.523228,CWS_ARU$longitude)

### Remove everything but birds
CWS_ARU<-subset(CWS_ARU, CWS_ARU$species_class =="AVES")

### Omit waterbirds
CWS_ARU$species_code<-ifelse(CWS_ARU$species_code %in% waterbird,"NULL", CWS_ARU$species_code)
CWS_ARU<-unique(CWS_ARU) # reduce to a single NULL record per survey

### Split the data set by records in the Masterlist
CWS_ARUfine<-subset(CWS_ARU, CWS_ARU$species_code %in% Masterlist)
unique(CWS_ARUfine$species_code)# 

CWS_ARUchk<-subset(CWS_ARU, CWS_ARU$species_code %notin% Masterlist)
unique(CWS_ARUchk$species_code)# 

CWS_ARUchk$species_code<-ifelse(CWS_ARUchk$species_code=="GRAJ","CAJA",CWS_ARUchk$species_code) #Canada Jay

CWS_ARU_clean<-rbind(CWS_ARUchk,CWS_ARUfine)

## Assign SOGR as DUGR given this is YK PCs
CWS_ARU_clean$species_code<-ifelse(CWS_ARU_clean$species_code=="SOGR","DUGR",CWS_ARU_clean$species_code) #Sooty grouse should be Dusky given split?

## Add roadside designation
## CWS_ARU_clean$road<-"FALSE"

### Extract dates and time to columns
CWS_ARU_clean$Year<-substr(CWS_ARU_clean$recording_date,0,4)
CWS_ARU_clean$Month<-substr(CWS_ARU_clean$recording_date,6,7)
CWS_ARU_clean$Day<-substr(CWS_ARU_clean$recording_date,9,10)
CWS_ARU_clean$Hr<-substr(CWS_ARU_clean$recording_time,1,2)
CWS_ARU_clean$Hr<-gsub(":","",CWS_ARU_clean$Hr)

CWS_ARU_clean$Minute<-substr(CWS_ARU_clean$recording_time,3,5)
CWS_ARU_clean$Minute<-gsub(":","",CWS_ARU_clean$Minute)

## Write out cleaned file
write.csv(CWS_ARU_clean,"Cleaned_PCs/Cleaned_CWS_ARU_landbirds.csv")

###############################################
## c) BBS records - summary and cleaning    ###
###############################################
dir()
BBS<-read.csv("PC_Inputs/BBS2BAM_data_output/bbs_ptCount.csv")
BBS_Visit<-read.csv("PC_Inputs/BBS2BAM_data_output/bbs_visit.csv")

#geographic locations
BBS_XY<-read.csv("PC_Inputs/BBS2BAM_data_output/bbs_XY.csv")
BBS_XY<-BBS_XY[c(3:5)]
BBS_XY$key<-paste(BBS_XY$location_longitude,BBS_XY$location_latitude,sep="_")

#Get location labels 
require(stringr)
BBS$Location<-str_sub(BBS$PKEY,-22,-6)
Loc<-unique(BBS$Location)
BBSDat<-merge(BBS,BBS_Visit,by="PKEY")
BBSDat$Region<-substr(BBSDat$Location,1,5)

# Subset to focal region and expanded climate region
require(sf)
Region <- st_read('/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/SampleRegion/SamplingRegion.shp')
Region <- st_simplify(Region, dTolerance = 4000)
crs <- "+proj=longlat +datum=WGS84 +no_defs"
Region <- st_transform(Region, crs)

# Isolate the BBS locations within sample region
xy<-BBS_XY[2:3] 
coordinates(xy)<- ~ location_longitude + location_latitude
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn back into a sf object
xyclim <- xy[Region, ] # subset with the shape file

RegionPts<-as.data.frame(st_coordinates(xyclim))
RegionPts$key<-paste(RegionPts$X,RegionPts$Y, sep="_")

### Subset BBS points spatially
BBS_XY<-subset(BBS_XY,BBS_XY$key %in% RegionPts$key) #14165 coordinates

BBSDat<-merge(BBSDat, BBS_XY, by.x="Location", by.y="location_name",all.y=TRUE, all.x=FALSE)
summary(BBSDat)
## Subset to years that overlap with BBA and CWS surveys? No - 105 sites lost within Ecozone if doing this
Years<-c(2001:2019) #min of other surveys, max available for BBS
BBS3<-subset(BBSDat,BBSDat$pc_visit_survey_year %in% Years)
Locations<-unique(BBS3[30:32])
#13130 locations
write.csv(Locations,"BBS_locations.csv")

# Omit waterbirds - retain PCs
BBS3$pc_species<-ifelse(BBS3$pc_species %in% waterbird,"NULL",BBS3$pc_species)
BBS3<-unique(BBS3)

# Split the data set by species records in the Masterlist
BBS3fine<-subset(BBS3, BBS3$pc_species %in% Masterlist)
unique(BBS3fine$pc_species)# 

BBS3chk<-subset(BBS3, BBS3$pc_species %notin% Masterlist)
unique(BBS3chk$pc_species)# 

### Change/drop unk species codes - retain PC locations

###Unknowns
Unknowns<-c("HAFI_UNI","POCH_UNI","TERN_UNI","ACCI_UNI","BUBA_UNI","WAXW_UNI","GULL_UNI","LOON_UNI","WOOD_UNI","SCAU_UNI","PEEP_UNI","REDP_UNI","CROS_UNI","GOLD_UNI","GRLY_UNI","NA","HDFL_UNI","PWWR_UNI","WECL_UNI","EPFL_UNI","SAPS_UNI","HUMM_UNI","BUTE_UNI") 
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species %in% Unknowns,"NULL",BBS3chk$pc_species)
BBS3chk$pc_species<-ifelse(is.na(BBS3chk$pc_species),"NULL",BBS3chk$pc_species) # drop NAs

### Drop southern/out of range/marine spp./RTHA
southern<-c("RTHA","SPTO","CAVI","BTNW","TRFL","LCSP","MAMU","PIGU","PECO","RHAU","BLOY","COFL","WEFL","LEWO","GRFL","WGWH","MUSW","HEGU","BDOW","NOSL","SAPS","NSTS","AMGP","BNST","SBIG","CASJ","DBBB","GRHE","HEEG","GBBG","WEGU","INPE","CAAU","EUBU","COMU","BRAC","LEGO","HASP","HASP","SOVI","BLGS","BAGO") #Western flycatcher (WEFL) could be Alder but assuming not to be cautious
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species %in% southern,"NULL",BBS3chk$pc_species)
BBS3chk<-unique(BBS3chk)

## Align other 4-letter codes
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="YSFL","NOFL",BBS3chk$pc_species) #Yellow shafted to Northern Flicker
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="RSFL","NOFL",BBS3chk$pc_species) #Red shafted to Northern Flicker
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="NFIN","NOFL",BBS3chk$pc_species) #Intergrade of yellow-red to Northern Flicker
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="SCJU","DEJU",BBS3chk$pc_species) #Slate-coloured Junco to DEJU
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="ORJU","DEJU",BBS3chk$pc_species) #Oregon Junco to DEJU
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="MYWA","YRWA",BBS3chk$pc_species) #Myrtle Warbler to Yellow-rump warbler 
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="AUWA","YRWA",BBS3chk$pc_species) #Merging Audubon's Warbler with Myrtle here
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="HALH","RTHA",BBS3chk$pc_species) #Harlan's Hawk subsp of Red-tailed Hawk
BBS3chk$pc_species<-ifelse(BBS3chk$pc_species=="NOCR","AMCR",BBS3chk$pc_species) #Northwestern crow to American crow

unique(BBS3chk$pc_species)

BBS_clean<-rbind(BBS3fine,BBS3chk) #790130 observations

##Back to QPAD directory
setwd('/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main')

### Extract dates and time to columns
BBS_clean$pc_visit_date <- as.Date(BBS_clean$pc_visit_date, "%d-%b-%y")
BBS_clean$Year<-format(BBS_clean$pc_visit_date, format = "%Y")
BBS_clean$Month<-format(BBS_clean$pc_visit_date, format = "%m")
BBS_clean$Day<-format(BBS_clean$pc_visit_date, format = "%d")
BBS_clean$Hr<-as.numeric(substr(BBS_clean$pc_visit_time,1,2))
BBS_clean$Minute<-as.numeric(substr(BBS_clean$pc_visit_time,4,5))
BBS_clean$Minute<-ifelse(as.numeric(substr(BBS_clean$pc_visit_time,7,8))>30,BBS_clean$Minute+1,BBS_clean$Minute) # round to nearest minute

write.csv(BBS_clean,"Cleaned_PCs/BBS_year_restricted_clean.csv")

###############################################
## d) BC BBA records - summary and cleaning ###
###############################################

BBA<-read.csv("PC_Inputs/Full_BC_BBA.csv") 
BBA$LocationID<-gsub("URN:catalog:BSC-EOC:BCATLAS1PC:","",BBA$GlobalUniqueIdentifier)
BBA$LocationID<-as.numeric(substr(BBA$LocationID,1,6))
names(BBA)
location<-unique(BBA[c(33,34,170)])
BBAcheck<-subset(BBA, BBA$LocationID==339889|BBA$LocationID==324887) #same location by year differences

# Add roadside data
# BBARoad<-read.csv("BCAtlasPCs.csv")
# BBARoad<-BBARoad[c(1,9)]
# BBA<-merge(BBA,BBARoad,by.x="ID",by.y="point_count_id")

BBA<-subset(BBA,!is.na(BBA$latitude))
BBA<-subset(BBA,!is.na(BBA$longitude)) #drop unknown locations
LocBBA<-BBA$LocationID
Sites<-unique(LocBBA) #36195 locations

# Isolate the BBA locations within sample region 
xy<-BBA[11:12]  
coordinates(xy)<- ~ longitude + latitude
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn back into a sf object
xyclim <- xy[Region, ] # subset with the shape file - only loose 5 count sites

BBAPts<-as.data.frame(st_coordinates(xyclim))
BBAPts$key<-paste(BBAPts$X,BBAPts$Y, sep="_")
write.csv(BBAPts,"BBA_locations.csv")

BBA$key<-paste(BBA$longitude,BBA$latitude, sep="_")
BBA3<-subset(BBA, BBA$key %in% BBAPts$key)

# Get species codes
BBA3$Spp<-substr(BBA3$CatalogNumber,8,11) #get 4-letter code

# Omit waterbirds
BBA3$Spp<-ifelse(BBA3$Spp %in% waterbird,"NULL",BBA3$Spp)

# Remove southern species and some stray marine/waterbirds and unknowns
southern<-c("SPTO","CAVI","BTNW","TRFL","LCSP","MAMU","PIGU","PECO","RHAU","BLOY","COFL","WEFL","LEWO","GRFL","WGWH","MUSW","HEGU","BDOW","NOSL","SAPS","NSTS","AMGP","BNST","SBIG","CASJ","DBBB","GRHE","HEEG","GBBG","WEGU","INPE","CAAU","EUBU","COMU","BRAC","LEGO","HASP","HASP","SOVI","BLGS","BAGO") #Western flycatcher (WEFL) could be Alder but assuming not to be cautious
BBA3$Spp<-ifelse(BBA3$Spp %in% southern,"NULL",BBA3$Spp)

BBA3<-unique(BBA3)

#lots of naming convention differences
BBA3$Spp<-ifelse(BBA3$Spp=="BASW","BARS",BBA3$Spp) #Barn Swallow
BBA3$Spp<-ifelse(BBA3$Spp=="BKSW","BANS",BBA3$Spp) #Bank Swallow
BBA3$Spp<-ifelse(BBA3$Spp=="TRSW","TRES",BBA3$Spp) #Tree Swallow
BBA3$Spp<-ifelse(BBA3$Spp=="GRJA","CAJA",BBA3$Spp) #Gray Jay
BBA3$Spp<-ifelse(BBA3$Spp=="WWPE","WEWP",BBA3$Spp) #Western Wood-Pewee
BBA3$Spp<-ifelse(BBA3$Spp=="MACW","MGWA",BBA3$Spp) #MacGillivray's Warbler
BBA3$Spp<-ifelse(BBA3$Spp=="NOSH","NSHR",BBA3$Spp) #Northern Shrike
BBA3$Spp<-ifelse(BBA3$Spp=="BKPW","BLPW",BBA3$Spp) #Blackpoll warbler
BBA3$Spp<-ifelse(BBA3$Spp=="MGNW","MAWA",BBA3$Spp) #Magnolia warbler
BBA3$Spp<-ifelse(BBA3$Spp=="CEWA","CEDW",BBA3$Spp) #Cedar Waxwing
BBA3$Spp<-ifelse(BBA3$Spp=="NOCR","AMCR",BBA3$Spp) #Northwestern Crow to American
BBA3$Spp<-ifelse(BBA3$Spp=="COWA","CONW",BBA3$Spp) #Connecticut Warbler
BBA3$Spp<-ifelse(BBA3$Spp=="BAYW","BBWA",BBA3$Spp) #Bay breasted Warbler
BBA3$Spp<-ifelse(BBA3$Spp=="EWWR","WIWR",BBA3$Spp) #Winter Wren/Pacific Wren
BBA3$Spp<-ifelse(BBA3$Spp=="NPOW","NOPO",BBA3$Spp) #


# Split the data set by species records in the Masterlist
BBA3fine<-subset(BBA3, BBA3$Spp %in% Masterlist)
unique(BBA3fine$Spp)# 

BBA3chk<-subset(BBA3, BBA3$Spp %notin% Masterlist)
unique(BBA3chk$Spp)# 12 different codes
BBA3chk$Spp<-"NULL"
BBA3chk<-unique(BBA3chk)

## Notes on BTBW and CAVI - seen in BBA surveys within Cordillera
# BTNW == Black-throated Green Warbler in focal Ecozones. Seems out of range - error transcribing/IDing? Though some records this far west in ebird.
# not ever seen with Townsend's though these are within the surrounding survey blocks - I would guess that these are actually TOWA
# If they are a mistaken ID it will still be fine for richness estimates. 
# Could be problem for abundance models if this is actually TOWA but few records

## CAVI	== Cassin's Vireo. Seems well out of range, though some northern records on ebird.
# Only ever seen with V. gilvus which isn't similar. 
# If they are a mistaken ID it will still count as another vireo, 
# so keep for richness estimates. Could be problem in abundance models 
# if this is actually a different, and modeled species

BC_BBA_clean<-rbind(BBA3fine,BBA3chk) #82350 observations

### Extract time to columns
BC_BBA_clean$Hr<-as.numeric(substr(BC_BBA_clean$TimeCollected,1,2))
summary(BC_BBA_clean$Hr)
BC_BBA_clean$Min<-as.numeric(substr(BC_BBA_clean$TimeCollected,2,7)) # whole hours coming out as NA
BC_BBA_clean$Min<-ifelse(is.na(BC_BBA_clean$Min),0,BC_BBA_clean$Min)
BC_BBA_clean$Min<-round(BC_BBA_clean$Min*60,0)

# write out file
write.csv(BC_BBA_clean,"Cleaned_PCs/BC_BBA_AllSpp.csv")

##################################################
## e) Kathy Martin's BC Mountain Bird PC data  ###
##################################################

BCMntKM<-read.csv("PC_Inputs/BCMnts_KM_CountTotals.csv")  
BCMntKM<-BCMntKM[c(2:3,8:9,11,14:84)]

library(tidyr)
BCMntKMLong <- gather(BCMntKM, spp, count, GCRF:BAGO, factor_key=TRUE)

BCMntLoc<-read.csv("PC_Inputs/BCData_Addition_MntBirds.csv")
BCMntLoc<-subset(BCMntLoc,BCMntLoc$Source=="MntBird_KM")
BCMntKM<-merge(BCMntKMLong,BCMntLoc,by.x="PC",by.y="LocationID")

# Omit waterbirds
BCMntKM$spp<-ifelse(BCMntKM$spp %in% waterbird, "NULL", as.character(BCMntKM$spp)) #71742

### Remove southern species and some stray marine/waterbirds and unknowns
southern<-c("SPTO","CAVI","BTNW","TRFL","LCSP","MAMU","PIGU","PECO","RHAU","BLOY","COFL","WEFL","LEWO","GRFL","WGWH","MUSW","HEGU","BDOW","NOSL","SAPS","NSTS","AMGP","BNST","SBIG","CASJ","DBBB","GRHE","HEEG","GBBG","WEGU","INPE","CAAU","EUBU","COMU","BRAC","LEGO","HASP","HASP","SOVI","BLGS","BAGO") #Western flycatcher (WEFL) could be Alder but assuming not to be cautious

BCMntKM<-unique(BCMntKM) #60583 records

# Split the data set by species records in the Masterlist
BCMntKMfine<-subset(BCMntKM, BCMntKM$spp %in% Masterlist)
unique(BCMntKMfine$spp)# 

BCMntKMchk<-subset(BCMntKM, BCMntKM$spp %notin% Masterlist)
unique(BCMntKMchk$spp)
BCMntKM$spp<-ifelse(BCMntKM$spp=="GRAJ","CAJA",as.character(BCMntKM$spp))

#Add roadside label
#BCMntKM$road<-"FALSE"
# Join files
BCMntKM<-rbind(BCMntKMfine,BCMntKMchk)

### Extract time to columns
BCMntKM$Hr<-as.numeric(substr(BCMntKM$Time,1,2))

BCMntKM$Min<-as.numeric(substr(BCMntKM$Time,2,4)) # whole hours coming out as NA
BCMntKM$Min<-ifelse(is.na(BCMntKM$Min),0,BCMntKM$Min)
BCMntKM$Min<-round(BCMntKM$Min*60,0)

# write out
write.csv(BCMntKM,"Cleaned_PCs/BC_MountainData_KM_CleanedPC.csv")

######################################################
#### f) Kathy Martin's BC Mountain bird ARU data #####
######################################################

BCMntARU<-read.csv("PC_Inputs/BC_MNT_KM_ARU.csv")  
BCMntARU<-BCMntARU[c(5,8,11:13,15:18)]

BCMntARU<-merge(BCMntARU,BCMntLoc,by.x="PC",by.y="LocationID")
BCMntARU$Source<-"BCMntKM_ARU"

# Omit waterbirds
BCMntARU$SPECIES_CODE<-ifelse(BCMntARU$SPECIES_CODE %in% waterbird, "NULL", as.character(BCMntARU$SPECIES_CODE)) #5447

### Remove southern species and some stray marine/waterbirds and unknowns
BCMntARU$SPECIES_CODE<-ifelse(BCMntARU$SPECIES_CODE %in% southern, "NULL", as.character(BCMntARU$SPECIES_CODE)) #5447

# Split the data set by species records in the Masterlist
BCMntARUfine<-subset(BCMntARU, BCMntARU$SPECIES_CODE %in% Masterlist)
unique(BCMntARUfine$SPECIES_CODE)# 

BCMntARUchk<-subset(BCMntARU, BCMntARU$SPECIES_CODE %notin% Masterlist)
unique(BCMntKMchk$SPECIES_CODE)# all fine

BCMntARU<-rbind(BCMntARUfine,BCMntARUchk)

### Extract time to columns
BCMntARU$Hr<-as.numeric(substr(BCMntARU$Time,1,2))
BCMntARU$Min<-as.numeric(substr(BCMntARU$Time,2,4)) # whole hrs coming out as NA
BCMntARU$Min<-ifelse(is.na(BCMntARU$Min),0,BCMntARU$Min)
BCMntARU$Min<-round(BCMntARU$Min*60,0)

write.csv(BCMntARU,"Cleaned_PCs/BC_MountainData_KM_CleanedPC_ARU.csv")

#############################################
# f) eBird stationary counts BC, YK, and AB
#############################################

SpeciesCodes <- read.csv("PC_Inputs/SpeciesCodeLookup.csv")

# Import stationary counts
eBirdYK<-read.csv("PC_Inputs/ebd_CA-YT_relMay-2022.txt", 
fill=TRUE, header=TRUE, quote = "", sep="\t", encoding="UTF-8") %>% 
  subset(PROTOCOL.TYPE=="Stationary") %>%
  subset(ALL.SPECIES.REPORTED==1)  %>% # where all species at site were recorded
  subset(DURATION.MINUTES<11) # and duration was less than 11 min (Lionel Leston's cutoff)

# Get dates and times
eBirdYK$YR<-as.numeric(as.character(substr(eBirdYK$OBSERVATION.DATE,1,4))) 
eBirdYK$MO<-as.numeric(as.character(substr(eBirdYK$OBSERVATION.DATE,6,7))) 
eBirdYK$HR<-substr(eBirdYK$TIME.OBSERVATIONS.STARTED,1,2)
eBirdYK$HR<- as.numeric(gsub(":","", eBirdYK$HR))

#May-July, 2007-Present, earlier than 10AM
eBirdYK_subset<-subset(eBirdYK,eBirdYK$YR>2006 & eBirdYK$MO>4 & eBirdYK$MO<8 & eBirdYK$HR>4 & eBirdYK$HR<10) #70193

## correct some scientific names

eBirdYK_subset$SCIENTIFIC.NAME<-ifelse(eBirdYK_subset$SCIENTIFIC.NAME=="Leiothlypis celata","Leiothylpis celata", ifelse(eBirdYK_subset$SCIENTIFIC.NAME=="Leiothlypis peregrina","Leiothylpis peregrina",eBirdYK_subset$SCIENTIFIC.NAME))

eBirdYK_subset<-merge(eBirdYK_subset,SpeciesCodes, by.x="SCIENTIFIC.NAME", 
               by.y="scientific_name", all.x=TRUE,all.y=FALSE) #71225  records

check<-subset(eBirdYK_subset,is.na(eBirdYK_subset$species_code))  
unique(check$COMMON.NAME) #mostly uncertain IDs
#subset(check,check$COMMON.NAME=="Orange-crowned Warbler") # bunch of records, Leiothlypis celata - corrected above now
#subset(check,check$COMMON.NAME=="Tennessee Warbler") #Leiothlypis peregrina 
eBirdYK_subset<-subset(eBirdYK_subset, !is.na(eBirdYK_subset$species_code)) #remove uncertain IDs 16751
names(eBirdYK_subset)
eBirdYK_subset<-eBirdYK_subset[c(2,7,1,11,29:38,51:53,55,48)]

## Bring in AB data
eBirdAB<-read.delim("PC_Inputs/ebd_CA-AB_200701_202206_relMay-2022.txt", 
                    fill=TRUE, header=TRUE, quote = "", sep="\t", encoding="UTF-8") %>% 
  subset(PROTOCOL.TYPE=="Stationary") %>%
  subset(ALL.SPECIES.REPORTED==1)  %>% # where all species at site were recorded
  subset(DURATION.MINUTES<11)

# Get dates and times
eBirdAB$YR<-as.numeric(as.character(substr(eBirdAB$OBSERVATION.DATE,1,4))) 
eBirdAB$MO<-as.numeric(as.character(substr(eBirdAB$OBSERVATION.DATE,6,7))) 
eBirdAB$HR<-substr(eBirdAB$TIME.OBSERVATIONS.STARTED,1,2)
eBirdAB$HR<- as.numeric(gsub(":","", eBirdAB$HR))

#May-July, 2007-Present, earlier than 10AM
eBirdAB_subset<-subset(eBirdAB, eBirdAB$MO>4 & eBirdAB$MO<8 & eBirdAB$HR>4 & eBirdAB$HR<10) #70193

## correct some scientific names

eBirdAB_subset$SCIENTIFIC.NAME<-ifelse(eBirdAB_subset$SCIENTIFIC.NAME=="Leiothlypis celata","Leiothylpis celata", ifelse(eBirdAB_subset$SCIENTIFIC.NAME=="Leiothlypis peregrina","Leiothylpis peregrina", ifelse(eBirdAB_subset$SCIENTIFIC.NAME=="Leiothlypis ruficapillae","Leiothylpis ruficapillae", eBirdAB_subset$SCIENTIFIC.NAME)))

eBirdAB_subset<-merge(eBirdAB_subset,SpeciesCodes, by.x="SCIENTIFIC.NAME", 
                      by.y="scientific_name", all.x=TRUE,all.y=FALSE) 

check<-subset(eBirdAB_subset,is.na(eBirdAB_subset$species_code))  
unique(check$COMMON.NAME) #mostly uncertain IDs
#subset(check,check$COMMON.NAME=="Nashville Warbler") # bunch of records, Leiothlypis celata - corrected above now
#subset(check,check$COMMON.NAME=="Tennessee Warbler") #Leiothlypis peregrina 
eBirdAB_subset<-subset(eBirdAB_subset, !is.na(eBirdAB_subset$species_code)) #remove uncertain IDs 16751
eBirdAB_subset<-eBirdAB_subset[c(2,7,1,11,29:38,51:53,55,48)]

## Bring in BC data
eBirdBC<-read.delim("PC_Inputs/ebd_CA-BC_200701_202206_relMay-2022.txt", 
                    fill=TRUE, header=TRUE, quote = "", sep="\t", encoding="UTF-8") %>% 
  subset(PROTOCOL.TYPE=="Stationary") %>%
  subset(ALL.SPECIES.REPORTED==1)  %>% # where all species at site were recorded
  subset(DURATION.MINUTES<11)

# Get dates and times
eBirdBC$YR<-as.numeric(as.character(substr(eBirdBC$OBSERVATION.DATE,1,4))) 
eBirdBC$MO<-as.numeric(as.character(substr(eBirdBC$OBSERVATION.DATE,6,7))) 
eBirdBC$HR<-substr(eBirdBC$TIME.OBSERVATIONS.STARTED,1,2)
eBirdBC$HR<- as.numeric(gsub(":","", eBirdBC$HR))

#May-July, 2007-Present, earlier than 10AM
eBirdBC_subset<-subset(eBirdBC, eBirdBC$MO>4 & eBirdBC$MO<8 & eBirdBC$HR>4 & eBirdBC$HR<10) #70193

## correct some scientific names

eBirdBC_subset$SCIENTIFIC.NAME<-ifelse(eBirdBC_subset$SCIENTIFIC.NAME=="Leiothlypis celata","Leiothylpis celata", ifelse(eBirdBC_subset$SCIENTIFIC.NAME=="Leiothlypis peregrina","Leiothylpis peregrina", ifelse(eBirdBC_subset$SCIENTIFIC.NAME=="Leiothlypis ruficapillae","Leiothylpis ruficapillae", eBirdBC_subset$SCIENTIFIC.NAME)))

eBirdBC_subset<-merge(eBirdBC_subset,SpeciesCodes, by.x="SCIENTIFIC.NAME", 
                      by.y="scientific_name", all.x=TRUE,all.y=FALSE) #152428  records

check<-subset(eBirdBC_subset,is.na(eBirdBC_subset$species_code))  
unique(check$COMMON.NAME)  

eBirdBC_subset<-subset(eBirdBC_subset, !is.na(eBirdBC_subset$species_code)) #remove uncertain IDs 149604
summary(eBirdBC_subset) #24562 vs 149604
eBirdBC_subset<-eBirdBC_subset[c(2,7,1,11,29:38,51:53,55,48)]

### NWT data ######

## Bring in BC data
eBirdNT<-read.delim("PC_Inputs/ebd_CA-NT_200701_202206_relMay-2022.txt", 
                    fill=TRUE, header=TRUE, quote = "", sep="\t", encoding="UTF-8") %>% 
  subset(PROTOCOL.TYPE=="Stationary") %>%
  subset(ALL.SPECIES.REPORTED==1)  %>% # where all species at site were recorded
  subset(DURATION.MINUTES<11)

# Get dates and times
eBirdNT$YR<-as.numeric(as.character(substr(eBirdNT$OBSERVATION.DATE,1,4))) 
eBirdNT$MO<-as.numeric(as.character(substr(eBirdNT$OBSERVATION.DATE,6,7))) 
eBirdNT$HR<-substr(eBirdNT$TIME.OBSERVATIONS.STARTED,1,2)
eBirdNT$HR<- as.numeric(gsub(":","", eBirdNT$HR))

#May-July, 2007-Present, earlier than 10AM
eBirdNT_subset<-subset(eBirdNT, eBirdNT$MO>4 & eBirdNT$MO<8 & eBirdNT$HR>4 & eBirdNT$HR<10) #70193

## correct some scientific names

eBirdNT_subset$SCIENTIFIC.NAME<-ifelse(eBirdNT_subset$SCIENTIFIC.NAME=="Leiothlypis celata","Leiothylpis celata", ifelse(eBirdNT_subset$SCIENTIFIC.NAME=="Leiothlypis peregrina","Leiothylpis peregrina", ifelse(eBirdNT_subset$SCIENTIFIC.NAME=="Leiothlypis ruficapillae","Leiothylpis ruficapillae", eBirdNT_subset$SCIENTIFIC.NAME)))

eBirdNT_subset<-merge(eBirdNT_subset,SpeciesCodes, by.x="SCIENTIFIC.NAME", 
                      by.y="scientific_name", all.x=TRUE,all.y=FALSE) #152428  records

check<-subset(eBirdNT_subset,is.na(eBirdNT_subset$species_code))  
unique(check$COMMON.NAME)  # nothing that can't be dropped

eBirdNT_subset<-subset(eBirdNT_subset, !is.na(eBirdNT_subset$species_code)) #remove uncertain IDs 149604
names(eBirdNT_subset)
eBirdNT_subset<-eBirdNT_subset[c(2,7,1,11,29:38,51:53,55,48)]

#### Stack the 4 provinces

eBird<-rbind(eBirdAB_subset,eBirdYK_subset,eBirdBC_subset,eBirdNT_subset) #258918
eBird$key<-paste(eBird$LONGITUDE,eBird$LATITUDE, sep="_")

### remove points out of the sampling area
xy<-eBird[5:6]
coordinates(xy)<- ~ LONGITUDE + LATITUDE
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn into a sf object
xyclim <- xy[Region, ] # subset with the shape file

RegionPts<-as.data.frame(st_coordinates(xyclim))
RegionPts$key<-paste(RegionPts$X,RegionPts$Y, sep="_")
write.csv(RegionPts, "eBirdlocations.csv")
### Subset eBird points 
eBird_clean<-subset(eBird,eBird$key %in% RegionPts$key) #235314 coordinates

### remove waterbirds, southern, blanks
eBird_clean$species_code<-ifelse(eBird_clean$species_code %in% waterbird,"NULL", eBird_clean$species_code)
eBird_clean$species_code<-ifelse(eBird_clean$species_code == "","NULL", eBird_clean$species_code) 
eBird_clean$species_code<-ifelse(eBird_clean$species_code %in% southern, "NULL", as.character(eBird_clean$species_code)) #71742

# Split the data set by species records in the Masterlist
eBirdfine<-subset(eBird_clean, eBird_clean$species_code %in% Masterlist)
unique(eBirdfine$species_code)# 

eBirdchk<-subset(eBird_clean, eBird_clean$species_code %notin% Masterlist)
unique(eBirdchk[c(2,18)])#
unique(eBirdchk$species_code)
eBirdchk$species_code<-ifelse(eBirdchk$species_code=="GRAJ","CAJA",as.character(eBirdchk$species_code))

eBird_clean<-rbind(eBirdchk,eBirdfine)
eBird_clean<-unique(eBird_clean) #229167
summary(eBird)

write.csv(eBird_clean,"Cleaned_PCs/eBird_clean_20072022.csv")

################################################
# g) Big Grids, Jasper NP and Wood Buffalo ARUs
#################################################
BigGrid<-read.csv("PC_Inputs/BU_Big_Grids_basic_summary.csv")
JNP<-read.csv("PC_Inputs/JNP_Jasper_National_Park_2017_basic_summary.csv")
WBF<-read.csv("PC_Inputs/BU_General-Community-WoodBuffaloNationalPark-2018_basic_summary.csv")

ARUProj<-rbind(BigGrid,JNP,WBF)
unique(ARUProj$method) # note different methods
ARUProj$key<-paste(ARUProj$longitude,ARUProj$latitude, sep="_")

### remove points out of the sampling area
xy<-ARUProj[5:6]
coordinates(xy)<- ~ longitude + latitude
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn into a sf object
xyclim <- xy[Region, ] # subset with the shape file

RegionPts<-as.data.frame(st_coordinates(xyclim))
RegionPts$key<-paste(RegionPts$X,RegionPts$Y, sep="_")
write.csv(RegionPts, "ARU_JNP_BUF_BG_locations.csv")

### Subset ARU points 
ARUProj_subset<-subset(ARUProj,ARUProj$key %in% ARUProj$key) #235314 coordinates

### remove waterbirds, southern, blanks
ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code %in% waterbird,"NULL", ARUProj_subset$species_code)
ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code == "","NULL", ARUProj_subset$species_code) 
ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code %in% southern, "NULL", as.character(ARUProj_subset$species_code)) #71742

# Split the data set by species records in the Masterlist

## fix known naming issues

ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code=="GRAJ","CAJA",as.character(ARUProj_subset$species_code))
ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code=="AUWA","YRWA",ARUProj_subset$species_code)
ARUProj_subset$species_code<-ifelse(ARUProj_subset$species_code=="MYWA","YRWA",ARUProj_subset$species_code)

ARUProjfine<-subset(ARUProj_subset, ARUProj_subset$species_code %in% Masterlist)
unique(ARUProjfine$species_code)# 

ARUProjchk<-subset(ARUProj_subset, ARUProj_subset$species_code %notin% Masterlist)
subset<-subset(ARUProjchk,ARUProjchk[9]!="NULL")
unique(subset[c(9,10)])# all can be removed
ARUProjchk$species_code<-"NULL"

ARUProj_clean<-rbind(ARUProjchk,ARUProjfine)
ARUProj_clean<-unique(ARUProj_clean) # get rid of duplicate "NULL" entries 
ARUProj_clean$dur<-ifelse(ARUProj_clean$method=="3m 1SPM",3,ifelse(ARUProj_clean$method=="3m1SPM+7mVS",10,ifelse(ARUProj_clean$method=="9m 1SPM",9,ifelse(ARUProj_clean$method=="10m 1SPM",10,ifelse(ARUProj_clean$method=="1m 1SPM",1,ifelse(ARUProj_clean$method=="2m 1SPM",2,"unk"))))))
unique(ARUProj_clean$method)

write.csv(ARUProj_clean,"Cleaned_PCs/ARU_WBF_JNP_BG.csv")

##########################################
# h) additional point counts: Fort Liard Linear Feature
#    & NWT seismic line recovery project
##########################################

SeismicLine<-read.csv("PC_Inputs/BU_NWT_Seismic_Line_Recovery_Project_2010_report.csv")
FtLiard<-read.csv("PC_Inputs/BU_Fort_Liard_Linear_Feature_Surveys_2008_report.csv")
SeismicLines<-rbind(SeismicLine,FtLiard) 
unique(SeismicLines$durationInterval)  #Inf distance and 10min
SeismicLines$key<-paste(SeismicLines$longitude,SeismicLines$latitude, sep="_")

### remove points out of the sampling area
xy<-SeismicLines[4:5]
coordinates(xy)<- ~ longitude + latitude
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn into a sf object
xyclim <- xy[Region, ] # subset with the shape file

RegionPts<-as.data.frame(st_coordinates(xyclim))
RegionPts$key<-paste(RegionPts$X,RegionPts$Y, sep="_")
write.csv(RegionPts, "SeismicLinesPC_locations.csv")

### Subset ARU points 
SeismicLines_subset<-subset(SeismicLines,SeismicLines$key %in% SeismicLines$key) #3639 coordinates

### remove waterbirds, southern, blanks
SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode %in% waterbird,"NULL", SeismicLines_subset$speciesCode)
SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode == "","NULL", SeismicLines_subset$speciesCode) 
SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode %in% southern, "NULL", as.character(SeismicLines_subset$speciesCode)) 

# Split the data set by species records in the Masterlist

## fix known naming issues

SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode=="GRAJ","CAJA",as.character(SeismicLines_subset$speciesCode))
SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode=="AUWA","YRWA",SeismicLines_subset$speciesCode)
SeismicLines_subset$speciesCode<-ifelse(SeismicLines_subset$speciesCode=="MYWA","YRWA",SeismicLines_subset$speciesCode)

SeismicLines_fine<-subset(SeismicLines_subset, SeismicLines_subset$speciesCode %in% Masterlist)
unique(SeismicLines_fine$speciesCode)# 

SeismicLines_ck<-subset(SeismicLines_subset, SeismicLines_subset$speciesCode %notin% Masterlist)
unique(SeismicLines_ck[c(9,10)])# all can be removed
SeismicLines_ck$speciesCode<-"NULL"


SeismicLines_clean<-rbind(SeismicLines_fine,SeismicLines_ck)
SeismicLines_clean<-unique(SeismicLines_clean) # get rid of duplicate "NULL" entries 
SeismicLines_clean<-subset(SeismicLines_clean,SeismicLines_clean$durationInterval!="UNKNOWN")

write.csv(SeismicLines_clean,"Cleaned_PCs/SeismicLines.csv")

###########################################
##(i) USGS Alaskan Data
###########################################

Alaska<-read.csv("PC_Inputs/BAMv6_AKALMS_DATA.csv")
unique(Alaska$durationMethod)
unique(Alaska$distanceMethod)

# different ecozone classification - may as well use all data..
read.csv("distance_method_codes.csv")
read.csv("duration_method_codes.csv")

Alaska$dur<-ifelse(Alaska$durationMethod==13,8,ifelse(Alaska$durationMethod==2,5,ifelse(Alaska$durationMethod==14|Alaska$durationMethod==1,10,ifelse(Alaska$durationMethod==9,"UNK","UNK"))))
Alaska$dis<-ifelse(Alaska$distanceMethod==19,"Inf",ifelse(Alaska$durationMethod==3,"Inf","UNK"))
unique(Alaska$date)

### remove waterbirds, southern/other, blanks
Alaska$speciesCode<-ifelse(Alaska$speciesCode %in% waterbird,"NULL", Alaska$speciesCode)
Alaska$speciesCode<-ifelse(Alaska$speciesCode == "","NULL", Alaska$speciesCode) 
Alaska$speciesCode<-ifelse(Alaska$speciesCode %in% southern,"NULL", Alaska$speciesCode)

Alaska$speciesCode<-ifelse(Alaska$speciesCode=="NOCR","AMCR",Alaska$speciesCode) 

# Split the data set by species records in the Masterlist

Alaskafine<-subset(Alaska, Alaska$speciesCode %in% Masterlist)
unique(Alaskafine$speciesCode)# 

Alaskachk<-subset(Alaska, Alaska$speciesCode %notin% Masterlist)
Alaskachk$speciesCode<-"NULL"
Alaskachk<-unique(Alaskachk)
#ROSA,RFCO,TUPU,LESP,BARG,PAAU,ALTE,LSAP,SNOW,LEAU,BTGO,ARWA,EYWA,BLUE,CORO,BRAM - all can be removed

Alaska<-rbind(Alaskafine,Alaskachk)

write.csv(Alaska,"Cleaned_PCs/AlaskaData.csv")


###############################
# (j) CWS Northwest Territories
###############################
CWS_NWT<-read.csv("PC_Inputs/NTBMS2019.csv")

CWS_NWT$speciesCode<-ifelse(CWS_NWT$speciesCode %in% waterbird,"NULL", CWS_NWT$speciesCode)
CWS_NWT$speciesCode<-ifelse(CWS_NWT$speciesCode == "","NULL", CWS_NWT$speciesCode) 
CWS_NWT$speciesCode<-ifelse(CWS_NWT$speciesCode %in% southern,"NULL", CWS_NWT$speciesCode)

CWS_NWT$speciesCode<-ifelse(CWS_NWT$speciesCode=="GRAJ","CAJA",CWS_NWT$speciesCode) 

CWS_NWTfine<-subset(CWS_NWT, CWS_NWT$speciesCode %in% Masterlist)
CWS_NWTchk<-subset(CWS_NWT, CWS_NWT$speciesCode %notin% Masterlist)
unique(CWS_NWTchk$speciesCode) #mammals and unknowns
CWS_NWTchk$speciesCode <-"NULL"
CWS_NWTchk<-unique(CWS_NWTchk)
CWS_NWT<-rbind(CWS_NWTchk,CWS_NWTfine)
write.csv(CWS_NWT,"Cleaned_PCs/CWS_NWT_SHache_cleaned.csv")

########################## END OF DATASET CLEANING #################################

##########################################################
## PART 2. Format for QPAD + conversions + write out #####
##########################################################

## read raster data
rtree <- raster("./data/tree.tif")
crs <- proj4string(rtree)  #correct projection

## source functions
source("functions.R")

#### Import files:
setwd('./Cleaned_PCs/')

## Methods by dataset:
## BBA: Counts were 5 minutes long, unlimited radius: https://www.birdatlas.bc.ca/methods/
## CWS ARU: 3 minutes, infinite radius 
## CWS PC: 10 min count, 150m radius
## BBS: 3 minutes - 400m for visual ID though implies unlimited for sound. Treat as 400m.
## KM's BC Mountain data (ARU and PC): 6 min, infinite radius
## Seismic line surveys: 10 min, infinite radius
## ARU files (Big Grid, Wood Buffalo): mixed. Already have times, inf radius
## eBird: are survey specific in time and Infinite radius
## AK
#### BBA formatting ____________________________________________________
BC_BBA_clean<-read.csv("BC_BBA_AllSpp.csv") 

## Truncate lat lon to deal with recording inconsistencies within surveys
BC_BBA_clean$longitude<-plyr::round_any(as.numeric(BC_BBA_clean$longitude), accuracy=.0001, f=floor) # accurate within 11m
BC_BBA_clean$latitude<-plyr::round_any(as.numeric(BC_BBA_clean$latitude), accuracy=.0001, f=floor) #
BC_BBA_clean$LocationID<-paste("BBA_PC",as.character(sprintf("%.3f",BC_BBA_clean$lat)),as.character(sprintf("%.3f",BC_BBA_clean$lon)),sep="_")

# Observation count sums within surveys
summaryBBA<-summaryBy(ObservationCount~Spp+LocationID+latitude+longitude+survey_year+survey_month+survey_day+Hr+Min, FUN=sum, dat=BC_BBA_clean)
summaryBBA$spp <- summaryBBA$Spp
summaryBBA$count<-summaryBBA$ObservationCount.sum

## date and time
## https://en.wikipedia.org/wiki/ISO_8601
summaryBBA$dayformat<-ifelse(summaryBBA$survey_day<10,paste(0,summaryBBA$survey_day,sep=""),summaryBBA$survey_day) # add a 0 for single digits
summaryBBA$monthformat<-ifelse(summaryBBA$survey_month<10,paste(0,summaryBBA$survey_month,sep=""),summaryBBA$survey_month) # add a 0 for single digits
summaryBBA$hrformat<-ifelse(summaryBBA$Hr<10,paste(0,summaryBBA$Hr,sep=""),summaryBBA$Hr) # add a 0 for single digits
summaryBBA$minformat<-ifelse(summaryBBA$Min<10,paste(0,summaryBBA$Min,sep=""),summaryBBA$Min) # add a 0 for single digits

summaryBBA$dt <- paste(summaryBBA$survey_year,summaryBBA$monthformat,summaryBBA$dayformat,sep="-")  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)
summaryBBA$tm <- paste(summaryBBA$hrformat,summaryBBA$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

## spatial coordinates
summaryBBA$lon <- summaryBBA$longitude # longitude WGS84 (EPSG: 4326)
summaryBBA$lat <- summaryBBA$latitude # latitude WGS84 (EPSG: 4326)

## point count duration and truncation distance (Inf for unlimited)
summaryBBA$dur <- 5 # minutes
summaryBBA$dis <- "Inf" # meters
subset(summaryBBA,summaryBBA$count>20) #OK

QPAD_BBA <-summaryBBA[c(2,11:12,17:22)] 
QPAD_BBA$Source<-"BC BBA"

#### BBS formatting ______________________________________________________________
BBSin<-read.csv("BBS_year_restricted_clean.csv")

## Truncate lat lon to deal with recording inconsistencies within surveys
BBSin$location_longitude<-plyr::round_any(as.numeric(BBSin$location_longitude), accuracy=.0001, f=floor) # accurate within 11m
BBSin$location_latitude<-plyr::round_any(as.numeric(BBSin$location_latitude), accuracy=.0001, f=floor) #

BBSin$LocationID<-paste("BBS",as.character(sprintf("%.3f",BBSin$location_latitude)),as.character(sprintf("%.3f",BBSin$location_longitude)),sep="_")

BBSSummary<-summaryBy(pc_abundance~pc_species+LocationID+location_latitude+location_longitude+Year+Month+Day+Hr+Minute, FUN=sum, dat=BBSin)
BBSSummary$spp <- BBSSummary$pc_species
BBSSummary$count<-BBSSummary$pc_abundance.sum

## date and time
## https://en.wikipedia.org/wiki/ISO_8601
BBSSummary$hrformat<-ifelse(BBSSummary$Hr<10,paste(0,BBSSummary$Hr,sep=""),BBSSummary$Hr) # add a 0 for single digits
BBSSummary$minformat<-ifelse(BBSSummary$Min<10,paste(0,BBSSummary$Min,sep=""),BBSSummary$Min) # add a 0 for single digits
BBSSummary$moformat<-ifelse(as.numeric(BBSSummary$Month)<10,paste(0,BBSSummary$Month,sep=""),as.numeric(BBSSummary$Month)) # add a 0 for single digits
BBSSummary$dayformat<-ifelse(as.numeric(BBSSummary$Day)<10,paste(0,BBSSummary$Day,sep=""),as.numeric(BBSSummary$Day)) # add a 0 for single digits

subset(BBSSummary,BBSSummary$Min>59) #Why are there 60min inputs?
BBSSummary$minformat<-ifelse(BBSSummary$minformat==60,59,BBSSummary$minformat) #assume these are the end and not beginning of the hour listed
BBSSummary$dt <- paste(BBSSummary$Year,BBSSummary$moformat,BBSSummary$dayformat,sep="-")  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)
BBSSummary$tm <- paste(BBSSummary$hrformat,BBSSummary$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

## spatial coordinates
BBSSummary$lon <- BBSSummary$location_longitude # longitude WGS84 (EPSG: 4326)
BBSSummary$lat <- BBSSummary$location_latitude # latitude WGS84 (EPSG: 4326)

## point count duration and truncation distance
BBSSummary$dur <- 3 # minutes
BBSSummary$dis <- 400 # 

QPAD_BBS <-BBSSummary[c(2,11:12,17:22)] 
View(subset(QPAD_BBS,QPAD_BBS$count>20)) #cliff swallows/EUST etc not outlandish
QPAD_BBS$Source<-"BBS"

QPAD_BBS$count<-ifelse(QPAD_BBS$spp=="NULL",0,QPAD_BBS$count)
QPAD_BBS<-unique(QPAD_BBS)

#### CWS PC formatting  ______________________________________________________________

CWSin<-read.csv("Cleaned_CWS_BMP_landbirds.csv")

CWSin$location.longitude<-plyr::round_any(as.numeric(CWSin$location.longitude), accuracy=.0001, f=floor) # accurate within 11m
CWSin$location.latitude<-plyr::round_any(as.numeric(CWSin$location.latitude), accuracy=.0001, f=floor) #
CWSin$LocationID<-paste("CWS_PC",as.character(sprintf("%.3f",CWSin$location.latitude)),as.character(sprintf("%.3f",CWSin$location.longitude)),sep="_")

CWSSummary<-summaryBy(count~species_code+LocationID+location.latitude+location.longitude+year+month+day+hour_start+minute_start, FUN=sum, dat=CWSin)

CWSSummary$spp <- CWSSummary$species_code
CWSSummary$count<-CWSSummary$count.sum

CWSSummary$hrformat<-ifelse(CWSSummary$hour_start<10,paste(0,CWSSummary$hour_start,sep=""),CWSSummary$hour_start) # add a 0 for single digits
CWSSummary$minformat<-ifelse(CWSSummary$minute_start<10,paste(0,CWSSummary$minute_start,sep=""),CWSSummary$minute_start) # add a 0 for single digits
CWSSummary$dayformat<-ifelse(CWSSummary$day<10,paste(0,CWSSummary$day,sep=""),CWSSummary$day) # add a 0 for single digits
CWSSummary$monthformat<-ifelse(CWSSummary$month<10,paste(0,CWSSummary$month,sep=""),CWSSummary$month) # add a 0 for single digits
CWSSummary$dt <- paste(CWSSummary$year,CWSSummary$monthformat,CWSSummary$dayformat,sep="-")  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)
CWSSummary$tm <- paste(CWSSummary$hrformat,CWSSummary$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

CWSSummary$lon <- CWSSummary$location.longitude # longitude WGS84 (EPSG: 4326)
CWSSummary$lat <- CWSSummary$location.latitude # latitude WGS84 (EPSG: 4326)

CWSSummary$dur <- 10 # minutes
CWSSummary$dis <- 150 # meters 

QPAD_CWS <-CWSSummary[c(2,11:12,17:22)] 
QPAD_CWS$Source<-"CWS_BMP_PC"
summary(QPAD_CWS$count) 
subset(QPAD_CWS, QPAD_CWS$count>10) # irruptive/flocking spp particularly WWCR so OK. Prob will exclude from analysis as irruptive

QPAD_CWS$count<-ifelse(QPAD_CWS$spp=="NULL",0,QPAD_CWS$count)
QPAD_CWS<-unique(QPAD_CWS)

#### CWS HEMP ARU formatting______________________________________________________________

CWS_ARUin<-read.csv("Cleaned_CWS_ARU_landbirds.csv")

CWS_ARUin$longitude<-plyr::round_any(as.numeric(CWS_ARUin$longitude), accuracy=.0001, f=floor) # accurate within 11m
CWS_ARUin$latitude<-plyr::round_any(as.numeric(CWS_ARUin$latitude), accuracy=.0001, f=floor) #
CWS_ARUin$LocationID<-paste("CWS_HEMP_ARU",as.character(sprintf("%.3f",CWS_ARUin$latitude)),as.character(sprintf("%.3f",CWS_ARUin$longitude)),sep="_")

summaryCWS_ARU<-summaryBy(species_code~species_code+LocationID+latitude+longitude+Year+Month+Day+Hr+Minute, FUN=length, dat=CWS_ARUin)

summaryCWS_ARU$spp <- summaryCWS_ARU$species_code
summaryCWS_ARU$count<-summaryCWS_ARU$species_code.length

summaryCWS_ARU$hrformat<-ifelse(as.numeric(summaryCWS_ARU$Hr)<10,paste(0,summaryCWS_ARU$Hr,sep=""),as.numeric(summaryCWS_ARU$Hr)) # add a 0 for single digits
summaryCWS_ARU$minformat<-ifelse(as.numeric(summaryCWS_ARU$Minute)<10,paste(0,summaryCWS_ARU$Minute,sep=""),as.numeric(summaryCWS_ARU$Minute)) # add a 0 for single digits
summaryCWS_ARU$moformat<-ifelse(as.numeric(summaryCWS_ARU$Month)<10,paste(0,summaryCWS_ARU$Month,sep=""),as.numeric(summaryCWS_ARU$Month)) # add a 0 for single digits
summaryCWS_ARU$dayformat<-ifelse(as.numeric(summaryCWS_ARU$Day)<10,paste(0,summaryCWS_ARU$Day,sep=""),as.numeric(summaryCWS_ARU$Day)) # add a 0 for single digits

summaryCWS_ARU$dt <- paste(summaryCWS_ARU$Year,summaryCWS_ARU$moformat,summaryCWS_ARU$dayformat,sep="-")  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)
summaryCWS_ARU$tm <- paste(summaryCWS_ARU$hrformat,summaryCWS_ARU$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

summaryCWS_ARU$lon <- summaryCWS_ARU$longitude # longitude WGS84 (EPSG: 4326)
summaryCWS_ARU$lat <- summaryCWS_ARU$latitude # latitude WGS84 (EPSG: 4326)
summaryCWS_ARU$dur <- 3 # minutes
summaryCWS_ARU$dis <- 'Inf' #

QPAD_ARU_CWS <-summaryCWS_ARU[c(2,11:12,17:22)] 

summary(QPAD_ARU_CWS$count) 
subset(QPAD_ARU_CWS, QPAD_ARU_CWS$count>5) # check some of these - 6-7 WCSP: not outlandishly high
QPAD_ARU_CWS$Source<-"CWS_ARU"

QPAD_ARU_CWS$count<-ifelse(QPAD_ARU_CWS$spp=="NULL",0,QPAD_ARU_CWS$count)
QPAD_ARU_CWS<-unique(QPAD_ARU_CWS)

## KM/BC Mountain ARU data ______________________________________________________________

BCMntARUin<-read.csv("BC_MountainData_KM_CleanedPC_ARU.csv")#already summarized

BCMntARUin$DecimalLongitude<-plyr::round_any(as.numeric(BCMntARUin$DecimalLongitude), accuracy=.0001, f=floor) # accurate within 11m
BCMntARUin$DecimalLatitude<-plyr::round_any(as.numeric(BCMntARUin$DecimalLatitude), accuracy=.0001, f=floor) #
BCMntARUin$LocationID<-paste("KM_BC_MNT_ARU",as.character(sprintf("%.3f",BCMntARUin$DecimalLatitude)),as.character(sprintf("%.3f",BCMntARUin$DecimalLongitude)),sep="_")
subset(BCMntARUin,BCMntARUin$DecimalLongitude=="")

BCMntARUin$hrformat<-ifelse(as.numeric(BCMntARUin$Hr)<10,paste(0,BCMntARUin$Hr,sep=""),as.numeric(BCMntARUin$Hr)) # add a 0 for single digits
BCMntARUin$minformat<-ifelse(as.numeric(BCMntARUin$Min)<10,paste(0,BCMntARUin$Min,sep=""),as.numeric(BCMntARUin$Min)) # add a 0 for single digits
BCMntARUin$dt <- ifelse(BCMntARUin$Year<2000,paste(20,BCMntARUin$DATE,sep=""),BCMntARUin$DATE)  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)

#Odd 2010 dates - all surveys done in 2019
BCMntARUin$dt<-gsub("2010","2019",BCMntARUin$dt)

BCMntARUin$tm <- paste(BCMntARUin$hrformat,BCMntARUin$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

BCMntARUin$lon <- BCMntARUin$DecimalLongitude # longitude WGS84 (EPSG: 4326)
BCMntARUin$lat <- BCMntARUin$DecimalLatitude # latitude WGS84 (EPSG: 4326)
BCMntARUin$dur <- 6 # minutes
BCMntARUin$dis <- 'Inf' #

BCMntARUin$spp<-BCMntARUin$SPECIES_CODE
BCMntARUin$count<-BCMntARUin$ABUND

BCMtARUQPAD <-BCMntARUin[c(16,25:26,19:24,13)] 
BCMtARUQPAD$count<-ifelse(BCMtARUQPAD$spp=="NULL",0,BCMtARUQPAD$count)
BCMtARUQPAD<-unique(BCMtARUQPAD)
#corrected to here Dec 4
###### Now KM data PCs __________________________________________________________
BCMntKM<-read.csv("BC_MountainData_KM_CleanedPC.csv")
View(BCMntKM) #NOTE count=0 in some cases. These are unrecorded singles, multiples are recorded
BCMntKM$count<-ifelse(BCMntKM$count==0,1,BCMntKM$count)
BCMntKM$DecimalLongitude<-plyr::round_any(as.numeric(BCMntKM$DecimalLongitude), accuracy=.0001, f=floor) # accurate within 11m
BCMntKM$DecimalLatitude<-plyr::round_any(as.numeric(BCMntKM$DecimalLatitude), accuracy=.0001, f=floor) #
BCMntKM$LocationID<-paste("KM_BC_MNT_PC",as.character(sprintf("%.3f",BCMntKM$DecimalLatitude)),as.character(sprintf("%.3f",BCMntKM$DecimalLongitude)),sep="_")

BCMntKM$hrformat<-ifelse(as.numeric(BCMntKM$Hr)<10,paste(0,BCMntKM$Hr,sep=""),as.numeric(BCMntKM$Hr)) # add a 0 for single digits
BCMntKM$minformat<-ifelse(as.numeric(BCMntKM$Min)<10,paste(0,BCMntKM$Min,sep=""),as.numeric(BCMntKM$Min)) # add a 0 for single digits

BCMntKM$dt <- ifelse(BCMntKM$Year<2000,paste(20,BCMntKM$DATE,sep=""),BCMntKM$DATE)  # "2017-06-02" # ISO 8601 in YYYY-MM-DD (0-padded)
BCMntKM$tm <- paste(BCMntKM$hrformat,BCMntKM$minformat,sep=":") #"05:20" # ISO 8601 in hh:mm (24 hr clock, 0-padded)

BCMntKM$lon <- BCMntKM$DecimalLongitude # longitude WGS84 (EPSG: 4326)
BCMntKM$lat <- BCMntKM$DecimalLatitude # latitude WGS84 (EPSG: 4326)
BCMntKM$dur <- 6 # minutes
BCMntKM$dis <- 'Inf' #

BCMntKMQPAD <-BCMntKM[c(19,12:13,22:27,16)] 
BCMntKMQPAD$count<-ifelse(BCMntKMQPAD$spp=="NULL",0,BCMntKMQPAD$count)
BCMntKMQPAD<-unique(BCMntKMQPAD)

## eBird stationary counts______________________________________________________
eBirdin<-read.csv("eBird_clean_20072022.csv") #summarized

eBirdin$LONGITUDE<-plyr::round_any(as.numeric(eBirdin$LONGITUDE), accuracy=.0001, f=floor) # accurate within 11m
eBirdin$LATITUDE<-plyr::round_any(as.numeric(eBirdin$LATITUDE), accuracy=.0001, f=floor) #
eBirdin$LocationID<-paste("eBird",as.character(sprintf("%.3f",eBirdin$LATITUDE)),as.character(sprintf("%.3f",eBirdin$LONGITUDE)),sep="_")

eBirdin$spp<-eBirdin$species_code
eBirdin$count<-eBirdin$OBSERVATION.COUNT
eBirdin$dt<-eBirdin$OBSERVATION.DATE
eBirdin$tm<-substr(eBirdin$TIME.OBSERVATIONS.STARTED,1,5)
eBirdin$lon<-eBirdin$LONGITUDE
eBirdin$lat<-eBirdin$LATITUDE
eBirdin$dur<-eBirdin$DURATION.MINUTES
eBirdin$dis<-'Inf'
eBirdin$Source<-"eBird"

QPAD_eBird <-eBirdin[c(22:31)]  #NULL should be 0 counts
QPAD_eBird$count<-ifelse(QPAD_eBird$spp=="NULL",0,QPAD_eBird$count)
QPAD_eBird<-unique(QPAD_eBird)

### ARU data: WBF_JNP_BG__________________________________________________________

ARU_WBF_JNP_BG<-read.csv("ARU_WBF_JNP_BG.csv") #summarized

ARU_WBF_JNP_BG$longitude<-plyr::round_any(as.numeric(ARU_WBF_JNP_BG$longitude), accuracy=.0001, f=floor) # accurate within 11m
ARU_WBF_JNP_BG$latitude<-plyr::round_any(as.numeric(ARU_WBF_JNP_BG$latitude), accuracy=.0001, f=floor) #
ARU_WBF_JNP_BG$LocationID<-paste("ARU_WBF_JNP_BG",as.character(sprintf("%.3f",ARU_WBF_JNP_BG$latitude)),as.character(sprintf("%.3f",ARU_WBF_JNP_BG$longitude)),sep="_")

ARU_WBF_JNP_BG$spp<-ARU_WBF_JNP_BG$species_code
ARU_WBF_JNP_BG$count<-ARU_WBF_JNP_BG$abundance
ARU_WBF_JNP_BG$dt<-substr(ARU_WBF_JNP_BG$recording_date,1,10)
ARU_WBF_JNP_BG$tm<-substr(ARU_WBF_JNP_BG$recording_date,12,19)
ARU_WBF_JNP_BG$lon<-ARU_WBF_JNP_BG$longitude
ARU_WBF_JNP_BG$lat<-ARU_WBF_JNP_BG$latitude
ARU_WBF_JNP_BG$dis<-'Inf'
ARU_WBF_JNP_BG$Source<-"ARU_WBF_JNP_BG"

QPAD_WBFJNPBG_ARU<-ARU_WBF_JNP_BG[c(20:26,19,27:28)]
QPAD_WBFJNPBG_ARU$count<-ifelse(QPAD_WBFJNPBG_ARU$spp=="NULL",0,QPAD_WBFJNPBG_ARU$count)
QPAD_WBFJNPBG_ARU<-unique(QPAD_WBFJNPBG_ARU)

#Duplicate records - need to find and fix
QPAD_WBFJNPBG_ARU$Survey<-paste(QPAD_WBFJNPBG_ARU$LocationID,QPAD_WBFJNPBG_ARU$dt,QPAD_WBFJNPBG_ARU$tm, sep="_")
#e.g:
#subset(QPAD_AK,QPAD_AK$Survey=="AlaskaPC_62.649_-142.391_2007-06-14_04:28")

ck<-summaryBy(Survey~Survey+spp+count, FUN=length, dat=QPAD_WBFJNPBG_ARU)
ck<-subset(ck,ck$Survey.length>1) #12 records are duplicated all in 5AM ARU surveys
duplicated<-unique(ck$Survey) #4 surveys
QPAD_WBFJNPBG_ARUfine<-subset(QPAD_WBFJNPBG_ARU,QPAD_WBFJNPBG_ARU$Survey %notin% duplicated)
QPAD_WBFJNPBG_ARUdup<-subset(QPAD_WBFJNPBG_ARU,QPAD_WBFJNPBG_ARU$Survey %in% duplicated) # lon off by 0.0001 degrees in subset 
QPAD_WBFJNPBG_ARUdup<-subset(QPAD_WBFJNPBG_ARUdup, as.character(QPAD_WBFJNPBG_ARUdup$lon)!="-110.9014")
QPAD_WBFJNPBG_ARUdup<-subset(QPAD_WBFJNPBG_ARUdup, QPAD_WBFJNPBG_ARUdup$dur!=1)
QPAD_WBFJNPBG_ARUdup$dur<-ifelse(QPAD_WBFJNPBG_ARUdup$dur==10,3,QPAD_WBFJNPBG_ARUdup$dur) # set 10 counts to 3 as are majority
QPAD_WBFJNPBG_ARUdup<-unique(QPAD_WBFJNPBG_ARUdup)
QPAD_WBFJNPBG_ARU<-rbind(QPAD_WBFJNPBG_ARUdup,QPAD_WBFJNPBG_ARUfine)
QPAD_WBFJNPBG_ARU<-QPAD_WBFJNPBG_ARU[-11]

## Seismic lines____________________________________________________________

Seismic <- read.csv("SeismicLines.csv")

Seismic$longitude<-plyr::round_any(as.numeric(Seismic$longitude), accuracy=.0001, f=floor) # accurate within 11m
Seismic$latitude<-plyr::round_any(as.numeric(Seismic$latitude), accuracy=.0001, f=floor) #
Seismic$LocationID<-paste("Seismic",as.character(sprintf("%.3f",Seismic$latitude)),as.character(sprintf("%.3f",Seismic$longitude)),sep="_")
names(Seismic)
SeismicSummary<-summaryBy(abundance~speciesCode+date+species_code+LocationID+latitude+longitude, FUN=sum, dat=Seismic)
SeismicSummary$spp<-SeismicSummary$speciesCode
SeismicSummary$count<-SeismicSummary$abundance.sum
SeismicSummary$dt<-substr(SeismicSummary$date,1,10)
SeismicSummary$tm<-substr(SeismicSummary$date,12,19)
SeismicSummary$lon<-SeismicSummary$longitude
SeismicSummary$lat<-SeismicSummary$latitude
SeismicSummary$dis<-'Inf'
SeismicSummary$dur<-10
SeismicSummary$Source<-"SeismicLines"
names(SeismicSummary)
QPAD_SeismicLines<-SeismicSummary[c(3,7:15)]

QPAD_SeismicLines$count<-ifelse(QPAD_SeismicLines$spp=="NULL",0,QPAD_SeismicLines$count)
QPAD_SeismicLines<-unique(QPAD_SeismicLines)

# Alaska Data___________________________________________________________

Alaska<-read.csv("AlaskaData.csv")

Alaska$longitude<-plyr::round_any(as.numeric(Alaska$longitude), accuracy=.0001, f=floor) # accurate within 11m
Alaska$latitude<-plyr::round_any(as.numeric(Alaska$latitude), accuracy=.0001, f=floor) #
Alaska$LocationID<-paste("AlaskaPC",as.character(sprintf("%.3f",Alaska$latitude)),as.character(sprintf("%.3f",Alaska$longitude)),sep="_")

summaryAlaska<-summaryBy(abundance~speciesCode+LocationID+latitude+longitude+date+dur+dis, FUN=sum, dat=Alaska)

summaryAlaska$spp<-summaryAlaska$speciesCode
summaryAlaska$count<-summaryAlaska$abundance.sum
summaryAlaska$dt<-substr(summaryAlaska$date,1,10)
summaryAlaska$tm<-substr(summaryAlaska$date,12,19)
summaryAlaska$lon<-summaryAlaska$longitude
summaryAlaska$lat<-summaryAlaska$latitude

summaryAlaska<-subset(summaryAlaska, summaryAlaska$tm!="NA") # get rid of unknown time surveys - unfortunately 4000 records
summaryAlaska$tm<-ifelse(nchar(summaryAlaska$tm)<5,paste(0,summaryAlaska$tm,sep=""),summaryAlaska$tm) #correct formatting for single digits

QPAD_AK<-summaryAlaska[c(2,6:7,9:14)]
QPAD_AK$Source<-"AlaskanPointCounts"

## AK has duplicates survey entries...
## Different durations recorded for same observations. Go with more conservative 10 min rather than 8.
QPAD_AK$Survey<-paste(QPAD_AK$LocationID,QPAD_AK$dt,QPAD_AK$tm, sep="_")
#e.g:
#subset(QPAD_AK,QPAD_AK$Survey=="AlaskaPC_62.649_-142.391_2007-06-14_04:28")

ck<-summaryBy(Survey~Survey+spp+count, FUN=length, dat=QPAD_AK)
ck<-subset(ck,ck$Survey.length>1) #2999 records are duplicated
duplicated<-unique(ck$Survey) #453 surveys
QPAD_AKfine<-subset(QPAD_AK,QPAD_AK$Survey %notin% duplicated)
QPAD_AKdup<-subset(QPAD_AK,QPAD_AK$Survey %in% duplicated)
QPAD_AKdup<-subset(QPAD_AKdup,QPAD_AKdup$dur==10)# keep 10 min survey
QPAD_AK<-rbind(QPAD_AKfine,QPAD_AKdup) #92250-89248=3004 replicates removed, 3002 if 10 min reps removed == some differences in records but minor
QPAD_AK<-QPAD_AK[-11]

## CWS Northwest Territories_______________________________________________

NTBMS2019<-read.csv("CWS_NWT_SHache_cleaned.csv")

NTBMS2019$longitude<-plyr::round_any(as.numeric(NTBMS2019$longitude), accuracy=.0001, f=floor) # accurate within 11m
NTBMS2019$latitude<-plyr::round_any(as.numeric(NTBMS2019$latitude), accuracy=.0001, f=floor) #
NTBMS2019$LocationID<-paste("NTBMS2019",as.character(sprintf("%.3f",NTBMS2019$latitude)),as.character(sprintf("%.3f",NTBMS2019$longitude)),sep="_")

summaryNTBMS2019<-summaryBy(abundance~speciesCode+LocationID+latitude+longitude+date, FUN=sum, dat=NTBMS2019)

summaryNTBMS2019$spp<-summaryNTBMS2019$speciesCode
summaryNTBMS2019$count<-summaryNTBMS2019$abundance.sum
summaryNTBMS2019$dt<-substr(summaryNTBMS2019$date,1,10)
summaryNTBMS2019$tm<-substr(summaryNTBMS2019$date,12,19)
summaryNTBMS2019$lon<-summaryNTBMS2019$longitude
summaryNTBMS2019$lat<-summaryNTBMS2019$latitude
summaryNTBMS2019$dur<- 10
summaryNTBMS2019$dis<-"Inf"
summaryNTBMS2019$Source <-"NTBMS2019_NWT"

QPAD_NTBMS<-summaryNTBMS2019[c(2,7:15)]

#######################
#### Stack them all 
####______________________________________________________________

BirdData<-rbind(QPAD_BBA,QPAD_BBS,QPAD_CWS,QPAD_ARU_CWS,QPAD_eBird,QPAD_SeismicLines,QPAD_WBFJNPBG_ARU,BCMntKMQPAD,BCMtARUQPAD,QPAD_AK,QPAD_NTBMS) #1262580 records

### Get rid of anomalous times and dates
BirdData$tm<-ifelse(nchar(BirdData$tm)<5,paste(0,BirdData$tm,sep=""),BirdData$tm)
BirdData$tm<-substr(BirdData$tm,1,5)
BirdData$samp<-as.numeric(substr(BirdData$tm,1,2))
BirdData<-subset(BirdData,BirdData$samp<=10) #remove late-night ARU samples - all other survey's end at 10AM

#plot(BirdData$samp~BirdData$lat) # note very early morning ARU samples
#***some of this may be high latitude (post-sunrise) 
#***need to filter using SSR data in QPAD

BirdData<-subset(BirdData,BirdData$lat>10) # remove Alaskan data with no location

# Now limit to breeding period
BirdData$samp<-as.numeric(substr(BirdData$dt,6,7))
summary(BirdData$samp)
BirdData<-subset(BirdData,BirdData$samp<8 &BirdData$samp>4) #limit to May-July
BirdData<-BirdData[,-11]   # 1453233 records

## Change/fix naming conventions for QPAD offsets
## CAJA=="GRAJ",WWPE=="WEWP"
## Correct DEJU and VATH typo
BirdData$spp<-ifelse(BirdData$spp=="CAJA","GRAJ",ifelse(BirdData$spp=="WWPE","WEWP",BirdData$spp))
BirdData$spp<-ifelse(BirdData$spp=="DEJU ","DEJU", BirdData$spp)
BirdData$spp<-ifelse(BirdData$spp=="VAth","VATH", BirdData$spp)

length(unique(BirdData$LocationID)) #68314 location IDs

### Proof count and distance/duration columns
subset(BirdData,is.na(BirdData$count)) # one NA here but original records show 1 distant audio rec of RECR
BirdData$count<-ifelse(is.na(BirdData$count),1,BirdData$count) # fixed

BirdData$count<-ifelse(BirdData$count=="X",1,BirdData$count) # eBird records with X == P/A? Setting to 1? Most seem to be waterbirds and will be dropped anyway
subset(BirdData,BirdData$count=="N/A") #unclear as to why these are N/As. Usually applied to inanimate sounds.
BirdData$count<-ifelse(BirdData$count=="N/A",1,BirdData$count) #only 12 records - assign count of 1 as presence info is important
BirdData<-subset(BirdData,BirdData$dur!="UNK") 
BirdData<-subset(BirdData,BirdData$dis!="UNK") #1396161 records

## Confirm NULL records are all 0 counts
BirdData$count<-ifelse(BirdData$spp=="NULL",0,BirdData$count)
BirdData<-unique(BirdData)

##Note some ARU records are TMTT - using the national model approach to assign the top 99% value
# Generate upper 99% counts for each species________________________
q<-unique(BirdData$spp)
upper<-c()
ceiling_val<-data.frame(spp=q)

for (i in 1:length(q)){
  yyy<-subset(BirdData,BirdData$spp==q[i]&BirdData$count!="TMTT")
  yyy<-as.numeric(yyy$count)
  upper[i] <- ceiling(quantile(yyy[yyy>0 & yyy<100], 0.99, na.rm=TRUE))
}

upperlevel<-data.frame(cbind(q,upper))

## split the dataset into abundance and TMTT records
TMTT<-subset(BirdData, BirdData$count=="TMTT")
Count<-subset(BirdData, BirdData$count!="TMTT")

## create vector of the upper 99% values 
species<-TMTT$spp
value<-vector()
for (i in 1:length(species)){
value[i] <- as.vector(subset(upperlevel, upperlevel$q==species[i])[,2])
}

# Assign to counts
TMTT$count<-value
BirdData<-rbind(TMTT,Count)  # fixed
##__________________________________________________

### Set variables to numeric
BirdData$count<-as.numeric(BirdData$count)
BirdData$dur<-as.numeric(BirdData$dur)

#### Get rid of overlapping data (double entries between known projects and eBird)
#devtools::install_github("r-barnes/dggridR", force=TRUE)
library(dggridR)
dggs <- dgconstruct(spacing = 0.2) # ~206 m spacing, 0.39 km2 - BBS is 800m apart, KM data is 200m. Lat Lon is 11.1 m accuracy, max so == 617 m blocks max

BirdData$local <- dgGEO_to_SEQNUM(dggs, BirdData$lon, BirdData$lat)$seqnum
BirdData$Duplicate<-paste(BirdData$local,BirdData$tm,BirdData$dt) #date-time-217radius

Points<-unique(BirdData[c(1,12)]) #207143 location-surveys
Points$survey<-1
loc<-summaryBy(survey~Duplicate, dat=Points, FUN=length) #count surveys at the same date and time stamp and within 200m of each other

BirdData2<-merge(BirdData,loc,by="Duplicate", all.x=TRUE) 
BirdData2$survey.length<-paste(BirdData2$Source, BirdData2$survey.length)
unique(BirdData2$survey.length) # get rid of likely eBird double entries
duplicates<-c("eBird 2","eBird 3")

BirdData2<-subset(BirdData2, BirdData2$survey.length %notin% duplicates)#1370296

# Look at remaining replicates
Points<-unique(BirdData2[c(1,2)]) #203233/207143  dropped 2% of location-surveys
Points$survey<-1
loc<-summaryBy(survey~Duplicate, dat=Points, FUN=length)
Ck<-subset(loc,loc$survey.length>1) #64 duplicate location-surveys - most BBA. 
BirdDataCk<-subset(BirdData2,BirdData2$Duplicate %in% Ck$Duplicate)
unique(BirdDataCk[1:2]) 
# Mostly BBA - Looks like recording errors but likely *time* not place (ballpark time recorded at nearby points?). Species not duplicated.
# There is a weird subset of 4 BBA and BBS surveys with identical time/date/locations. 
# Species don't match, assume these are different

BirdData3<-BirdData2[2:11] 


#####################################################################
###          Now species representation in the dataset            ###
#####################################################################

length(unique(BirdData3$LocationID)) #63760 locations
Occur<-summaryBy(count~LocationID+spp, FUN=sum, dat=BirdData3) #summarize individual species counts within location
Occur$count.sum<-ifelse(Occur$count.sum>0,1,0) # occur or not
Occur<-summaryBy(count.sum~spp, FUN=sum, dat=Occur)#total sites occuring
Occur$Rate<-(Occur$count.sum.sum/63760)*100  # 63760*0.01  # 1%==occurs at 638 sites
Oneper<-subset(Occur,Occur$Rate>=1) #123 species 
Oneper<-c(Oneper$spp,"BANS") # Add back Bank Swallow - listed spp - though 0.8% occurrence

### Look for and remove species currently only seen outside the focal Ecozone

BorealTaiga <- st_read('/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main/RegionShapefile/Boreal_Taiga.shp')
BorealTaiga <- st_simplify(BorealTaiga, dTolerance = 4000)
crs <- "+proj=longlat +datum=WGS84 +no_defs"
BorealTaiga <- st_transform(BorealTaiga, crs)

xy<-BirdData3[6:7]
coordinates(xy)<- ~ lon + lat
proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs"
xy<-st_as_sf(xy) #turn into a sf object
xyclim <- xy[BorealTaiga, ] # subset with the shape file
BorealTaigaPts<-as.data.frame(st_coordinates(xyclim))

EcozoneSubset<-subset(BirdData3,paste(BirdData3$lon,BirdData3$lat, sep="_") %in% paste(BorealTaigaPts$X,BorealTaigaPts$Y, sep="_"))
EcozoneSpp<-unique(EcozoneSubset$spp)

Spp<-subset(Oneper,Oneper %in% EcozoneSpp) # Reduced to 105 species

##Write out sub-setted species file for reference
setwd('/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main')
Masterlist2<-read.csv("Spp_MasterList_YK&BC.csv") #Known breeders in the region
SppName<-c(Spp,"CAJA","WWPE") #naming issue again
SpeciesFinal<-subset(Masterlist2,Masterlist2$Species_ID%in%SppName)# 110, AMCR duplicated as expected
write.csv(SpeciesFinal,"ModelingSubset_SppMasterList.csv")

#### *****13 western species do not have QPAD offsets - temporarily drop from list ####
## VGSW, MOCH, MGWA, CAHU, BHGR, CBCH, NRWS, PAWR, PSFL, RBSA, RUHU, STJA, WEME, WIFL irruptive CORE, PISI, WWCR, and nocturnal CONI (not well sampled by morning counts), raptors BAEA, OSPR, AMKE, and RTHA, and coastal SOGR
NoMod<-c("NULL","WEME","VGSW","MOCH", "MGWA","CAHU","BHGR","CBCH","NRWS","PAWR","PSFL","RBSA","RUHU","STJA","WIFL","CORE","WWCR","PISI","CONI","BAEA",'RTHA',"OSPR","AMKE","SOGR")
Spp<-subset(Spp,Spp%notin%NoMod) #84 spp.
write.csv(Spp,"SppListOnePer.csv")

### In future could use NAPOPS https://na-pops.org/index.html#about - has p q and EDR for these species
#########################################################

######################################################################
### Limit full Dataset to focal species subset but retain all surveys
######################################################################

BirdData3$SurveyID<-paste(BirdData3$LocationID,BirdData3$dt,BirdData3$tm,sep="/")#create unique survey identifier
Surveys<-unique(BirdData3$SurveyID) #202881 surveys done

BirdData4<-subset(BirdData3,BirdData3$spp %in% Spp) #1101945 records
SurveyWith<-unique(BirdData4$SurveyID) #199447 - have lost a bunch of surveys where none of the focal birds were IDed
SurveyAdd<-subset(Surveys, Surveys%notin%SurveyWith) #3434 survey events need to be added back. correct.

AddIn<-subset(BirdData3, BirdData3$SurveyID%in%SurveyAdd) #4605 records
AddIn$spp<-"NULL"
AddIn$count<-0
AddIn<-unique(AddIn)#3526-3434 surveys to add back - why the extra 92?
n_occur <- data.frame(table(AddIn$SurveyID))
n_occur <-n_occur[n_occur$Freq > 1,] # all eBird with slightly off coordinates
duplicated<-n_occur$Var1
Fine<-subset(AddIn,AddIn$SurveyID %notin% duplicated) #3342
Duplicated<-subset(AddIn,AddIn$SurveyID %in% duplicated)
Duplicated #eBird (NULLS with 0.0001 degree lon off)
Duplicated<-Duplicated[seq(1, nrow(Duplicated), 2), ] #pick alternate rows
BirdData_Clean<-rbind(BirdData4,Fine,Duplicated) #1105379

Surveys<-unique(BirdData_Clean$SurveyID) #202881 surveys - correct

# Add a year column 
BirdData_Clean$year<-as.numeric(substr(BirdData_Clean$dt,1,4))

saveRDS(BirdData_Clean,"AllBirdDataCleanedJul20.rds") #110539 records

### staring here..._____________________________________________
BirdData_Clean<-readRDS("AllBirdDataCleanedJul20.rds")
length(unique(BirdData_Clean$LocationID))#63760 locations - after removing Aleutians, below 63681
length(unique(BirdData_Clean$SurveyID)) # 202881, remove Aleutians== 202712 should be output size below
## for annual LCC QPAD see previous version of this file
#67% of LCC4 categories align between QPAD and annual data  
#79% agreement in LCC2
#TREE shows much less concordance

##########################################################################
### Create series of species-specific datasets with offsets appended to them

## QPAD ######

## read raster data
rlcc <- raster("./data/lcc.tif")
rtree <- raster("./data/tree.tif")
rtz <- raster("./data/utcoffset.tif")
rd1 <- raster("./data/seedgrow.tif")
crs <- proj4string(rtree)
plot(rd1)

##### Get spp list ##
Oneper<-read.csv("SppListOnePer.csv") 
Oneper<-Oneper$x
BAM<-getBAMspecieslist()
Oneper1<-c(Oneper %in% BAM) #confirm all there.
write.csv(Oneper,"FinalSpeciesList_YKproject.csv")

.rs.unloadPackage("tidyr") # extract in tidyr is preventing raster from functioning


#Note QPAD limits region over which it will calculate offsets
#checkfun(lon, "lon", c(-164, -52))
#checkfun(lat, "lat", c(39, 69))
subset(BirdData_Clean,BirdData_Clean$lon<(-164)) #AK surveys on the Aleutians, can drop
BirdData_Clean<-subset(BirdData_Clean,BirdData_Clean$lon>=(-164)) #AK surveys on the Aleutians, can drop

subset(BirdData_Clean,BirdData_Clean$lat>69) #this is more of an issue, BUT rasters extend up here so over-ride limits

#### Edit source code #####
getAnywhere(make_x) #### for annual edit of make_x function - see previous iteration #

make_x<-function(
    dt, tm, lon, lat, dur, dis, ...,
    check_xy=TRUE) {
  ## checking lengths
  nn <- c(dt=length(dt), tm=length(tm), lon=length(lon), lat=length(lat), dur=length(dur), dis=length(dis))
  n1 <- nn[nn == 1L]
  n2 <- nn[nn > 1L]
  if (!all(n2 == n2[1L]))
    stop("input lengths must be equal or 1")
  n <- unname(if (length(n2)) n2[1L] else n1[1L])
  if (length(dt) == 1L)
    dt <- rep(dt, n)
  if (length(tm) == 1L)
    tm <- rep(tm, n)
  if (length(lon) == 1L)
    lon <- rep(lon, n)
  if (length(lat) == 1L)
    lat <- rep(lat, n)
  if (length(dur) == 1L)
    dur <- rep(dur, n)
  if (length(dis) == 1L)
    dis <- rep(dis, n)
  ## types
  lat <- as.numeric(lat)
  lon <- as.numeric(lon)
  dur <- as.numeric(dur)
  dis <- as.numeric(dis)
  ## parse date+time into POSIXlt
  dt <- as.character(dt)
  tm <- as.character(tm)
  dtm <- strptime(paste0(dt, " ", tm, ":00"),
                  format="%Y-%m-%d %H:%M:%S", tz="America/Edmonton")
  day <- as.integer(dtm$yday)
  hour <- as.numeric(round(dtm$hour + dtm$min/60, 2))
  ## checks
  checkfun <- function(x, name="", range=c(-Inf, Inf)) {
    if (any(x[!is.na(x)] %)(% range))
      stop(sprintf("Parameter %s is out of range [%.0f, %.0f]", name, range[1], range[2]))
    invisible(NULL)
  }
  ## BCR 4:14 included
  ## crs: WGS84 (EPSG: 4326)
  ## "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  #         min       max
  #x -163.89547 -52.66936
  #y   39.66214  68.98741
  if (check_xy) {
    checkfun(lon, "lon", c(-164, -52))
    checkfun(lat, "lat", c(39, 70)) #EDIT - let it go 1 degree further north
  }
  checkfun(day, "day", c(0, 360))
  checkfun(hour, "hour", c(0, 24))
  checkfun(dur, "dur", c(0, Inf))
  checkfun(dis, "dis", c(0, Inf))
  if (any(is.infinite(lon)))
    stop("Parameter lon must be finite")
  if (any(is.infinite(lat)))
    stop("Parameter lat must be finite")
  ## handling missing values
  ok_xy <- !is.na(lon) & !is.na(lat)
  
  ## intersect here
  xy <- data.frame(x=lon, y=lat)
  xy$x[is.na(xy$x)] <- mean(xy$x, na.rm=TRUE)
  xy$y[is.na(xy$y)] <- mean(xy$y, na.rm=TRUE)
  coordinates(xy) <- ~ x + y
  proj4string(xy) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  xy <- spTransform(xy, crs)
  
  ## LCC4 and LCC2
  vlcc <- extract(rlcc, xy)
  # 0: No data (NA/NA)
  # 1: Temperate or sub-polar needleleaf forest (Conif/Forest)
  # 2: Sub-polar taiga needleleaf forest (Conif/Forest)
  # 5: Temperate or sub-polar broadleaf deciduous (DecidMixed/Forest)
  # 6:  Mixed Forest (DecidMixed/Forest)
  # 8: Temperate or sub-polar shrubland (Open/OpenWet)
  # 10: Temperate or sub-polar grassland (Open/OpenWet)
  # 11: Sub-polar or polar shrubland-lichen-moss (Open/OpenWet)
  # 12: Sub-polar or polar grassland-lichen-moss (Open/OpenWet)
  # 13: Sub-polar or polar barren-lichen-moss (Open/OpenWet)
  # 14: Wetland (Wet/OpenWet)
  # 15: Cropland (Open/OpenWet)
  # 16: Barren Lands (Open/OpenWet)
  # 17: Urban and Built-up (Open/OpenWet)
  # 18: Water (NA/NA)
  # 19: Snow and Ice (NA/NA)
  lcclevs <- c("0"="", "1"="Conif", "2"="Conif", "3"="", "4"="",
               "5"="DecidMixed", "6"="DecidMixed", "7"="", "8"="Open", "9"="",
               "10"="Open", "11"="Open", "12"="Open", "13"="Open", "14"="Wet",
               "15"="Open", "16"="Open", "17"="Open", "18"="", "19"="")
  lcc4 <- factor(lcclevs[vlcc+1], c("DecidMixed", "Conif", "Open", "Wet"))
  lcc2 <- lcc4
  levels(lcc2) <- c("Forest", "Forest", "OpenWet", "OpenWet")
  
  ## TREE
  vtree <- extract(rtree, xy)
  TREE <- vtree / 100
  TREE[TREE %)(% c(0, 1)] <- 0
  
  ## extract seedgrow value (this is rounded)
  d1 <- extract(rd1, xy)
  ## UTC offset + 7 makes Alberta 0 (MDT offset)
  tz <- extract(rtz, xy) + 7
  
  ## transform the rest
  JDAY <- round(day / 365, 4) # 0-365
  TREE <- round(vtree / 100, 4)
  MAXDIS <- round(dis / 100, 4)
  MAXDUR <- round(dur, 4)
  
  ## sunrise time adjusted by offset
  ok_dt <- !is.na(dtm)
  dtm[is.na(dtm)] <- mean(dtm, na.rm=TRUE)
  sr <- sunriset(cbind("X"=lon, "Y"=lat),
                 as.POSIXct(dtm, tz="America/Edmonton"),
                 direction="sunrise", POSIXct.out=FALSE) * 24
  TSSR <- round(unname((hour - sr + tz) / 24), 4)
  
  ## days since local spring
  DSLS <- (day - d1) / 365
  
  out <- data.frame(
    TSSR=TSSR,
    JDAY=JDAY,
    DSLS=DSLS,
    LCC2=lcc2,
    LCC4=lcc4,
    TREE=TREE,
    MAXDUR=MAXDUR,
    MAXDIS=MAXDIS,
    ...)
  out$TSSR[!ok_xy | !ok_dt] <- NA
  out$DSLS[!ok_xy] <- NA
  out$LCC2[!ok_xy] <- NA
  out$LCC4[!ok_xy] <- NA
  out$TREE[!ok_xy] <- NA
  out
}

## Summary output
Summary<-data.frame(Sp=factor(),Max=double(),Mean=double())

### issue with eBird coordinates - number of duplicate surveys off by 0.0001 longitude
### need to go back but don't have time ATM. Post-hoc fix below

for(i in 1: length(Oneper)) 
  {
SpData<-subset(BirdData_Clean,BirdData_Clean$spp%in%Oneper[i]) # get species sightings
SurveyP<-SpData$SurveyID
Nosighting<-subset(BirdData_Clean,BirdData_Clean$SurveyID%notin%SurveyP) # surveys where not seen
Nosighting$spp<-Oneper[i] # set all to focal species
Nosighting$count<-0 # set all to 0 counts
Nosighting<-unique(Nosighting) #reduce the data frame to 1 record per survey

SpData<-rbind(SpData,Nosighting) #206692

#eBird problem records
n_occur <- data.frame(table(SpData$SurveyID))
n_occur <-n_occur[n_occur$Freq > 1,] # all eBird with slightly off coordinates
duplicated<-n_occur$Var1

Data_Fine<-subset(SpData,SpData$SurveyID %notin% duplicated) #196889
Data_Dup<-subset(SpData,SpData$SurveyID %in% duplicated)
Data_Fixed<-Data_Dup[!duplicated(Data_Dup$SurveyID),] #get rid of duplicates

DataClean<-rbind(Data_Fine,Data_Fixed) #202712 correct

yyy<-SpData$count #apply the 99%ile count 
upper <- ceiling(quantile(yyy[yyy>0 & yyy<100], 0.99))
DataClean$count<-ifelse(DataClean$count>upper,upper,DataClean$count) #apply the 99% ceiling to the data as with TMTT and following BAM protocols

# Get offsets, annual values will be used for the LCC and %cover

x <- make_x(DataClean$dt, DataClean$tm, DataClean$lon, DataClean$lat, DataClean$dur, DataClean$dis)
o <- make_off(Oneper[i], x)
Join<-cbind(DataClean,o)
Join$Dens <- Join$count/(Join$A*Join$p*Join$q) # Calculate density/ha

### Write out single species offsets #####
saveRDS(Join,paste("./QPAD_Output/",(Oneper[i]),"_Data.rds",sep=""))

### Make a joined density table for all species - not enough memory just get summary stats ####
Out<-data.frame(Oneper[i],mean(Join$Dens),max(Join$Dens))
Summary<-rbind(Summary,Out)
}

options(scipen=999)
subset(Summary,Summary$max.Join.Dens.>15) 
## CLSW mean is reasonable except max (likely 1000 is OK for colonies, not 8000)
## if mean is OK the odd extreme max can be removed with later scaling

## extreme density for BRBL, GCSP, RECR, TOWA, TOSO 
## issues with BAM models these species?
bestmodelBAMspecies("BRBL", type="BIC")
summaryBAMspecies("BRBL", "12", "0") # DSLS/DSLS2 bad estimates
bestmodelBAMspecies("GCSP", type="BIC")
summaryBAMspecies("GCSP", "3", "1") # JDAY/JDAY2 bad estimates
summaryBAMspecies("GCSP", "3", "1") # shows the extreme squared term for date
summaryBAMspecies("GCSP", "6", "1") # fails
summaryBAMspecies("GCSP", "10", "1")# extreme
summaryBAMspecies("GCSP", "1", "1") # more reasonable

### Drop extreme species
drop<-c("BRBL","GCSP","RECR","TOWA","TOSO")
Oneper<-Oneper[Oneper %notin% drop]#78 species
write.csv(Oneper,"FinalSpeciesList_YKproject.csv") # write it out again

#############################  END OF QPAD ##########################
