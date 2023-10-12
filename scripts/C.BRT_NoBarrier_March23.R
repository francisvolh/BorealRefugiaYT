##################################################
###   D. BRT Model + Test performance
###   Anna Drake  March, 2023
##################################################

### Clarification of QPAD offsets
#p*q*A = correction factor in QPAD
#log(p*q*A) = offset term

#And Density is any of the following
#exp(log(count) - log(p*q*A)) 
#exp(log(count) - offset) 
#exp(log(count))/exp(log(p*q*a)) 
#count/correction 

# In this model: "count" is y, "offset" is the QPAD offset &
# "Predictors" are the Predictors (x), excluding "offset"

###########################################################

##Load packages--------------------------------------------------

#require(devtools)
#devtools::install_github("r-barnes/dggridR", force=TRUE)

library(pacman)
pacman::p_load(scales,sf,dggridR,lubridate,Metrics,here,nlme) # for prep 
pacman::p_load(svMisc,data.table,tidyverse,ggplot2,gbm,magrittr,dplyr,
               rsample,purrr,readr,hms,stringr,matrixStats,parallel,
               doParallel,sf,sp,doBy) #if just running GBM
detectCores()  # 

## Set directory--------------------------------------------------
dir<-'/Users//annadrake/Desktop/Yukon Project Data/qpad-offsets-main' # home
setwd(dir)

### add function, set seed--------------------------------------------------
`%notin%` <- Negate(`%in%`)
pb <- txtProgressBar(min = 0, max = 74, style = 3, width = 50, char = "=")
set.seed(1) 

### Get species list--------------------------------------------------           
SpList<-read.csv("FinalSpeciesList_YKproject.csv")
SpList<-SpList$x 
drop<-c("BRBL","GCSP","RECR","TOWA","TOSO") #spp where QPAD values are extreme
SpList<-SpList[SpList %notin% drop]#79 species

### Check all spp for temporal trend (will mostly be BBS data)###############
#Temporal<-data.frame()

#for(i in 1: length(SpList)){
#Species<-read.csv(paste("./QPAD_Output/",SpList[[i]],"_Data.csv",sep=""))
#mult<-summaryBy(year~LocationID,FUN=length, dat=unique(Species[c()])
#mult<-subset(mult,mult$year.length>2) # limit to repeated samples at same place
#mult<-subset(Species,Species$LocationID %in% mult$LocationID)
#m1 <- lme(Dens~ year + VCF, random=~1|LocationID, dat=mult, control=(msMaxIter=10000))
#ck<-anova(m1)
#flag<-ck$`p-value`[2]<0.05
#dir<-sign(m1$coefficients$fixed[2])
#Temp<-cbind(Species$spp[1],flag,dir)
#Temporal<-rbind(Temporal,Temp)
#}

#subset(Temporal,Temporal$flag=="TRUE") # some spp have significant trend. Trend is in both increasing and decreasing

## Conclusion: will need to include year as a predictor to account for any trend in abundance
## Set future projections to "2022" for this variable i.e. project from most recent abundance estimates
## Trend may reflect multiple factors (e.g. wintering ground habitat change, climate change). Can't identify and so results have this caveat. 

#############################################################
## (1) Bring in climate normals for the survey locations
##############################################################

FolderPath<-paste(dir,'/PresentDayNormals/',sep="")
setwd(FolderPath)

# Import 2 files: 1961 & 1991 30-yr Normals------------------------------------
PCNorms<-list.files(pattern="*data.csv")
PointClimate<-list()
for (j in 1:length(PCNorms)) {
  PointClimate[[j]]<-setDT(read.csv(PCNorms[[j]]))
}

# Average these climate periods for use in tree # optimization by species--------------------------------------------------
ClimateAv<-do.call(rbind, PointClimate)
ClimateAv<-aggregate(ClimateAv[,-1],list(ClimateAv$LocationID), mean)
head(ClimateAv)

#################################################################################
##(2) Add spatial grid to the survey locations, split by CEC Level 2            #
#################################################################################

dggs <- dgconstruct(spacing = 4) # 10.66 km2 - 3.2 km2 spacing
broadarea <- dgconstruct(spacing = 20) #288 km2 - 17km spacing
#SpatialFold<- dgconstruct(spacing = 300) #69967 km2/261 km spacing

################################################################################################
#Stralberg et al. 2015 used 4km grid: "We defined sampling units as the combination of the
#site (route, plot, or other local grouping of point counts) and 4-km grid cell (n=39 186 total 
#sampling units); from each sampling unit with >10 surveys, we randomly selected a single 
#point-count survey in each bootstrap iteration. This was to minimize spatial autocorrelation
#in surveys among points at the same site and temporal autocorrelation among surveys at the same 
#point. We accounted for additional spatial autocorrelation among nearby sampling units by 
#weighting the selection probabilities of each sampling unit by the inverse of the total number of 
#surveys within the 20 X 20 km area surrounding the sampling unit. We minimized the influence of 
#single data points by randomly selecting only one-third of the sampling units with <10 surveys 
#in each bootstrap replicate."
################################################################################################

setwd(dir)
Surveys<-readRDS(paste(dir,"/QPAD_output/BOCH_Data.rds",sep="")) #any spp, just to get survey locations
Surveys<-unique(Surveys[c(1,6:7,11)])
length(unique(Surveys$LocationID)) #63681 locations

Surveys$cell <- dgGEO_to_SEQNUM(dggs, Surveys$lon, Surveys$lat)$seqnum
Surveys$broadarea <- dgGEO_to_SEQNUM(broadarea, Surveys$lon, Surveys$lat)$seqnum

## survey locations in 4km grid --------------------------------------------------
Table<-summaryBy(LocationID~cell,FUN=length,data=unique(Surveys[c(1,5)])) #1-121

## Sampling of broader (20km) area--------------------------------------------------
Table2<-summaryBy(SurveyID~broadarea,FUN=length,data=Surveys) #1-1005 surveys in the the 20km range

#Add this data and weight by regional survey number--------------------------------------------------
Surveys<-merge(Surveys,Table,by="cell")
Surveys<-merge(Surveys,Table2,by="broadarea")
names(Surveys)[names(Surveys) == 'LocationID.length'] <- 'SitesPer4km'
names(Surveys)[names(Surveys) == 'SurveyID.length'] <- 'SiteSurveysPer20km'
Surveys$weights<-1/Surveys$SiteSurveysPer20km #inverse weighting of heavily sampled regions

### Bring in CEC boundaries--------------------------------------------------
setwd('/Users/annadrake/Desktop/Yukon Project Data/Mapping Resources/')

CEC<-st_read('NA_CEC_Eco_Level2/NA_CEC_Eco_Level2.shp')
crs<-'+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs' # CRS of climate rasters
CEC <- st_transform(CEC, crs) 

setwd(dir)

# Survey points--------------------------------------------------
xy<-Surveys[4:5]
coordinates(xy) <- ~ lon + lat
proj4string(xy) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Match projection-----------------------------------------------------------
xy <- st_as_sf(xy)
xy <- st_transform(xy,crs)

CECgroup<-st_join(xy,CEC)
CECgroup <- CECgroup %>% st_drop_geometry()
Surveys<-cbind(Surveys,CECgroup$NA_L2CODE)
names(Surveys)[names(Surveys)=="CECgroup$NA_L2CODE"]<-"CEC_L2"

#Some sites fail to be assigned - all are Marine West (7.1) - Small mismatch between coordinates and CEC coastal boundary so in ocean
Surveys$CEC_L2<-ifelse(is.na(Surveys$CEC_L2),"7.1",Surveys$CEC_L2)

#check very densely sampled sites
#summary(subset(Surveys,Surveys$SitesPer4km>50))#53-121 sites per 4km, 264-1005(??) surveys/20km2
#v_crowded<-subset(Surveys,Surveys$SiteSurveysPer20km>700) #one Big Grid in AB, 1005 surveys==Vancouver area (eBird) and Comox

## Reassign any cells that are split across CECs-----------------------------------
CECs<-summaryBy(CEC_L2~cell,FUN=c(min,max),dat=Surveys)
CECs$diff<-CECs$CEC_L2.max!=CECs$CEC_L2.min
nrow(subset(CECs,CECs$diff==TRUE)) # 70 cells are split by CECs 

Surveys<-merge(Surveys, CECs, by="cell")
Surveys$CEC_L2<-Surveys$CEC_L2.max #assign to the upper CECs
Surveys<-unique(Surveys[c(1,3:10)])
Surveys$pts_rare<-ifelse(Surveys$SitesPer4km<5,1,0)
setDT(Surveys)
#write.csv(Surveys,"Surveys.csv")

rm("Table","Table2","CEC","CECs","CECgroup","dggs","broadarea")

######################## End of data prep ########################

setwd(dir)

#### Assign CEC categories into blocks--------------------------------------------------
Surveys<-read.csv("Surveys.csv")

#assign 10 folds - 12 Ecozones but 3 Prairie/Tundra & poorly sampled: group with neighbor
CEC_L2<-data.frame(unique(Surveys$CEC_L2))
CEC_L2$Group<-c(1,2,1,3,4,5,6,1,7,6,8,10,9,9) #merge CEC 2s, merge 9.2 & 9.3 with 5.4
SurveysG<-merge(Surveys,CEC_L2,by.x="CEC_L2",by.y="unique.Surveys.CEC_L2.")
summaryBy(CEC_L2~Group,SurveysG, FUN=length) #min of 400 PCs in CEC 2, max of 59014 in CEC 6.2
rm("Surveys")

###########################################################################
# (3) Elements needed for BRT & Tuning
###########################################################################

Predictors<-c("bFFP","CMD","eFFP","FFP","MAT","MCMT","MWMT","NFFD",          
              "PPT_sm","PPT_wt","RH","tpi","eastness","northness","tri",
              "Thaw", "year","Cat","offset(offset)") #18 predictors


### BRT tuning:
Shrinkage<-read.csv("TuningParameters.csv")  # load previously-tuned shrinkage values
#Ntrees<-read.csv("Ntrees.csv")  # load previously-estimated optimal tree number (derived in Tuning, below)
Spp<-Shrinkage$Sp
Shrinkage<-Shrinkage$Shrinkage

###  Survey categories: PC, ARU --------------------------------------------------
PCs<-c("BC BBA","MntBird_KM","AlaskanPointCounts","eBird","MntBird_KM","CWS_BMP_PC","SeismicLines","NTBMS2019_NWT")
ARU<-c("CWS_ARU","BCMntKM_ARU","ARU_WBF_JNP_BG")

###########################################################################
# (4) Tuning: get optimal tree number/appropriate shrinkage for each species
#  (Using *"average"* of present climate Normals for prediction
#  as using a particular period may bias' subsequent fit)
###########################################################################

perf_gbm<-c()
 
# Formula 
clim_formula <- as.formula(paste("count ~ ", paste(Predictors, collapse= "+")))

# Run through species...

set.seed(1)

for (i in c(1:length(Spp))) {#  loop 1 - run through each species

  file<-paste(dir,"/QPAD_Output/",Spp[i],"_Data.rds",sep="")
  Species<-setDT(readRDS(file))
  Species<-Species[,.SD, .SDcols=c("LocationID","SurveyID","Source","year","count","correction","offset")]
  Species$Cat<-as.factor(ifelse(Species$Source %in% PCs,1, ifelse(Species$Source %in% ARU,2,ifelse(Species$Source=="BBS",3,NA))))

  Species<-merge(Species,SurveysG, by=c("SurveyID","LocationID"), all.x=TRUE, all.y=FALSE) #Add regional effort

  ### Use *all data* but sub-sample to deal with clustered data
  
  SppClim<-merge(Species,ClimateAv,by.x="LocationID",by.y="Group.1", all.x=TRUE, all.y=FALSE)
  rm("Species")
  
  # random sample from each cell within rare and commonly sampled areas
  training_rare<- SppClim %>% subset(pts_rare==1) %>% group_by(cell) %>% sample_n(size = 1) %>% ungroup()   #9659
  training_common <- SppClim %>% subset(pts_rare==0) %>% group_by(cell) %>% sample_n(size = 3) %>% ungroup()  #14865
  
  #filter using weights for common sites
  training_rare<-training_rare[sample(seq_len(nrow(training_rare)),(nrow(training_rare))*0.6),] #cut 1/3 of rare points so they aren't always in the models
  training_common<-training_common[sample(seq_len(nrow(training_common)),(nrow(training_common))*0.3),prob=training_common$weights,] # cut 2/3 of common points using weights for selection prob
  training<-data.frame(rbind(training_rare,training_common)) #~10,000 obs
  
  ### Run 10 fold CV
  
  TreeNumber <- gbm(clim_formula, 
                    data=training, distribution="poisson", n.trees=10000, shrinkage = Shrinkage[i],             
                    interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
                    n.minobsinnode = 10, cv.folds = 10, keep.data = FALSE,  #still running internal CV
                    verbose = FALSE, n.cores = 4)
  
  ### Est. optimal tree number
 
  perf_gbm[i] <- gbm.perf(TreeNumber, method = "cv", plot.it = FALSE) 
  setTxtProgressBar(pb, i)  
} # close loop 1

########################################################################
#Initial tuning: adjusted shrinkage & re-ran until species had greater than 1000 trees but didn`t
#max out - manually corrected shrinkage in the .csv

#Stralberg et all 2015: To ensure that the optimal number of 
#trees could be found (Elith et al. 2008), we increased the learning rate 
#to 0.005 if the limit of 10000 trees was achieved, and reduced it to 0.0001 
#if fewer than 1000 trees were obtained.

Ntrees<-perf_gbm #est optimal tree number based on fit above
write.csv(Ntrees,"Ntrees.csv")

#############################################################################################################
##  (5) Run Bootstrapped BRT model                                                                         ##
#############################################################################################################

setwd(dir)

#Bring in tuned parameters-----------------------------------------------
#Shrinkage<-read.csv("Shrinkage.csv")  # load previously-tuned shrinkage values
#Ntrees<-read.csv("Ntrees.csv")  # load previously-estimated optimal tree number (derived in Tuning, below)
#Tuning<-cbind(Shrinkage,Ntrees)
#write.csv(Tuning,"TuningParameters.csv")

# Post-hoc removal of out of species that are too much on the edge of the region-----------------------------------------------
# VESP, REVI, EUST, EVGR,  HOSP, MAWA BAWW,  BBWA, AMRE, CMWA, CCSP, OVEN, BHVI, CAWA, PAWA, MOWA, RBGR, WIWR, SWSP, PIWO

Tuning<-read.csv("TuningParameters.csv")
Spp<-Tuning$Sp
Shrinkage<-Tuning$Shrinkage
Ntrees<-Tuning$Trees

###  Survey categories: PC, ARU-----------------------------------------------
PCs<-c("BC BBA","MntBird_KM","AlaskanPointCounts","eBird","MntBird_KM","CWS_BMP_PC","SeismicLines","NTBMS2019_NWT")
ARU<-c("CWS_ARU","BCMntKM_ARU","ARU_WBF_JNP_BG")

CurrentNames <- str_remove(PCNorms,"_data.csv")
Clim<-rep(CurrentNames,each=100) #For ANOVA

#############################
##  Loop 1: Call Species   ##
#############################

set.seed(1)
AllSppFit<-list()

for (i in c(1:54)){# LOOP 1 open: run through each species 
  file<-paste(dir,"/QPAD_Output/",Spp[i],"_Data.rds",sep="")
  Species<-setDT(readRDS(file))
  Species$Cat<-as.factor(ifelse(Species$Source %in% PCs,1, ifelse(Species$Source %in% ARU,2,ifelse(Species$Source=="BBS",3,NA))))
  Species<-Species[,.SD, .SDcols=c("LocationID","SurveyID","Cat","year","count","correction","offset")]

# Set up species-specific Output-----------------------------------------------
GBM<-list()
#Influence<- lapply(1:4, function(x) {data.table(var=c(rep(NA,18)))})
Influence<- data.table(var=c(rep(NA,18)))
PerformanceTest<-list()
FitSummary<-list()
ANOVA<-c()

###########################################
## Loop 2 - call present climate normal ###
###########################################

#for (j in c(1:2)) { #LOOP 2 open: run through 2 current climate normal periods, (currently running PointClimate[[1]] only)

# Merge datasets-----------------------------------------------
SppClim<-merge(Species,PointClimate[[1]],by="LocationID", all.x=TRUE, all.y=FALSE) #PointClimate[[j]]

mean<-round(mean(SppClim$count/SppClim$correction),4) #mean density for error scaling

# Set up climate-norm-specific output - pre-fill to speed up-----------------------------------------------
gbm1<-list()
Infl<-c(rep(NA,900)) #18 variables*50  
var<-c(rep(NA,900))
Predicted<-c(rep(NA,100))
Actual<-c()

# Performance metrics----------------------------------------------- 
RMSE<-c(rep(NA,100))
Bias<-c(rep(NA,100))
PER<-c(rep(NA,100))


##############################################
## RUN BRT over 10*5 testing-training sets  ##
##############################################

for (k in 1:10) { # 10 iterations of testing and training

# split the testing and training data using the spatial blocks ----------------------------------------------- 
  Test <-subset(SurveysG,SurveysG$Group %in% k)
  Testing<-Test$SurveyID # set aside spatial block
  Training<-subset(SurveysG,SurveysG$Group %notin% k)
  
#bootstrap within testing-training 5*-----------------------------------------------
for (y in 1:5) {

# create iteration index:
z<-(k-1)*5 + y  
progress(z, max.value=50)

# take random sample from each cell within rare (<5 survey locations/10.7km2) and commonly sampled areas-----------------------------------------------
  training_rare<- Training %>% subset(pts_rare==1) %>% group_by(cell) %>% sample_n(size = 1) %>% ungroup()   #8735
  training_common <- Training %>% subset(pts_rare==0) %>% group_by(cell) %>% sample_n(size = 3) %>% ungroup()  #12834 (sampling more here but cutting more in next step)

# filter further using weights for common sites-----------------------------------------------
  training_rare<-training_rare[sample(seq_len(nrow(training_rare)),(nrow(training_rare))*0.6),] #cut 1/3 of rare points so they aren't always in the models
  training_common<-training_common[sample(seq_len(nrow(training_common)),(nrow(training_common))*0.3),prob=training_common$weights,] # cut 2/3 of common points using weights for selection probability
  training<-c(training_rare$SurveyID,training_common$SurveyID) #9091 obs
  
rm("training_rare","training_common")  

#SitesUsed <-append(SitesUsed,unique(training$LocationID)) # keeping track of the frequency of use of each site in the dataset


#Model Run - get RMSE and relative Influence of variables over 10 testing-training folds-----------------------------------------------

Tr<-subset(SppClim, SppClim$SurveyID %in% training) # Training subset
x<-round(0.5*nrow(Tr),0) #1/2 the data set for "nTrain" ("train.fraction" deprecated)

gbm1[[z]]<-gbm.fit(Tr[,.SD, .SDcols=Predictors[-19]], Tr$count, offset = Tr$offset, 
                 distribution="poisson", n.trees=Ntrees[i], shrinkage = Shrinkage[i],             
                 interaction.depth = 3, bag.fraction = 0.5, nTrain = x,  
                 n.minobsinnode = 10, keep.data = FALSE, verbose = FALSE)

########################################################################################
## Predict against the held out spatial blocks for performance measure 
##
## https://stat.ethz.ch/pipermail/r-help/2010-September/253647.html ### dealing with offsets
## Just multiply the predicted values by the exposure time or number of cases and that will get you what you want. 
## Note that, depending on the scale of the predictions, you might want to do all the predictions and +log(offset) on the log scale and then exponentiate it. Should be more stable numerically. - Greg
#########################################################################################

TestingSample<-sample(Testing,100,replace=FALSE) #test random sample of 100

Test<-subset(SppClim, SppClim$SurveyID %in% TestingSample)  # testing subset
Predicted <-exp(predict.gbm(object = gbm1[[z]], type="link",
                                            newdata = Test, n.trees = Ntrees[i])) # This output is density/ha 
Actual<-Test$count/Test$correction #-> density/ha 

# Fit-----------------------------------------------

RMSE[z] =Metrics::rmse(Actual, Predicted) #RMSE aims for mean (correct on average) responds to outliers more
PER[z]=Metrics::rmse(Actual, Predicted)/mean #scale by average spp density (=relatively big or small error)
Bias[z]=Metrics::bias(Actual, Predicted)
} # end internal bootstrap

rm("Training")  
} # end k-fold testing-training

#Climate Period Fit Summary-----------------------------------------------
# FitSummary[[j]], CurrentNames[j] if running both climate periods

FitSummary[[1]]<-c(SpList[i],CurrentNames[1],
                               as.numeric(c(round(mean(RMSE,na.rm=TRUE),4),
                               round(mean(PER,na.rm=TRUE),4), 
                               round(mean(Bias,na.rm=TRUE),4),
                               round(quantile(RMSE, prob=0.08,na.rm=TRUE),4),
                               round(quantile(RMSE, prob=0.92,na.rm=TRUE),4))))

#ANOVA<-c(ANOVA,RMSE)
#rm("Actual","Predicted","PER","Bias","RMSE")

# Relative Influence-----------------------------------------------
Infl<-as.numeric(unlist(c(sapply(gbm1, function(x)(summary(x, n.trees = Ntrees[i],plotit = FALSE)[[2]])))))
var<-unlist(c(sapply(gbm1, function(x){summary(x, n.trees = Ntrees[i],plotit = FALSE)[[1]]})))
Influence<-aggregate(Infl,list(var),FUN=mean, na.action = na.omit) # get the 100 run mean variable influence

### nest gbm lists by period to keep them...
GBM<-gbm1
rm("gbm1","var","Infl","SppClim")

FitSummary<-as.data.table(do.call(rbind, FitSummary))
colnames(FitSummary)<-c("Spp","Period","MeanRMSE","MeanError","Bias","RMSE_LWR84CI","RMSE_UPR84CI")

AllSppFit[[i]]<-FitSummary

rm("FitSummary")

# write out models
save(GBM, file=paste(dir,"/BRT_output/NoBarrier1961/",Spp[i],CurrentNames[1],"_GBM_NoBarrier.Rdat",sep=""))
saveRDS(as.data.table(do.call(rbind, AllSppFit)),"./BRT_output/NoBarrier1961/OngoingFitStat_NoBarrier_4.rds")

### Write out variable influence-----------------------------------------

names(Influence)[c(1:2)]<-c("ClimateVar","Norm1961")
#names(InfluenceFile)[c(1:3)]<-c("ClimateVar","Norm1961","Norm1991")
saveRDS(Influence,paste(dir,"/BRT_output/NoBarrier1961/",Spp[i],"VariableInfluence_NoBarrier.rds", sep=""))

rm("Influence","Species")

} # LOOP 1 end: Species loop closed.

################### END OF BRT MODELING ##########