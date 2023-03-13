## Install the required packages
install.packages('corrplot')
install.packages('ggpubr')
install.packages('PresenceAbsence')
install.packages('RColorBrewer')
install.packages('quickPlot')
install.packages('rgdal')
install.packages('vip')
install.packages('pdp')
install.packages('boot')
install.packages('raster')

## Load the installed libraries
library(corrplot)
library(ggpubr)
library("PresenceAbsence")
library(RColorBrewer)
library(quickPlot)
library(rgdal)
library(vip)
library(pdp)
library(boot)
library(raster)

## Set the working directory
setwd('C:\\Users\\rmongare\\OneDrive - International Centre of Insect Physiology and Ecology (ICIPE)\\Desktop\\All\\Raphael\\2022\\Icipe\\Projects\\COMBAT\\Data_and_Maps\\Combat_Raw_Data_Annual\\Processed_V4')

## Load the database
d <- read.csv('Thinned_1Km_rasValues_V5.csv')
names(d)

## Value of inflation (VIF)
## drop the variables based on importance to tsetse flies (retain variables VIF = <10 )
## Do each variable separately per year i.e., 2017, 2018, 2019 and 2020

                         ####   2017    ####


vif_2017 <- VIFcalc(data.frame(d$maxSkinTEMP17,d$minSkinTEMP17,d$medSkinTEMP17,  
                               d$maxSoilTEMP17,d$minSoilTEMP17,d$medSoilTEMP17,
                               d$maxAIRTEMP17,d$minAIRTEMP17,d$medAIRTEMP17,
                               d$maxLSTNight17,d$minLSTNight17,d$medLSTNight17,
                               d$maxLSTDay17,d$minLSTDay17,d$medLSTDay17,
                               d$sumTOTPREC17,d$sumSurfRunoff17,
                               d$maxNDVI17,d$minNDVI17,d$medNDVI17,
                               d$maxSbSMOIST17,d$minSbSMOIST17,d$medSbSMOIST17, 
                               d$maxSSMOIST17,d$minSSMOIST17,d$medSSMOIST17))
                               
vif_2017



## Perform a VIF for retained variables for all years
vif_17_20 <- VIFcalc(data.frame(d$minSkinTEMP17,d$minLSTNight17,d$minLSTDay17,
                                d$sumSurfRunoff17,d$maxNDVI17,d$minNDVI17,
                                d$minSSMOIST17,d$maxSkinTEMP18,d$maxAIRTEMP18,
                                d$maxLSTDay18,d$minLSTDay18,d$maxLSTNight18,
                                d$minLSTNight18,d$sumSurfRunoff18,d$maxNDVI18,
                                d$minNDVI18,d$maxSbSMOIST18,d$minSbSMOIST18,
                                d$maxSSMOIST18,d$minSSMOIST18,d$maxLSTDay19,
                                d$minLSTDay19,d$minLSTNight19,d$maxNDVI19,
                                d$minNDVI19,d$maxSSMOIST19,d$medSSMOIST19,
                                d$maxLSTDay20,d$minLSTDay20,d$minLSTNight20,
                                d$maxNDVI20,d$minNDVI20,d$minSSMOIST20,
                                d$sand_combat,d$NewTWI,d$Kenya_Human_Pop_2020_1km,
                                d$Cattle_Density_2020,d$Sheep,d$Goat,d$EucDist_ProtectedAreas))

vif_17_20


## Explore the retained variables for each year
V17<- stack('maxSkinTEMP17.tif','minSkinTEMP17.tif','medSkinTEMP17.tif',
            'maxSoilTEMP17.tif','minSoilTEMP17.tif','medSoilTEMP17.tif',
            'maxAIRTEMP17.tif','minAIRTEMP17.tif','medAIRTEMP17.tif',
            'maxLSTDay17.tif','minLSTDay17.tif','medLSTDay17.tif',
            'maxLSTNight17.tif','minLSTNight17.tif','medLSTNight17.tif',
            'sumTOTPREC17.tif','sumSurfRunoff17.tif',
            'maxNDVI17.tif','minNDVI17.tif','medNDVI17.tif',
            'maxSbSMOIST17.tif','minSbSMOIST17.tif','medSbSMOIST17.tif',
            'maxSSMOIST17.tif','minSSMOIST17.tif','medSSMOIST17.tif')

plot(V17, 17:26)

V18<- stack('maxSkinTEMP18.tif','minSkinTEMP18.tif','medSkinTEMP18.tif',
'maxSoilTEMP18.tif','minSoilTEMP18.tif','medSoilTEMP18.tif',
'maxAIRTEMP18.tif','minAIRTEMP18.tif','medAIRTEMP18.tif',
'maxLSTDay18.tif','minLSTDay18.tif','medLSTDay18.tif', 
'maxLSTNight18.tif','minLSTNight18.tif','medLSTNight18.tif',
'sumTOTPREC18.tif','sumSurfRunoff18.tif', 
'maxNDVI18.tif','minNDVI18.tif','medNDVI18.tif',
'maxSbSMOIST18.tif','minSbSMOIST18.tif','medSbSMOIST18.tif',
'maxSSMOIST18.tif','minSSMOIST18.tif','medSSMOIST18.tif')

plot(V18, 17:26)

V19<- stack('maxSkinTEMP19.tif','minSkinTEMP19.tif','medSkinTEMP19.tif',  
'maxSoilTEMP19.tif','minSoilTEMP19.tif','medSoilTEMP19.tif',
'maxAIRTEMP19.tif','minAIRTEMP19.tif','medAIRTEMP19.tif',
'maxLSTDay19.tif','minLSTDay19.tif','medLSTDay19.tif',
'maxLSTNight19.tif','minLSTNight19.tif','medLSTNight19.tif',
'sumTOTPREC19.tif','sumSurfRunoff19.tif',
'maxNDVI19.tif','minNDVI19.tif','medNDVI19.tif',
'maxSbSMOIST19.tif','minSbSMOIST19.tif','medSbSMOIST19.tif',
'maxSSMOIST19.tif','minSSMOIST19.tif','medSSMOIST19.tif')

plot(V19)

V20<- stack('maxSkinTEMP20.tif','minSkinTEMP20.tif','medSkinTEMP20.tif',
'maxSoilTEMP20.tif','minSoilTEMP20.tif','medSoilTEMP20.tif',
'maxAIRTEMP20.tif','minAIRTEMP20.tif','medAIRTEMP20.tif',
'maxLSTDay20.tif','minLSTDay20.tif','medLSTDay20.tif',
'maxLSTNight20.tif','minLSTNight20.tif','medLSTNight20.tif',
'sumTOTPREC20.tif','sumSurfRunoff20.tif',
'maxNDVI20.tif','minNDVI20.tif','medNDVI20.tif',          
'maxSbSMOIST20.tif','minSbSMOIST20.tif','medSbSMOIST20.tif',  
'maxSSMOIST20.tif','minSSMOIST20.tif','medSSMOIST20.tif')

plot(V20)

vV <- stack(Elevation,sand_combat,NewTWI,Kenya_Human_Pop_2020_1km,
Cattle_Density_2020,EucDist_ProtectedAreas,Buffalo,Sheep,Goat)

plot(vV)

