# R version: 4.1.3
# This script is for the step-wise regression, training and predicting the GLM model.

# Install the required packages

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
 install.packages('foreign')
 install.packages('sdm')
 install.packages('gbm')

# Load the needed libraries

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
library(foreign)
library(sdm)
library(gbm)
library(rgdal)
library(PresenceAbsence)

# set the working directory
setwd('C:\\Users\\rmongare\\OneDrive - International Centre of Insect Physiology and Ecology (ICIPE)\\Desktop\\All\\Raphael\\2022\\Icipe\\Projects\\COMBAT\\Data_and_Maps\\Combat_Raw_Data_Annual\\Cummulative\\ProCumm')
list.files()

# load the database
d <- read.csv('combat_annual_cumm_values.csv')
names(d)

# Backward stepwise regression for glm model

glm <- glm(Occ ~ maxSkinTEMP17+minSkinTEMP17+minLSTNight17
           +minLSTDay17+sumSurfRunoff17+maxNDVI17+minNDVI17+maxSbSMOIST17
           +minSSMOIST17+medSSMOIST17+maxLSTDay18+minLSTDay18
           +minLSTNight18+sumSurfRunoff18+maxNDVI18+minNDVI18+medNDVI18
           +minSbSMOIST18+maxSSMOIST18+minSSMOIST18+maxLSTDay19+minLSTDay19
           +minLSTNight19+maxNDVI19+minNDVI19+medSbSMOIST19+maxSSMOIST19
           +minLSTDay20+minLSTNight20+maxNDVI20+minNDVI20+minSbSMOIST20  
           +Elevation+sand_combat+NewTWI+Kenya_Human_Pop_2020_1km
           +Cattle_Density_2020+Sheep+Goat,data=d, family= binomial(link = "logit"))
             
glm.step <- step(glm)
glm.vip <- vi(glm.step)
glm.vip
summary(glm.step)

## GLM Models Multicollinearity + MR####
## the glm models uses the variables retained after applying the stap function
glm.kenya <-sdmData(formula = Occ ~ maxNDVI17 + minNDVI17 + minSSMOIST17 + 
                      medSSMOIST17 + maxLSTDay18 + minLSTDay18 + minLSTNight18 + 
                      maxNDVI18 + minNDVI18 + medNDVI18 + minSSMOIST18 + 
                      minLSTDay19 + minLSTNight19 + maxNDVI19 + medSbSMOIST19 + 
                      minLSTDay20 + maxNDVI20 + sand_combat + NewTWI + 
                      Kenya_Human_Pop_2020_1km + Cattle_Density_2020 + Sheep + 
                      Goat+Elevation,train=d)

sdmglm.kenya <-sdm(Occ~.,data=glm.kenya,methods=c('glm'), replication='cv',test.percent=30, cv.folds=10)
accglm.kenya<- getEvaluation(sdmglm.kenya,wtest="test",opt="max(se+sp)",stat=c("sensitivity","specificity", "AUC","TSS","prevalence","Kappa"))
colMeans(accglm.kenya[sapply(accglm.kenya, is.numeric)])
accglm.kenya
vi.kenyaglm <- getVarImp(sdmglm.kenya, method='glm',wtest="train")
vi.kenyaglm

### Extrapolate the Multicollinerity + MRpredictions####
## stack the variables ###
Varglm <-stack('maxNDVI17.tif','minNDVI17.tif','minSSMOIST17.tif', 
                 'medSSMOIST17.tif','maxLSTDay18.tif','minLSTDay18.tif','minLSTNight18.tif', 
                 'maxNDVI18.tif','minNDVI18.tif','medNDVI18.tif','minSSMOIST18.tif', 
                 'minLSTDay19.tif','minLSTNight19.tif','maxNDVI19.tif','medSbSMOIST19.tif', 
                 'minLSTDay20.tif','maxNDVI20.tif','sand_combat.tif','NewTWI.tif', 
                 'Kenya_Human_Pop_2020_1km.tif','Cattle_Density_2020.tif','Sheep.tif',
                 'Goat.tif','Elevation.tif')
plot(Varglm,1:16)

start_time <- Sys.time()
glmkenya.pred <- predict(sdmglm.kenya,newdata=Varglm,filename='glm_combat_annual_UV4three.tif',mean=TRUE)
end_time <- Sys.time()
end_time - start_time
plot(glmkenya.pred)

### Optimal thresholds for the model based on validation dataset####
set.seed(1234)
d$randomocc<-runif(length(d$Occ)) 

####Divide training and testing
trainKenya<-d[d$randomocc<0.70,] ## the training database (70%)
evaluKenya<-d[d$randomocc>=0.70,] ## the evaluation database (30%)
####***********************Teneral**************************
evaluKenya$p.logisticKenya<-predict(sdmglm.kenya ,type="response",newdata=evaluKenya)
pa.validateKenya<-data.frame(length(evaluKenya$Occ),
                         PA=evaluKenya$Occ,
                         logistic=evaluKenya$p.logisticKenya)

optKenya<- as.data.frame(optimal.thresholds(pa.validateKenya))
data.frame(ID=optKenya[,1], Means=rowMeans(optKenya[,-1]))
