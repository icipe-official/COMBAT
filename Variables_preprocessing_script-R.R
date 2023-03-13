# This is script will be used to preprocessing the Environmental variables 
# for tsetse suitability modeling
# The script will perform scaling, projection, cropping, resampling and database
# extraction for all the variables

### Install needed packages
install.packages('rasterVis')
install.packages('gdalUtils', dependencies = TRUE)
install.packages('rgdal')
install.packages('ggplot2')

### load needed librairies
library(raster)
library(rasterVis)
library(gdalUtils)
library(rgdal)
library(ggplot2)

### set working directory ###
setwd('C:/Users/rmongare/OneDrive - International Centre of Insect Physiology and Ecology (ICIPE)/Desktop/All/Raphael/2022/Icipe/Projects/COMBAT/Data_and_Maps/Combat_Raw_Data_Annual')

### Define the projection
sr = "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

### Convert the tsetse occurence points data into shapefiles using QGIS/ArcGIS
### Read the shapefile 'tsetse occurrence points'
tsetse <- readOGR('Tsetse_occurrence.shp')

## Project tsetse shp
tsetse_traps <- spTransform(tsetse,CRS(sr))

## Read the kenya shapefile (Area of interest boundary)
ke <- readOGR('Counties.shp')

### Project kenya shapefile
kenya = spTransform(ke, CRS(sr))
plot(kenya)

plot(tsetse_traps, add = TRUE)

masklayer = raster('mask.tif')
ML <- projectRaster(masklayer, crs = crs(sr)) 
plot(ML)

### Scale the variables that require scaling##
### working with the stacked version is easier.##
skintemp_2017_2020 <- stack('skintemp_combat.tif') - 273.15
soiltemp_2017_2020 <- stack('soiltemp_combat.tif') - 273.15
airtemp_2017_2020 <- stack('airtemp_combat.tif') - 273.15
lst_day_2017_2020 <- stack('lst_day_combat.tif') * 0.02 - 273.15
lst_night_2017_2020 <- stack('lst_night_combat.tif') * 0.02 - 273.15
lst_med_2017_2020 <- stack('lst_med_combat.tif') * 0.02 - 273.15
totprec_2017_2020 <- stack('totprec_combat.tif') * 1000
ndvi_2017_2020 <- stack('ndvi_combat.tif')
sbsmoist_2017_2020 <- stack('sbsmoist_combat.tif')
ssmoist_2017_2020 <- stack('ssmoist_combat.tif')
surfRunoff_2017_2020 <- stack('surfRunoff_combat.tif')

### Stack all of them together###
env_17_20 <- stack(skintemp_2017_2020,soiltemp_2017_2020,airtemp_2017_2020,
                             lst_day_2017_2020,lst_night_2017_2020,lst_med_2017_2020,
                             totprec_2017_2020,ndvi_2017_2020,sbsmoist_2017_2020,
                             ssmoist_2017_2020,surfRunoff_2017_2020)

env_17_20_prj <- projectRaster(env_17_20, crs = crs(sr))
plot(env_17_20_prj)

### process other variable with different extent from the rest of the variables
### mask and crop the files
elev <- raster('Elevation.tif')
elev_prj = projectRaster(elev, crs = crs(sr))
masked <- mask(elev_prj, kenya)
elev_crop <- crop(masked, extent(kenya))

sand <- raster('sand_combat.tif')
sand_prj <- projectRaster(sand, crs = crs(sr))
masked <- mask(sand_prj, kenya)
sand_crop <- crop(masked, extent(kenya))

twi <- raster('NewTWI.tif')
twi_prj <- projectRaster(twi, crs = crs(sr))
masked <- mask(twi_prj, kenya)
twi_crop <- crop(masked, extent(kenya))

pop <- raster('Kenya_Human_Pop_2020_1km.tif')
pop_prj <- projectRaster(pop, crs = crs(sr))
masked <- mask(pop_prj, kenya)
pop_crop <- crop(masked, extent(kenya))

cattle <- raster('Cattle_Density_2020.tif')
cattle_prj <- projectRaster(cattle, crs = crs(sr))
masked <- mask(cattle_prj, kenya)
cattle_crop <- crop(masked, extent(kenya))

protected_areas <- raster('EucDist_ProtectedAreas.tif')
protected_areas_prj <- projectRaster(protected_areas, crs = crs(sr))
masked <- mask(protected_areas_prj, kenya)
protected_areas_crop <- crop(masked, extent(kenya))

buffalo <- raster('Buffalo.tif')
buffalo_prj <- projectRaster(buffalo, crs = crs(sr))
masked <- mask(buffalo_prj, kenya)
buffalo_crop <- crop(masked, extent(kenya))

sheep <- raster('Sheep.tif')
sheep_prj <- projectRaster(sheep, crs = crs(sr))
masked <- mask(sheep_prj, kenya)
sheep_crop <- crop(masked, extent(kenya))

goat <- raster('Goat.tif')
goat_prj <- projectRaster(goat, crs = crs(sr))
masked <- mask(goat_prj, kenya)
goat_crop <- crop(masked, extent(kenya))


### Crop to Kenya counties only
### First mask the files then apply the crop function
masked <- mask(env_17_20_prj, kenya)
env_17_20_crop <- crop(masked, extent(kenya)) ##Crop the masked files
plot(env_17_20_crop)

### Resample the cropped files
extent(protected_areas_crop)## choose one raster file that you want to use as the base for resampling all the layers to
ncol(protected_areas_crop)
nrow(protected_areas_crop)
protected_areas_crop

e = extent(-67084.8, 822915.2, 9479734, 10566734 ) # use the coordinates of the base layer to set the extent
s = raster(e, nrows=1087, ncols=890, crs=protected_areas_crop@crs) #generate a raster with extent and number of columns of the base raster

### Resample the variables
env_17_20.resample<-resample(env_17_20_crop,s,method="ngb")
plot(env_17_20.resample)

elev.resample<-resample(elev_crop,s,method="ngb")
sand.resample<-resample(sand_crop,s,method="ngb")
twi.resample<-resample(twi_crop,s,method="ngb")
human_pop.resample<-resample(pop_crop,s,method="ngb")
cattle.resample<-resample(cattle_crop,s,method="ngb")
protected_areas.resample<-resample(protected_areas_crop,s,method="ngb")
buffalo.resample<-resample(buffalo_crop,s,method="ngb")
sheep.resample<-resample(sheep_crop,s,method="ngb")
goat.resample<-resample(goat_crop,s,method="ngb")

### Stack all variables
combat_annual_var_stack <- stack(env_17_20.resample,elev.resample,sand.resample,
                      twi.resample,human_pop.resample,cattle.resample,
                      protected_areas.resample,buffalo.resample,sheep.resample,
                      goat.resample)
names(combat_annual_var_stack)

### Extract raster values to points
combat_annual_rasValue <- extract(combat_annual_var_stack, tsetse_traps)

### combine the points with the raster values and save as a .csv
combat_annual_rasValues <- cbind(tsetse_traps,combat_annual_rasValue)

## Save the extracted database to a folder

write.table(combat_annual_rasValues,file='combat_annual_rasValues.csv', append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

## Save all variables to a directory
writeRaster(combat_annual_var_stack, names(combat_annual_var_stack), bylayer=TRUE, format='GTiff',overwrite=TRUE)
