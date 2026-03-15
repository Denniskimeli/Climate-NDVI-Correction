
# Climate Similarity NDVI Correction for Crop Mapping

This repository implements a Climate Similarity Index (CSI) based NDVI correction workflow for crop type mapping using remote sensing and climate data.

The goal is to improve crop classification in a Target Season (TS) by correcting NDVI using climate differences relative to a Reference Season (RS).

The corrected NDVI is then classified using models trained on the Reference Season.

------------------------------------------------------------

WORKFLOW OVERVIEW

1. Load climate raster stacks for Reference Season (RS) and Target Season (TS)
2. Compute climate differences between seasons
3. Generate random spatial sampling points
4. Extract NDVI and climate difference values
5. Train Random Forest regression models to explain NDVI change
6. Identify important climate variables
7. Predict NDVI corrections across the study area
8. Correct Target Season NDVI time series
9. Apply Reference Season classifiers to generate crop maps

------------------------------------------------------------

CLIMATE VARIABLES Evaluated

pdsi  - Palmer Drought Severity Index
tmmx  - Maximum temperature
tmmn  - Minimum temperature
vap   - Vapor pressure
vpd   - Vapor pressure deficit
soil  - Soil moisture
aet   - Actual evapotranspiration
def   - Climatic water deficit
pet   - Potential evapotranspiration
pr    - Precipitation
ro    - Runoff
srad  - Solar radiation
swe   - Snow water equivalent
vs    - Wind speed

Each variable is stored as a monthly raster stack.

------------------------------------------------------------

REQUIRED R PACKAGES

install.packages(c(
'raster',
'sp',
'randomForest',
'dplyr',
'ggplot2'
))

library(raster)
library(sp)
library(randomForest)
library(dplyr)
library(ggplot2)

------------------------------------------------------------

DATA PATHS

path_RS <- 'paste your RS climate path here'
path_TS <- 'paste your TS climate path here'

vars <- c(
'pdsi','tmmx','vap','vpd','soil','aet','def','pet',
'pr','ro','srad','swe','tmmn','vs'
)

------------------------------------------------------------

LOAD CLIMATE DATA

load_climate_data <- function(path, season){

  raster_list <- list()

  for(var in vars){

    file_path <- file.path(path,paste0(var,'_',season,'_stack.tif'))

    r <- stack(file_path)

    names(r) <- months

    raster_list[[var]] <- r
  }

  return(raster_list)
}

climate_RS <- load_climate_data(path_RS,'RS')
climate_TS <- load_climate_data(path_TS,'TS')

------------------------------------------------------------

COMPUTE CLIMATE DIFFERENCES

climate_diffs <- lapply(vars,function(var){

  diff_raster <- climate_RS[[var]] - climate_TS[[var]]

  names(diff_raster) <- paste0(months,'_',var)

  return(diff_raster)

})

climate_diff_stack <- stack(climate_diffs)

------------------------------------------------------------

GENERATE RANDOM SAMPLING POINTS

random_points <- spsample(
as(extent(ndvi_RS),'SpatialPolygons'),
n = 2000,
type = 'random'
)

crs(random_points) <- crs(ndvi_RS)

coords <- coordinates(random_points)

random_points <- random_points[!duplicated(coords),]

------------------------------------------------------------

EXTRACT RASTER VALUES

climate_diff_df <- as.data.frame(
extract(climate_diff_stack,random_points)
)

NDVI_diff_df <- as.data.frame(
extract(ndvi_RS - ndvi_TS,random_points)
)

------------------------------------------------------------

TRAIN RANDOM FOREST MODEL

for(month in months){

y <- NDVI_diff_df[[month]]

valid_idx <- which(!is.na(y))

X <- climate_diff_df[valid_idx,]

y_clean <- y[valid_idx]

rf <- randomForest(
x = X,
y = y_clean,
ntree = 500,
importance = TRUE
)

}

------------------------------------------------------------

IDENTIFY IMPORTANT CLIMATE VARIABLES

imp <- as.data.frame(rf$importance)

imp$var_group <- sub('.*_','',rownames(imp))

agg_imp <- imp %>%
group_by(var_group) %>%
summarise(
total_IncMSE = sum(`%IncMSE`)
) %>%
arrange(desc(total_IncMSE))

------------------------------------------------------------

SELECT TOP CLIMATE DRIVERS

Imp_vars <- agg_imp$var_group[
rank(-agg_imp$total_IncMSE) <= 5
]

Imp_climate_vars_stack <- stack(climate_diffs[Imp_vars])

------------------------------------------------------------

PREDICT NDVI CORRECTION

pred_stack <- stack()

for(month in months){

rf <- randomForest(
x = X,
y = y_clean,
ntree = 500
)

pred_r <- raster::predict(
Imp_climate_vars_stack,
rf
)

names(pred_r) <- month

pred_stack <- stack(pred_stack,pred_r)

}

------------------------------------------------------------

CORRECT TARGET SEASON NDVI

ndvi_corrected <- ndvi_TS + pred_stack

------------------------------------------------------------

GENERATE CROP MAPS

TS_Crop_Map_RF <- predict(ndvi_corrected,RF_RS)

TS_Crop_Map_SVM <- predict(ndvi_corrected,SVM_RS)

TS_Crop_Map_NN <- predict(ndvi_corrected,model_nnet)

------------------------------------------------------------

VISUALIZATION

plot(
TS_Crop_Map_RF,
col = class_colors[1:n_classes],
main = 'RF_predictions_corrected',
legend = FALSE
)

------------------------------------------------------------

AUTHOR

Denis Kimeli

Research interests:
Crop type mapping
Machile learning in remote sensing
Climate similarity correction

