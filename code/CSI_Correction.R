
# Load Climate variables e.g terra climate vpd soil tmmx vap, pdsi

# Define paths and variables
path_2022 <- "C:/GIS Files/Data_GEE/Research/CSI Correction/Terra_All/Climate_2022"
path_2023 <- "C:/GIS Files/Data_GEE/Research/CSI Correction/Terra_All/Climate_2023"
vars <- c("pdsi", "tmmx", "vap", "vpd", "soil", "aet", "def", "pet", "pr", "ro", "srad", "swe", "tmmn", "vs")

# Function to load rasters for a given year
load_climate_data <- function(path, year) {
  raster_list <- list()
  for (var in vars) {
    file_path <- file.path(path, paste0(var, "_", year, "_stack.tif"))
    r <- stack(file_path)
    names(r) <- paste0(months)
    raster_list[[var]] <- r
  }
  return(raster_list)
}

# Load data for both years
climate_2022 <- load_climate_data(path_2022, "2022")
climate_2023 <- load_climate_data(path_2023, "2023")

# Calculate differences and create final stack
climate_diffs <- lapply(vars, function(var) {
  diff_raster <- climate_2022[[var]] - climate_2023[[var]]
  names(diff_raster) <- paste0(months, "_", var)
  return(diff_raster)
})
climate_diffs
names(climate_diffs[[14]])


# Stack all differences into one raster
climate_diff_stack <- stack(climate_diffs)

# Check dimensions
print(dim(climate_diff_stack))

# generate random spatial points for use to extract raster values.
random_points <- spsample(as(extent(ndvi_2022), "SpatialPolygons"), 
                          n = 2500, type = "random")
# Assign same CRS as raster
crs(random_points) <- crs(ndvi_RS)
coords <- coordinates(random_points)
unique_idx <- !duplicated(coords)
# Remove duplicates points if any
random_points <- random_points[unique_idx, ]

# Extract sample values in one step
climate_diff_df <- as.data.frame(raster::extract(climate_diff_stack, random_points))
names(climate_diff_df)
dim(climate_diff_df)

# Extract ndvi_diff values using same randomly generated points
ndvi_TS <- raster::stack("C:/GIS Files/Data_GEE/Research/CSI Correction/S2_monthly_NDVI_2023.tif")
names(ndvi_TS) <- months
ndvi_diff <- ndvi_RS-ndvi_TS
names(ndvi_diff) <- months
NDVI_diff_df <- as.data.frame(raster::extract(ndvi_diff, random_points))
dim(NDVI_diff_df)# should be 2500,8
dim(climate_diff_df)# should be 2500,112

# train a random forest regression model to predict change in NDVI over time
for (month in months) {
  message("Processing ", month)
  # Extract the NDVI difference column for this month
  y <- NDVI_diff_df[[month]]
  # Remove missing values
  valid_idx <- which(!is.na(y))
  X <- climate_diff_df[valid_idx, ]
  y_clean <- y[valid_idx]
  
  # Train Random Forest
  rf <- randomForest(x = X, y = y_clean, ntree = 100, importance = TRUE)
}

# Extract variable importance
importance_scores <- importance(rf)

imp <- as.data.frame(rf$importance)
imp$var_group <- sub(".*_", "", rownames(imp))  # if variable name is after underscore

agg_imp <- imp %>%
  group_by(var_group) %>%
  summarise(
    total_IncMSE = sum(`%IncMSE`, na.rm = TRUE),
    total_IncNodePurity = sum(IncNodePurity, na.rm = TRUE)
  ) %>%
  arrange(desc(total_IncMSE))

# Create a variable importance plot using %IncMSE
agg_imp$color_group <- ifelse(rank(-agg_imp$total_IncMSE) <= 5, "steelblue", "grey")

ggplot(agg_imp, aes(x = reorder(var_group, total_IncMSE), y = total_IncMSE)) +
  geom_col(fill = agg_imp$color_group, alpha = 0.7) +
  coord_flip() +  # Horizontal bars for better readability
  labs(title = "Variable Importance (%IncMSE)",
       x = "Climate Variables",
       y = "%IncMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Now let's select the first five important variables as highlighted in Blue!
Imp_vars <- agg_imp$var_group[rank(-agg_imp$total_IncMSE) <= 5]
print("Top 5 variables:")
print(Imp_vars)

# Select only top 5 climate variables from climate_diffs list
names(climate_diffs)<-vars

Imp_climate_vars <- climate_diffs[Imp_vars]
Imp_climate_vars
# Stack only the top 5 variables
Imp_climate_vars_stack <- stack(Imp_climate_vars)
dim(Imp_climate_vars_stack) #should be 8*5=40 bands

# Retrain and predict on the entire stack
# re-extract values and the reduced stack
Imp_climate_diff_df <- as.data.frame(raster::extract(Imp_climate_vars_stack, random_points))
Imp_climate_diff_df<-na.omit(Imp_climate_diff_df)
colnames(Imp_climate_diff_df)
names(Imp_climate_vars_stack)
pred_stack <- stack()

for (month in months) {
  message("Processing ", month)
  
  # Extract the NDVI difference column for this month
  y <- NDVI_diff_df[[month]]
  
  # Remove missing values
  valid_idx <- which(!is.na(y))
  X <- Imp_climate_diff_df[valid_idx, ]
  y_clean <- y[valid_idx]
  
  # Train Random Forest
  rf <- randomForest(x = X, y = y_clean, ntree = 100, importance = FALSE)
  # Predict for full raster stack
  pred_r <- raster::predict(Imp_climate_vars_stack, rf, progress = "text")
  names(pred_r) <- month  # Name the layer
  
  # Add to raster stack
  pred_stack <- stack(pred_stack, pred_r)
}

# Now correct the Target Season Time series
dim(pred_stack)
names(pred_stack)
ndvi_corrected <- ndvi_TS+pred_stack
names(ndvi_corrected) <- months

# Predict on the corrected time series using Reference Classifiers (RF_RS, SVM_RS & NNET_RS)
TS_Crop_Map_RF <- predict(ndvi_corrected,RF_RS)
TS_Crop_Map_SVM <- predict(ndvi_corrected,SVM_RS)
TS_Crop_Map_NN <- predict(ndvi_corrected,model_nnet)

#Plot

plot(TS_Crop_Map_RF , 
     col = class_colors[1:n_classes],
     main = "RF_predictions_corrected",
     legend = FALSE)  # you can add custom legend
plot(TS_Crop_Map_SVM , 
     col = class_colors[1:n_classes],
     main = "SVM_predictions_corrected",
     legend = FALSE) 
plot(TS_Crop_Map_NN , 
     col = class_colors[1:n_classes],
     main = "NN_predictions_corrected",
     legend = FALSE) 
