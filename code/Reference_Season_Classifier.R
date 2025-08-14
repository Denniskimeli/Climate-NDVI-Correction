# Load required libraries
library(raster)
library(caret)
library(randomForest)
library(sf)
library(e1071)
library(exactextractr)
library(dplyr)
library(tidyr)
library(purrr)

# Read ndvi time series from the reference season
ndvi_RS <- raster::stack("C:/GIS Files/Data_GEE/Research/CSI Correction/S2_monthly_NDVI_2022.tif")
names(ndvi_RS)
# create a variable to store month names
months <- c('March','April','May','June','July','August','September','October')

names(ndvi_RS) <- months
# read RS ground reference data 
samples_RS <- st_read("C:/GIS Files/Data_GEE/Research/ground_truth/Crop_Labels.gpkg")
# head(samples_RS)
# Option 1 Extract training samples using spatial points
# Extract training samples
training_samples <- raster::extract(ndvi_RS,samples_RS, df=TRUE)
head(training_samples)
training_samples$class <- samples_RS$Crop_22
training_samples$class <-as.factor(training_samples$class)
# names(training_samples)[2:9] <- months
head(training_samples)
# remove ID Column from predictors
train_data <- training_samples[, !names(training_samples) %in% "ID"]
# Remove Na's if they exist
train_data <- na.omit(train_data)

# Train and Test Split (70% and 30% respectively)
set.seed(123)  # for reproducibility
train_index <- createDataPartition(train_data$class, p = 0.7, list = FALSE)
train_set <- train_data[train_index, ]
test_set <- train_data[-train_index, ]

# Option 2 Extract training samples using spatial polygons
polys_RS <- st_read("C:/GIS Files/Data_GEE/Research/ground_truth/Polys2.gpkg")
# drop Z dimension
polys_RS <- st_zm(polys_RS)
# Reproject to crs of raster
polys_RS_proj <- sf::st_transform(polys_RS, crs = crs(ndvi_RS))
# Extract raster values at exact polygons, and select only pixels that falls fully inside the polygons
poly_RS_Vals <- exact_extract(ndvi_RS,polys_RS_proj)

# attach values to polygon
poly_RS_df <- map2_df(poly_RS_Vals, seq_along(poly_RS_Vals), ~ 
                          mutate(.x, polygon_id = .y))

head(poly_RS_df)

# select full coverage pixels
full_pixels <- poly_RS_df[poly_RS_df$coverage_fraction == 1, ]

full_pixels <- na.omit(full_pixels)

# check if there are still Na's 
if(any(is.na(full_pixels))) {
  cat("WARNING: NAs found in full_pixels!\n")
  cat("Number of NAs:", sum(is.na(full_pixels)), "\n")
} else {
  cat("No NAs in train_set - good to go!\n")
}


# Add class column to the dataframe

crop_classes <- polys_RS_proj$Crop_22  # or whatever column holds your class
full_pixels$class <- crop_classes[full_pixels$polygon_id]
# check attributes
colnames(full_pixels)
head(full_pixels)
dim(full_pixels)

# remove now unnecessary columns
full_pixels<- full_pixels %>% dplyr::select(-polygon_id,-coverage_fraction)
RS_poly_df_clean = full_pixels

# Check class counts

class_counts <- RS_poly_df_clean %>%
  group_by(class) %>%
  summarise(count = n())
# print the summary of pixel counts per class
print(class_counts)

# Now let's created balanced training samples to avoid bias/over-fitting on one common class
# Create a bar plot to show distribution of each class
RS_poly_df_clean %>% group_by(class) %>% 
  summarise(n = n()) %>% ggplot(aes(y=n, x=class, fill=class)) +
  geom_bar(stat = "identity")

RS_poly_df_clean %>%
  group_by(class) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  summarise(sample_size = min(total)) %>% 
  unlist() -> sample_size

RS_poly_df_clean %>%
  group_by(class) %>% 
  sample_n(sample_size) %>% 
  ungroup() -> samples

samples <- samples %>% mutate(ID = row_number())

train = list()
test = list()

classes = unique(samples$class)

# 70% training, 30% testing
for(i in seq_along(classes)){
  temp = samples[samples$class == classes[i],]
  train[[i]] = sample_frac(temp, 0.7)
  test[[i]] = temp[!(temp$ID %in% train[[i]]$ID),]
}

# drop ID column
train = do.call(rbind.data.frame, train) %>%dplyr::select(-ID)
test = do.call(rbind.data.frame, test) %>% dplyr::select(-ID)

train %>% group_by(class) %>% 
  summarise(n = n())

test %>% group_by(class) %>% 
  summarise(n = n())

head(train)
train$class <- as.factor(train$class)
test$class <- as.factor(test$class)


# train classifiers- Ether using train or train_set (whichever your choice to use points or polygons)
# train classifiers
# Start with Random forest classifier
set.seed(123)
RF_RS <- randomForest(
  class ~ .,                    # Predict class using all other variables
  data = train_set,            # Training data
  ntree = 500,                 # Number of trees
  mtry = sqrt(ncol(train_set)-1), # Number of variables at each split
  importance = TRUE,           # Calculate variable importance
  proximity = TRUE             # Calculate proximity matrix
)

# Print model summary
print(RF_RS)

# Make predictions on test set
predictions <- predict(RF_RS, test_set)

# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, test_set$class)
print(conf_matrix)

# Model accuracy
accuracy <- conf_matrix$overall['Accuracy']
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")




