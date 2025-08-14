# Load required libraries
library(raster)
library(caret)
library(randomForest)
library(sf)
library(e1071)

# Read ndvi time series from the reference season
ndvi_RS <- raster::stack("C:/GIS Files/Data_GEE/Research/CSI Correction/S2_monthly_NDVI_2022.tif")
names(ndvi_RS)
# create a variable to store month names
months <- c('March','April','May','June','July','August','September','October')

names(ndvi_RS) <- months
# read RS ground reference data 
samples_RS <- st_read("C:/GIS Files/Data_GEE/Research/ground_truth/Crop_Labels.gpkg")
# head(samples_RS)
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




