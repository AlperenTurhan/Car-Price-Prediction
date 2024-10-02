#Importing the dataset
dataset = read.csv('car_price_prediction.csv')

str(dataset)
#Dropping ID column
dataset = subset(dataset, select = -c(ID))
#Checking Duplicated data
sum(duplicated(dataset))
dataset <- dataset[!duplicated(dataset), ]#removing duplicate data

colSums(is.na(dataset))#check count of na values
summary(dataset)
unique(dataset[["Price"]])

unique(dataset[["Levy"]])
dataset$Levy[dataset$Levy == "-"] <- NA#I realize "-" values in this column and convert them to NA
unique(dataset[["Levy"]])
dataset$Levy <- as.numeric(dataset$Levy)
median_levy <- median(dataset$Levy, na.rm = TRUE)#calculating median of Levy column
dataset$Levy[is.na(dataset$Levy)] <- median_levy# filling missing value with median
unique(dataset[["Levy"]])

unique(dataset[["Manufacturer"]])
table(dataset$Manufacturer)
dataset <- dataset[dataset$Manufacturer != "სხვა", ]#remove this manufacturer because I can not find a manufacturer like this
unique(dataset[["Model"]])
unique(dataset[["Prod..year"]])
library(dplyr)
# Creating the Age column by calculating the age of the vehicle from the Prod..year column
library(stringr)
dataset <- dataset %>%
  mutate(Prod_Year = as.integer(str_extract(Prod..year, "\\d+")),
         Age = as.integer(format(Sys.Date(), "%Y")) - Prod_Year) %>%
  select(-Prod..year, -Prod_Year)
head(dataset)
unique(dataset[["Category"]])
unique(dataset[["Leather.interior"]])
unique(dataset[["Fuel.type"]])

unique(dataset[["Engine.volume"]])
dataset$Turbo <- ifelse(grepl("Turbo", as.character(dataset$Engine.volume)), "With Turbo", "Without Turbo")#new column for Turbo
dataset$Engine.volume <- as.numeric(gsub("Turbo", "", as.character(dataset$Engine.volume)))#replace 'Turbo' with space in Engine.volume
unique(dataset[["Engine.volume"]])
table(dataset$Engine.volume)
dataset[dataset$`Engine.volume` == 0, ]
dataset <- dataset[dataset$`Engine.volume` != 0, ]

str(dataset)
sum(dataset == "-")#checking for another missing values
dataset$Mileage <- as.integer(sub(" .*", "", as.character(dataset$Mileage)))#splitting "xxxx km" and converting "xxxx" to integer value 
str(dataset)
unique(dataset[["Mileage"]])
dataset <- dataset[dataset$`Mileage` != 0, ]#Mileage can not be 0 because these cars are second-hand
unique(dataset[["Cylinders"]])
unique(dataset[["Gear.box.type"]])
unique(dataset[["Drive.wheels"]])
unique(dataset[["Doors"]])#some door types are misnamed
dataset$Doors <- ifelse(dataset$Doors == "04-May", 4,
                        ifelse(dataset$Doors == "02-Mar", 2,
                               ifelse(dataset$Doors == ">5", 5, dataset$Doors)))#Replacing the wrong values with true values
dataset$Doors <- as.integer(dataset$Doors)
str(dataset)
unique(dataset[["Doors"]])
unique(dataset[["Wheel"]])
unique(dataset[["Color"]])
unique(dataset[["Airbags"]])

###NEW FEATURE### I created it to help the models, but it didn't help much.
#dataset$Average_Mileage_Per_Year <- dataset$Mileage / dataset$Age
#dataset$Relation_with_levy_and_engine <- dataset$Levy / dataset$Engine.volume
summary(dataset)

# Outlier Detection and Cleaning with IQR 
# Outlier Cleaning for Mileage
Q1 <- quantile(dataset$Mileage, 0.25)
Q3 <- quantile(dataset$Mileage, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

dataset <- dataset[dataset$Mileage >= lower_bound & dataset$Mileage <= upper_bound, ]

# Outlier Cleaning for Price
Q1 <- quantile(dataset$Price, 0.25)
Q3 <- quantile(dataset$Price, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

dataset <- dataset[dataset$Price >= lower_bound & dataset$Price <= upper_bound, ]

# Outlier Cleaning for Levy
Q1 <- quantile(dataset$Levy, 0.25)
Q3 <- quantile(dataset$Levy, 0.75)
IQR_val <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

dataset <- dataset[dataset$Levy >= lower_bound & dataset$Levy <= upper_bound, ]

summary(dataset)
par(mfrow=c(3, 3)) # Creates a 3x3 histogram layout

hist(dataset$Mileage, breaks = 25, col = "lightblue", main = "Mileage Histogram", xlab = "Mileage", ylab = "Frequency")
hist(dataset$Price, breaks = 25, col = "lightgreen", main = "Price Histogram", xlab = "Price", ylab = "Frequency")
hist(dataset$Levy, breaks = 25, col = "lightyellow", main = "Levy Histogram", xlab = "Levy", ylab = "Frequency")
hist(dataset$`Engine.volume`, breaks = 25, col = "lightpink", main = "Engine Volume Histogram", xlab = "Engine Volume", ylab = "Frequency")
hist(dataset$Cylinders, breaks = 25, col = "lightcoral", main = "Cylinders Histogram", xlab = "Cylinders", ylab = "Frequency")
hist(dataset$Airbags, breaks = 25, col = "lightgray", main = "Airbags Histogram", xlab = "Airbags", ylab = "Frequency")
hist(dataset$`Age`, breaks = 25, col = "lightcyan", main = "Age Histogram", xlab = "Age", ylab = "Frequency")
#install.packages("ggplot2")
library(ggplot2)
category_counts <- as.data.frame(table(dataset$Category))
names(category_counts) <- c("Category", "Count")

# Sort data by number of categories
category_counts <- category_counts[order(-category_counts$Count),]

# Creating a color scale
color_palette <- colorRampPalette(c("lightblue", "darkblue"))(nrow(category_counts))

# Create a bar chart
ggplot(category_counts, aes(x = reorder(Category, -Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = color_palette[1], high = color_palette[nrow(category_counts)]) +
  labs(title = "Category", x = "Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


color_counts <- as.data.frame(table(dataset$Color))
names(color_counts) <- c("Color", "Count")

# Sort data by number of categories
color_counts <- color_counts[order(-color_counts$Count),]

#  Creating a color scale
color_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(nrow(color_counts))

# Create a bar chart
ggplot(color_counts, aes(x = reorder(Color, -Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = color_palette[1], high = color_palette[nrow(color_counts)]) +
  labs(title = "Of Colors", x = "Color", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


columns <- c('Leather.interior', 'Fuel.type', 'Gear.box.type', 'Drive.wheels', 'Wheel')

for (col in columns) {
  col_counts <- as.data.frame(table(dataset[[col]]))
  names(col_counts) <- c(col, "Count")
  
  # Sort data by number of categories
  col_counts <- col_counts[order(-col_counts$Count),]
  
  # Creating a color scale
  color_palette <- colorRampPalette(c("lightblue", "darkblue"))(nrow(col_counts))
  
  # Create a bar chart
  p <- ggplot(col_counts, aes(x = reorder(.data[[col]], -Count), y = Count, fill = Count)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = color_palette[1], high = color_palette[nrow(col_counts)]) +
    labs(title = col, x = col, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X ekseni etiketlerini döndürme
  
  print(p) 
}


top_10_cars <- head(names(sort(table(dataset$Manufacturer), decreasing = TRUE)), 10)

# We get the manufacturer numbers
top_10_counts <- as.data.frame(table(dataset$Manufacturer))
names(top_10_counts) <- c("Manufacturer", "Count")
top_10_counts <- top_10_counts[top_10_counts$Manufacturer %in% top_10_cars, ]

# We list manufacturers according to their numbers
top_10_counts <- top_10_counts[order(-top_10_counts$Count),]

# Create a bar chart
p <- ggplot(top_10_counts, aes(x = reorder(Manufacturer, -Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top10 The Most Frequent Cars", x = "Cars", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15, face = "bold"))

print(p)



top_10_cars_means_prices <- sapply(top_10_cars, function(i) {
  mean(dataset$Price[dataset$Manufacturer == i], na.rm = TRUE)
})

# Create a data frame
top_10_cars_prices_df <- data.frame(Cars = names(top_10_cars_means_prices), Average_Price = top_10_cars_means_prices)

# Create a line chart
p <- ggplot(top_10_cars_prices_df, aes(x = reorder(Cars, Average_Price), y = Average_Price)) +
  geom_line(color = "red", linewidth = 4) +
  geom_point(color = "red", size = 10) +
  labs(title = "Top 10 Cars by Average Price", x = "Cars", y = "Average Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 15, face = "bold"))

print(p)

#install.packages("tidyverse")

library(tidyverse)
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
reversed_numeric_cols <- rev(numeric_cols)
dataset_reversed <- dataset[, reversed_numeric_cols]
# Calculating the correlation matrix
cor_matrix_reversed <- cor(dataset_reversed)

# Bringing the correlation matrix to the appropriate format
cor_df_reversed <- as.data.frame(cor_matrix_reversed)
cor_df_reversed$variables <- rownames(cor_df_reversed)

# Visualizing the correlation matrix
cor_df_reversed %>%
  pivot_longer(cols = -variables, names_to = "var2", values_to = "correlation") %>%
  ggplot(aes(x = variables, y = var2, fill = correlation, label = round(correlation, 2))) +
  geom_tile() +
  geom_text(color = "black") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Correlation Heatmap ", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Select categorical columns
obdata <- dataset %>%
  select_if(is.character)

# Loop for label encoding
for (i in 1:ncol(obdata)) {
  obdata[[i]] <- as.integer(as.factor(obdata[[i]]))
}

# Merge categorical columns with label encoded numeric columns
dataset <- cbind(obdata, dataset[, sapply(dataset, is.numeric)])
#install.packages('caTools')
#install.packages("caret")
library(caTools)
# Separating the data set into dependent and independent variables
X <- subset(dataset, select = -c(Price)) # Columns other than Price
y <- dataset$Price # Price column y

# Dividing data into train and test
set.seed(123)
split <- sample.split(y, SplitRatio = 0.75)
train_X <- subset(X, split == TRUE)
train_y <- y[split == TRUE]
test_X <- subset(X, split == FALSE)
test_y <- y[split == FALSE]

# Applying Standard Scaler
scaler <- scale(train_X)
train_X_scaled <- as.data.frame(scaler)
scaler_test <- scale(test_X)
test_X_scaled <- as.data.frame(scaler_test)

#install.packages("caret")
library(caret)
# Creating a cross-validation check
control <- trainControl(method = "cv", number = 10) # 10 katlı cross-validation

# Creating a Linear Regression Model
model <- lm(train_y ~ ., data = train_X_scaled, trControl = control)

# Evaluating the model on the test set
predicted <- predict(model, newdata = test_X_scaled)
R2 <- summary(model)$r.squared
RMSE <- sqrt(mean((test_y - predicted)^2))

# Print results
print(paste("R^2:", R2))
print(paste("RMSE:", RMSE))
#install.packages("rpart")
library(rpart)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating a Decision Tree Regressor Model
tree_model <- rpart(train_y ~ ., data = train_X_scaled, method = "anova")

# Evaluating the model on the test set
predicted_tree <- predict(tree_model, newdata = test_X_scaled)
R2_tree <- cor(predicted_tree, test_y)^2
RMSE_tree <- sqrt(mean((test_y - predicted_tree)^2))

# Print results
print(paste("Decision Tree - R^2:", R2_tree))
print(paste("Decision Tree - RMSE:", RMSE_tree))

####KNN#####
#install.packages("caret")
library(caret)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating a K-Neighbors Regression Model
knn_model <- train(train_X_scaled, train_y, method = "knn",trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = data.frame(k = 8))

# Evaluating the model on the test set
predicted_knn <- predict(knn_model, newdata = test_X_scaled)
R2_knn <- cor(predicted_knn, test_y)^2
RMSE_knn <- sqrt(mean((test_y - predicted_knn)^2))

# Print results
print(paste("K-Neighbors Regression - R^2:", R2_knn))
print(paste("K-Neighbors Regression - RMSE:", RMSE_knn))

#install.packages("e1071")
library(e1071)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating a Support Vector Regression Model
svr_model <- svm(train_y ~ ., data = train_X_scaled, kernel = "radial", trControl = control)

# Evaluating the model on the test set
predicted_svr <- predict(svr_model, newdata = test_X_scaled)
R2_svr <- cor(predicted_svr, test_y)^2
RMSE_svr <- sqrt(mean((test_y - predicted_svr)^2))

# Print results
print(paste("Support Vector Regression - R^2:", R2_svr))
print(paste("Support Vector Regression - RMSE:", RMSE_svr))

####Random Forest####
#install.packages("randomForest")
library(randomForest)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating a Random Forest Regression Model
rf_model <- randomForest(train_X_scaled, train_y, ntree = 500, trControl = control)

# Evaluating the model on the test set
predicted_rf <- predict(rf_model, newdata = test_X_scaled)
R2_rf <- cor(predicted_rf, test_y)^2
RMSE_rf <- sqrt(mean((test_y - predicted_rf)^2))

# Print results
print(paste("Random Forest Regression - R^2:", R2_rf))
print(paste("Random Forest Regression - RMSE:", RMSE_rf))

####XGBoost####
#install.packages("xgboost")
library(xgboost)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating an XGBoost Regression Model
xgb_model <- xgboost(data = as.matrix(train_X_scaled), label = train_y, nrounds = 100, objective = "reg:squarederror", trControl = control)

# Evaluating the model on the test set
predicted_xgb <- predict(xgb_model, as.matrix(test_X_scaled))
R2_xgb <- cor(predicted_xgb, test_y)^2
RMSE_xgb <- sqrt(mean((test_y - predicted_xgb)^2))

# Print results
print(paste("XGBoost Regression - R^2:", R2_xgb))
print(paste("XGBoost Regression - RMSE:", RMSE_xgb))

#install.packages("gbm")
library(gbm)

# Separating the data set into dependent and independent variables
# Train and test sets created in the previous steps will be used

# Creating a Gradient Boosting Regression Model
gbm_model <- gbm.fit(x = as.matrix(train_X_scaled), y = train_y, distribution = "gaussian", n.trees = 100, interaction.depth = 4, shrinkage = 0.01)

# Evaluating the model on the test set
predicted_gbm <- predict(gbm_model, newdata = as.matrix(test_X_scaled), n.trees = 100)
R2_gbm <- cor(predicted_gbm, test_y)^2
RMSE_gbm <- sqrt(mean((test_y - predicted_gbm)^2))

# Print results
print(paste("Gradient Boosting Regression - R^2:", R2_gbm))
print(paste("Gradient Boosting Regression - RMSE:", RMSE_gbm))


######Testing KNN with different k values and cross validation######
library(caret)

# Identifying different k values
k_values <- c(3, 5, 7, 9, 11, 13, 15, 17, 19)

for (k in k_values) {
  # Creating a K-Neighbors Regression Model
  knn_model <- train(train_X_scaled, train_y, method = "knn",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = data.frame(k = k))
  
  # Evaluating the model on the test set
  predicted_knn <- predict(knn_model, newdata = test_X_scaled)
  R2_knn <- cor(predicted_knn, test_y)^2
  RMSE_knn <- sqrt(mean((test_y - predicted_knn)^2))
  
  # Print results
  cat("K =", k, "\n")
  cat("R-squared:", R2_knn, "\n")
  cat("RMSE:", RMSE_knn, "\n\n")
}

###Random Forest with different parameters###
ntree_values <- c(100, 200, 300)
mtry_values <- c(2, 4, 6)
criterion_values <- c("mse", "mae")

# Create empty matrices to store results
results_R2 <- array(NA, dim = c(length(ntree_values), length(mtry_values), length(criterion_values)))
results_RMSE <- array(NA, dim = c(length(ntree_values), length(mtry_values), length(criterion_values)))

for (i in 1:length(ntree_values)) {
  for (j in 1:length(mtry_values)) {
    for (k in 1:length(criterion_values)) {
      # Create the Random Forest model
      rf_model <- randomForest(train_X_scaled, train_y, ntree = ntree_values[i], mtry = mtry_values[j], trControl = control, criterion = criterion_values[k])
      
      #Evaluating the model on the test set
      predicted_rf <- predict(rf_model, newdata = test_X_scaled)
      R2_rf <- cor(predicted_rf, test_y)^2
      RMSE_rf <- sqrt(mean((test_y - predicted_rf)^2))
      
      #Add results to matrix
      results_R2[i, j, k] <- R2_rf
      results_RMSE[i, j, k] <- RMSE_rf
      
      # Parameter combination and printing results
      cat("ntree:", ntree_values[i], ", mtry:", mtry_values[j], ", criterion:", criterion_values[k], "\n")
      cat("Random Forest Regression - R^2:", R2_rf, "\n")
      cat("Random Forest Regression - RMSE:", RMSE_rf, "\n\n")
    }
  }
}
###SVM with different parameters##
kernel_values <- c("linear", "radial")
cost_values <- c(0.1, 1, 10)
gamma_values <- c(0.1, 1, 10)

# Create empty matrices to store results
results_R2 <- array(NA, dim = c(length(kernel_values), length(cost_values), length(gamma_values)))
results_RMSE <- array(NA, dim = c(length(kernel_values), length(cost_values), length(gamma_values)))

for (i in 1:length(kernel_values)) {
  for (j in 1:length(cost_values)) {
    for (k in 1:length(gamma_values)) {
      # Creating the SVM model and evaluating it with cross-validation
      svr_model <- svm(train_y ~ ., data = train_X_scaled, kernel = kernel_values[i], cost = cost_values[j], gamma = gamma_values[k], trControl = control)
      
      # Evaluating the model on the test set
      predicted_svr <- predict(svr_model, newdata = test_X_scaled)
      R2_svr <- cor(predicted_svr, test_y)^2
      RMSE_svr <- sqrt(mean((test_y - predicted_svr)^2))
      
      # Add results to matrix
      results_R2[i, j, k] <- R2_svr
      results_RMSE[i, j, k] <- RMSE_svr
      
      # Parameter combination and printing results
      cat("kernel:", kernel_values[i], ", cost:", cost_values[j], ", gamma:", gamma_values[k], "\n")
      cat("Support Vector Regression - R^2:", R2_svr, "\n")
      cat("Support Vector Regression - RMSE:", RMSE_svr, "\n\n")
    }
  }
}
###XGBOOST with different parameters##
library(xgboost)

# Values at which parameters will be tested
nrounds_values <- c(50, 100, 150)
max_depth_values <- c(3, 6, 9)
eta_values <- c(0.1, 0.3, 0.5)
subsample_values <- c(0.6, 0.8, 1)

# Create a list to store results
results <- list()

for (i in 1:length(nrounds_values)) {
  for (j in 1:length(max_depth_values)) {
    for (k in 1:length(eta_values)) {
      for (l in 1:length(subsample_values)) {
        # Creating the XGBoost model
        xgb_model <- xgboost(
          data = as.matrix(train_X_scaled), label = train_y,
          nrounds = nrounds_values[i],
          max_depth = max_depth_values[j],
          eta = eta_values[k],
          subsample = subsample_values[l],
          objective = "reg:squarederror",
          trControl = control
        )
        
        # Evaluating the model on the test set
        predicted_xgb <- predict(xgb_model, as.matrix(test_X_scaled))
        R2_xgb <- cor(predicted_xgb, test_y)^2
        RMSE_xgb <- sqrt(mean((test_y - predicted_xgb)^2))
        
        # Add results to the list
        results[[paste("nrounds", nrounds_values[i], "_max_depth", max_depth_values[j], "_eta", eta_values[k], "_subsample", subsample_values[l])]] <- list(
          R2 = R2_xgb,
          RMSE = RMSE_xgb
        )
      }
    }
  }
}

# Print results
for (result in names(results)) {
  cat(result, "\n")
  cat("XGBoost Regression - R^2:", results[[result]]$R2, "\n")
  cat("XGBoost Regression - RMSE:", results[[result]]$RMSE, "\n\n")
}
##Random Forest Regression Graph##
predicted_rf <- predict(rf_model, newdata = test_X_scaled)
predictions_df <- data.frame(Actual = test_y, Predicted = predicted_rf)
ggplot(predictions_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Random Forest Regression Graph", x = "Real Values", y = "Predicted Values")

##SVM Regression Graph##
predicted_svm <- predict(svr_model, newdata = test_X_scaled)
predictions_svm_df <- data.frame(Actual = test_y, Predicted = predicted_svm)
ggplot(predictions_svm_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "green") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "SVM Regression Graph", x = "Real Values", y = "Predicted Values")

##K-Nearest Regressor Regression Graph##
predicted_knn <- predict(knn_model, newdata = test_X_scaled)
predictions_knn_df <- data.frame(Actual = test_y, Predicted = predicted_knn)
ggplot(predictions_knn_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "orange") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "KNN Regression Graph", x = "Real Values", y = "Predicted Values")