library(caret)
library(class)
library(ggplot2)

# Explore data
summary(healthcare_dataset_stroke_data)

# Data cleaning and preprocessing
sum(is.na(healthcare_dataset_stroke_data))
data <- healthcare_dataset_stroke_data
data <- data[!data$bmi == "N/A", ]
data$bmi <- as.numeric(data$bmi)
data$id <- NULL

# Convert categorical variables to factors
str(data)
data$gender <- as.factor(data$gender)
data$ever_married <- as.factor(data$ever_married)
data$work_type <- as.factor(data$work_type)
data$Residence_type <- as.factor(data$Residence_type)
data$smoking_status <- as.factor(data$smoking_status)
data$stroke <- as.factor(data$stroke)

# Split data into training and testing set
set.seed(1)
splitIndex <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]
train_labels <- data$stroke[splitIndex]

# Scale
train_data[, c("age", "hypertension","heart_disease","avg_glucose_level", 
               "bmi")] <- scale(train_data[, c("age", "hypertension",
                                               "heart_disease","avg_glucose_level", "bmi")])
test_data[, c("age", "hypertension","heart_disease","avg_glucose_level", 
              "bmi")] <- scale(test_data[, c("age", "hypertension",
                                             "heart_disease","avg_glucose_level", "bmi")])

# Training 
knn_model <- knn(train = train_data[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")],
                 test = test_data[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")],
                 cl = train_data$stroke,
                 k = 3)

# Evaluate model
accuracy <- sum(knn_model == test_data$stroke) / length(test_data$stroke)
print(paste("Accuracy:", accuracy))

# Cross validation for optimal k value
folds <- 10
cv_results <- data.frame(k = numeric(0), accuracy = numeric(0))
predictors <- c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")
target <- "stroke"
for (k in 1:20) {
  set.seed(1)
  
  indices <- sample(1:folds, nrow(data), replace = TRUE)
  accuracy <- numeric(0)
  for (fold in 1:folds) {
    train_data <- data[indices != fold, ]
    test_data <- data[indices == fold, ]
    
    knn_model <- knn(train = train_data[predictors], test = test_data[predictors],
                     cl = train_data$stroke, k = k)
    
    accuracy[fold] <- sum(knn_model == test_data$stroke) / length(test_data$stroke)
  }
  
  cv_results <- rbind(cv_results, data.frame(k = k, accuracy = mean(accuracy)))
}
optimal_k <- cv_results$k[which.max(cv_results$accuracy)]
print(paste("Optimal k:", optimal_k))

# Visualize accuracy for various k values
ggplot(cv_results, aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "Cross-Validation Results for Different k Values",
       x = "k (Number of Neighbors)", y = "Average Accuracy")

# Train with optimal k = 13
knn_model <- knn(train = train_data[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")],
                 test = test_data[, c("age", "hypertension", "heart_disease", "avg_glucose_level", "bmi")],
                 cl = train_data$stroke,
                 k = 13)

# Evaluate optimal model
accuracy <- sum(knn_model == test_data$stroke) / length(test_data$stroke)
print(paste("Accuracy:", accuracy))