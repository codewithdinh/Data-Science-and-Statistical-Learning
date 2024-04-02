dat <- healthcare_dataset_stroke_data # Loads the healthcare dataset into a variable named dat.
View(dat)
dat <- dat[!dat$bmi == "N/A", ] # Removes rows where BMI is not available.
dat$id <- NULL #Removes the 'id' column from the dataset.
dat <- dat[!dat$gender == "Other",] #Removes rows where gender is labeled as "Other".
cols <- c("gender", "hypertension", "heart_disease", "ever_married", "work_type", "Residence_type", "smoking_status") #Convert some columns to factors.
dat[cols] <- lapply(dat[cols], factor) 

dat$bmi <- as.numeric(dat$bmi) #Convert BMI column to numeric.
dat$stroke <- as.factor(dat$stroke) #Convert  stroke column to factor for proper classification.

n <- nrow(dat)
set.seed(1)
train <- sample(1:n, 0.8*n)

library(e1071)
set.seed(1)

# function to perform hyperparameter tuning for the SVM model. It tests different combinations of the cost and gamma parameters for the radial kernel.
tune.out <- tune(svm, stroke~., data = dat[train,], kernel = "radial", ranges =
                   list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3))) 
summary(tune.out)

#Predicts the stroke outcomes for the test data using the best model obtained from hyperparameter tuning.
svm.predict <- predict(tune.out$best.model, newdata = dat[-train,])

#Calculates the error rate by comparing predicted outcomes with the actual outcomes for the test data.
mean(svm.predict != dat$stroke[-train])

#Fits an SVM model with radial kernel on the entire dataset using the best parameters obtained from hyperparameter tuning.
svm.radial <- svm(stroke~., data=dat, kernel="radial", cost = 0.1, gamma = 0.5)
summary(svm.radial)

#Visualizes the SVM decision boundaries on different pairs of features
plot(svm.radial, dat, age~bmi)
plot(svm.radial, dat, age~avg_glucose_level)
plot(svm.radial, dat, bmi~avg_glucose_level)