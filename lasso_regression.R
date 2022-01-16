# =======================================================
# Libraries
# =======================================================
require(nnet)
library(modelr)
library(purrr)
library(dplyr)
library(ggplot2)
library(yardstick)
library(MASS)


library(glmnet)
require(methods)

# Load helper functions from file
source("functions.R")

# =======================================================
# Load data
# =======================================================
paths <- readRDS("paths.rds")
# Define variables as factors
paths$Experiment <- as.factor(paths$Experiment)
paths$Person <- as.factor(paths$Person)
paths$Repetition <- as.factor(paths$Repetition)
paths$d <- as.factor(paths$d)
paths$obstacleHeight <- as.factor(paths$obstacleHeight)


###########################################################
#                       MODELLING                         #
###########################################################

################## STEP AIC ###################
smp_size <- floor(0.75 * nrow(paths))
set.seed(42)
train_idx <- sample(seq_len(nrow(paths)), size = smp_size)
train_data <- paths[train_idx, ]
val_data <- paths[-train_idx, ]

val_data_matrix <- data.matrix(val_data)
val_features <- val_data_matrix[,4:ncol(val_data_matrix)]
val_labels <- val_data_matrix[,1]


############# Lasso regression  ##############

#x <- as.matrix(train_data[,4:]) # all X vars
x <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                  + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                    yStd +xStd +zStd + zMin + pathDist, data = train_data)
y <- train_data$Experiment


#using 10-fold cross validation on training data to pick hyper parameters (lambda value)
XYZ_fit <- cv.glmnet(x, y,
                          alpha = 1, family = "multinomial", type.measure = "class")

plot(XYZ_fit)

#fitting without CV to  see what happens to coefficients
fit <- glmnet(x, y, family = "multinomial", type.multinomial = "grouped")
#When forcing it to either include or exclude a variable for all experiments, it selects: 
#zVertex, xzVertex, yRange, xStd, pathHeight AND an intercept for Person9

#plotting how coefficients change
#fit2 <- glmnet(x, y, family = "multinomial")

par(mfrow = c(4,4))
plot(fit2, xvar = "lambda", label = TRUE, type.coef = "coef")
par(mfrow = c(1,1))

######## Predicting for XYZ ###########

#formatting validation data so it plays well with glmnet
newx <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                     + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                       yStd +xStd +zStd + zMin + pathDist, data = val_data)


#Predicting on validation data using optimal lambda
xyz_predictions <- predict(XYZ_fit, newx = newx, s = "lambda.min", type = "class")



actual_class <- as.integer(val_data$Experiment)
predicted_xyz <- as.integer(xyz_predictions)
tab_class <- table(actual_class, xyz_predictions)


#Shows a lot of info
confusionMatrix(table(predicted_xyz, actual_class), mode = "everything")



######## Defining model with only YZ coordinates ###########
#only including variables that do not contain info on x-axis
x <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex
                    +yShakinessMean +yShakinessStd +yzMax +yRange +
                    yStd +zStd + zMin, data = train_data)

#fitting
YZ.fit <- cv.glmnet(x, y,
                          alpha = 1, family = "multinomial", type.measure = "class")


######## Predicting for YZ ###########
#formatting validation data
newx <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex
                       +yShakinessMean +yShakinessStd +yzMax +yRange +
                       yStd +zStd + zMin , data = val_data)

#Predicting on validation data using optimal lambda
yz_predictions <- predict(YZ.fit, newx = newx, s = "lambda.min", type = "class")


actual_class <- as.integer(val_data$Experiment)
predicted_yz <- as.integer(yz_predictions)
tab_class <- table(actual_class, yz_predictions)

confusionMatrix(table(predicted_yz, actual_class), mode = "everything")


#Comparing XYZ and YZ
correct_xyz <- predicted_xyz == actual_class
correct_yz <- predicted_yz == actual_class

#two-ways of doing McNemar:
#basic
mcnemar.test(table(correct_xyz,correct_yz))
#more info
confusionMatrix(table(correct_xyz,correct_yz), mode = "everything")
