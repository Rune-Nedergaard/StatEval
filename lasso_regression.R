source("functions.R")

# =======================================================
# Load/prepare data
# =======================================================

# Load cleaned data 
armdata <- readRDS("armdata_cleaned.rds")

armdata[[1]][[1]]

# Define experiment setups 
exp_setups <- data.frame(exp1 = c(15.0, 20),
                         exp2 = c(15.0, 27.5),
                         exp3 = c(15.0, 35),
                         exp4 = c(22.5, 20),
                         exp5 = c(22.5, 27.5),
                         exp6 = c(22.5, 35),
                         exp7 = c(30.0, 20),
                         exp8 = c(30.0, 27.5),
                         exp9 = c(30.0, 35),
                         exp10 = c(37.5, 20),
                         exp11 = c(47.5, 27.5),
                         exp12 = c(37.5, 35),
                         exp13 = c(45.0, 20),
                         exp14 = c(45.0, 27.5),
                         exp15 = c(45.0, 35),
                         exp16 = c(0,0)
)
# Set experiment setup names
row.names(exp_setups) <- c("d","obstacle_height")

# number of experiments (first layer )
n_experiments <- length(armdata);n_experiments

# number of persons (second layer)
n_participants <- length(armdata[[1]]
);n_participants

# number of repetitions (third layer)
n_repetitions <- length(armdata[[1]][[1]]);n_repetitions

# Matrix of path stats 
paths <- c() 

# Loop through experiments
for (i in 1:n_experiments){
  # Loop through participants
  for (j in 1:length(armdata[[i]])){
    # Loop through repetitions 
    for (k in 1:length(armdata[[i]][[j]])){
      # Define repetition
      repetition <- armdata[[i]][[j]][[k]]
     ' 
      for (m in 1:ncol(repetition)){
        for (n in 1:nrow(repetition)){
          if (is.na(repetition[n,m])){
            repetition[n,m] <- sum(repetition[n+1,m]+repetition[n-1,m])/2
          }
        }
      }  
      '
      
      
      # Define coordinates 
      x <- repetition[,1]
      y <- repetition[,2]
      z <- repetition[,3]
      
      # Get x-value of highest point on path (vertex of path)
      xz_vertex <- repetition[which.max(z), 1]
      # Get z-value of highest point on path (vertex of path)
      z_vertex <- repetition[which.max(z), 3]
      # Get x-value where y is max
      xy_max <- repetition[which.max(y),1]
      # Get x-value where y is min
      xy_min <- repetition[which.min(y),1]
      # See how inefficient people move in y-axis
      nulls <- rep(0,length(y))
      #y_shakiness <- abs(nulls-y)
      y_shakiness_mean <- mean(abs(nulls-y))
      y_shakiness_std <- sd(abs(nulls-y))
      
      yz_max <- repetition[which.max(z),2]
      
      
      # par(mfrow = c(1,2))
      # plot(x,y_shakiness)
      # abline(v = xz_vertex)
      # plot(x,z)
      # abline(v = xz_vertex)
      # 
      # Calculate y-value range
      y_range <- abs(max(y) - min(y))
      # y standard deviation
      y_std <- sd(y)
      
      #x standard deviation
      x_std <- sd(x)
      
      #z staandard deviation
      z_std <- sd(z)
      
      z_min <- min(repetition[,3])
      
      # Calculate path height (diference between max and min height)
      path_height <- max(z) - min(z)
      # Define distance between object and 
      d <- exp_setups[[i]][[1]]
      # Define obstacle height
      obstacle_height <- exp_setups[[i]][[2]]
      
      
      
      
      curve_dist <- 0
      prev_point <- NA
      # Loop through all points and sum distance between points
      for (n in 1:nrow(repetition)){
        if (!is.na(sum(prev_point))){
          curve_dist <- curve_dist + getDist3d(repetition[n,], prev_point)    
        }
        
        prev_point <- repetition[n,]
      }
      
      
      paths <- rbind(paths, c(i, d, obstacle_height, j, k, path_height, z_vertex, xz_vertex, xy_max, xy_min, y_shakiness_mean, y_shakiness_std, yz_max, y_range, y_std, x_std, z_std, z_min, curve_dist))
      
    }
  }
}

# Crate dataframe from matrix
paths <- data.frame(paths)

# Add column names
colnames(paths) <- c("Experiment", "d" , "obstacleHeight", "Person", "Repetition", "pathHeight", "zVertex", "xzVertex","xyMax","xyMin","yShakinessMean","yShakinessStd","yzMax", "yRange", "yStd","xStd","zStd", "zMin", "pathDist")
head(paths)

# Define variables as factors
paths$Experiment <- as.factor(paths$Experiment)
paths$Person <- as.factor(paths$Person)
paths$Repetition <- as.factor(paths$Repetition)
paths$d <- as.factor(paths$d)
paths$obstacleHeight <- as.factor(paths$obstacleHeight)



###########################################################
#                       MODELLING                         #
###########################################################
require(nnet)
library(modelr)
library(purrr)
library(dplyr)
library(ggplot2)
library(yardstick)
library(MASS)



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

library(glmnet)
require(methods)
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
