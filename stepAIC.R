# =======================================================
#  Define functions
# =======================================================

# Create jpg file path for plots by providing a plot name
getJpgFilePath <- function(plot_name) {
  plots_path <- "plots"
  file_type <- "jpg"
  path <- paste(paste(plots_path, plot_name, sep="/"), file_type, sep=".")
  
}

getDist3d <- function(v1, v2) {
  dist <- sqrt((v2[1] - v1[1])^2 + (v2[2]- v1[2])^2 + (v2[3]- v1[3])^2)
  return(dist)
}

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
library(broom)
#x <- as.matrix(train_data[,4:]) # all X vars
x <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                  + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                    yStd +xStd +zStd + zMin + pathDist, data = train_data)
y <- train_data$Experiment


#using 10-fold cross validation on training data to pick hyper parameters (lambda value)
fit.lasso.cv <- cv.glmnet(x, y,
                          alpha = 1, family = "multinomial", type.measure = "class")

mod <- glmnet::cv.glmnet(x, y,
                         alpha = 1, family = "multinomial", type.measure = "class")

broom::tidy(coef(mod$glmnet.fit, s = mod$lambda.min, digits = 3))
coef(fit.lasso.cv, s =0)

newx <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
+ xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
  yStd +xStd +zStd + zMin + pathDist, data = val_data)


regularizedpreds <- predict(fit.lasso.cv, newx = newx, s = "lambda.min", type = "class")



fit.lasso.pred <- predict(fit.lasso.best, t)
plot(fit.lasso.cv)

coef(cvfit, cvfit$lambda.min)

# extract optimal lambda
lmabda_opt <- cvfit$lambda.min

fitoptimal <- glmnet(x, y,lambda = lmabda_opt, family = "multinomial") 

plot(fitoptimal)

coef(fitoptimal)


# Fit the LASSO model (Lasso: Alpha = 1)
set.seed(100)
#cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
coef(cv_output, s= "lambda.min")

xnew <- model.matrix(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                     + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                       yStd +xStd +zStd + zMin + pathDist, data = test_data)

predicted <- predict(cv_output, newx = xnew, s = "lambda.min", type = "class")


df_coef <- round(as.matrix(coef(cv_output, s=cv_output$lambda.min)), 2)
df_coef[df_coef[, 1] != 0, ]









############# old stuff ############

folds <- 30
cv <- crossv_kfold(paths, k = folds);cv



get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

######### xyz_full ##########
cvxyz_full <- crossv_kfold(train_data, k = folds)
xyz_full <- map(cvxyz_full$train, ~multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                                            + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                                              yStd +xStd +zStd + zMin + pathDist, data = train_data))


pred <- map2_df(xyz_full, cvxyz_full$test, get_pred, .id = "Fold")
xyz_full_predicted <- data.frame(
  obs = pred$Experiment,
  pred = pred$pred)

xyz_full_predicted$obs <- as.factor(xyz_full_predicted$obs)
xyz_full_predicted$pred <- as.factor(xyz_full_predicted$pred)
mean(xyz_full_predicted$obs == xyz_full_predicted$pred) #60.56%

####### XYZ_selected ########


#stepAIC on xyz_full
xyz_full_nocv <- multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                          + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                            yStd +xStd +zStd + zMin + pathDist, data = paths)



#Selecting features
stepAIC(xyz_full_nocv, direction = "both")
#found Experiment ~ Person + pathHeight + zVertex + xzVertex + xyMin + yShakinessMean + yzMax + yStd+ zStd + zMin + pathDist

#CV on xyz_selected
cvxyz_selected <- crossv_kfold(paths, k = folds)
xyz_selected <- map(cvxyz_selected$train, ~multinom(Experiment ~ Person + pathHeight + zVertex + 
                                                      xzVertex + xyMin + yShakinessMean + yzMax + yStd + zStd + 
                                                      zMin + pathDist, data = .))



pred <- map2_df(xyz_selected, cvxyz_selected$test, get_pred, .id = "Fold")
xyz_selected_predicted <- data.frame(
  obs = pred$Experiment,
  pred = pred$pred)

xyz_selected_predicted$obs <- as.factor(xyz_selected_predicted$obs)
xyz_selected_predicted$pred <- as.factor(xyz_selected_predicted$pred)
mean(xyz_selected_predicted$obs == xyz_selected_predicted$pred) #62.19%







xyz_selected <- multinom(formula = Experiment ~ Person + pathHeight + zVertex + 
                           xzVertex + yShakinessMean + xStd + zStd + zMin, data = train_data)
test_data$xyz_selected <- predict(xyz_selected, newdata = test_data, "class")
mean(test_data$Experiment == test_data$xyz_selected) #61.75% acc

ctable$xyz_selected <- test_data$Experiment == test_data$xyz_selected

ctable(test_data$xyz_selected,test_data$xyz_full)


#XY-coordinates only
xy_full <- multinom(Experiment ~ Person + Repetition
                    + xyMax + xyMin +yShakinessMean +yShakinessStd +yRange +
                      yStd +xStd + zMin, data = train_data)

test_data$xy_full <- predict(xy_full, newdata = test_data, "class")
mean(test_data$Experiment == test_data$xy_full) #19.50% acc







