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


par(mfrow=c(1,1))

plot(paths$Experiment,paths$yShakinessMean, main= "yShakinessMean")
plot(paths$Experiment,paths$yShakinessStd, main= "yShakinessStd")

plot(paths$Experiment,paths$zMin, main= "Zmin")
plot(paths$Experiment,paths$zStd, main= "zStd")

plot(paths$Experiment,paths$pathDist, main= "pathDist")

plot(paths$Experiment,paths$xyMax, main= "xyMax")
plot(paths$Experiment,paths$yRange, main= "yRange")

plot(paths$Experiment,paths$xzVertex, main= "xzVertex")
plot(paths$Experiment,paths$zStd, main= "zStd")


###########################################################
#                       MODELLING                         #
###########################################################


folds <- 30
cv <- crossv_kfold(paths, k = folds);cv
?crossv_kfold

################## STEP AIC ###################
#These are needed when not doing cv - when we do stepAIC
smp_size <- floor(0.75 * nrow(paths))
set.seed(666)
train_idx <- sample(seq_len(nrow(paths)), size = smp_size)
train_data <- paths[train_idx, ]
test_data <- paths[-train_idx, ]


#For doing stepAIC
multinom.fit <- multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                         + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                           yStd +xStd +zStd + zMin + pathDist, data = paths)


stepAIC(multinom.fit, direction = "both")

#summary(multinom.fit)

#####################################

###### Multinomial logistic regression ##########

#https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/ 

# Sorting 
#train$Experiment <- map(cv$train, ~relevel(train$Experiment, ref = 1))
#test$Experiment <- map(cv$test, ~relevel(test$Experiment, ref = 1))

###### Cross-Validation ##########
# Model with Spatial Coordinates XYZ
multinom.fit.cv <- map(cv$train, ~multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                                           + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                                             yStd +xStd +zStd + zMin + pathDist + origoDist, data = .))

# Model with Spatial Coordinates XY
multinom.fit.cv.xy <- map(cv$train, ~multinom(Experiment ~ Person + Repetition
                                           + xyMax + xyMin +yShakinessMean +yShakinessStd +yRange +
                                             yStd +xStd + xyPathDist + xyOrigoDist, data = .))

# Model with Spatial Coordinates XZ
multinom.fit.cv.xz <- map(cv$train, ~multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                                            + xStd +zStd + zMin + xzPathDist + xzOrigoDist, data = .))


# Model with Spatial Coordinates YZ
multinom.fit.cv.yz <- map(cv$train, ~multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex
                                          + yShakinessMean + yShakinessStd + yzMax +yRange +
                                             yStd +zStd + zMin + yzPathDist + yzOrigoDist, data = .))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

pred <- map2_df(multinom.fit.cv, cv$test, get_pred, .id = "Fold")
pred$XYZModelPred <- map2_df(multinom.fit.cv, cv$test, get_pred, .id = "Fold")$pred
pred$XYModelPred <- map2_df(multinom.fit.cv.xy, cv$test, get_pred, .id = "Fold")$pred
pred$XZModelPred <- map2_df(multinom.fit.cv.xz, cv$test, get_pred,.id = "Fold")$pred
pred$YZModelPred <- map2_df(multinom.fit.cv.yz, cv$test, get_pred,.id = "Fold")$pred

# Baseline
Baseline <- rep(NA, length(pred$XYZModelPred))

# Maybe Baseline should be Majority Voting instead of random
for (idx in 1:folds) {
  data <- pred[which(pred$Fold == idx),]$Experiment
  guesses <- sample(data, length(which(pred$Fold == idx)))
  Baseline[which(pred$Fold == idx)] <- guesses
}
pred$Baseline <- Baseline

# Construct Accuracy table
Acc  <- pred %>% group_by(Fold) %>%
  summarise(Acc_XYZ_Model = round((sum(diag(table(Experiment, XYZModelPred)))/sum(table(Experiment, XYZModelPred)))*100,2),
            Acc_XY_Model = round((sum(diag(table(Experiment, XYModelPred)))/sum(table(Experiment, XYModelPred)))*100,2),
            Acc_XZ_Model = round((sum(diag(table(Experiment, XZModelPred)))/sum(table(Experiment, XZModelPred)))*100,2),
            Acc_YZ_Model = round((sum(diag(table(Experiment, YZModelPred)))/sum(table(Experiment, YZModelPred)))*100,2),
            Baseline = round((sum(diag(table(Experiment, Baseline)))/sum(table(Experiment, Baseline)))*100,2))
Acc


# Confusionmatrix for Model
xyz_truth_predicted <- data.frame(
  obs =  as.factor(pred$Experiment),
  pred =  as.factor(pred$XYZModelPred))

xy_truth_predicted <- data.frame(
  obs =  as.factor(pred$Experiment),
  pred =  as.factor(pred$XYModelPred))

xz_truth_predicted <- data.frame(
  obs =  as.factor(pred$Experiment),
  pred =  as.factor(pred$XZModelPred))

yz_truth_predicted <- data.frame(
  obs =  as.factor(pred$Experiment),
  pred =  as.factor(pred$YZModelPred))

cm_xyz <- conf_mat(xyz_truth_predicted, obs, pred)
cm_xy <- conf_mat(xy_truth_predicted, obs, pred)
cm_xz <- conf_mat(xz_truth_predicted, obs, pred)
cm_yz <- conf_mat(yz_truth_predicted, obs, pred)

autoplot(cm_xyz, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

autoplot(cm_xy, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

autoplot(cm_xz, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

autoplot(cm_yz, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

# Ved flere modeller kan vi sammenligne med en t-test eller McNemar
# Her antager vi at Generalisation error for modellerne er normafordelte.
# Vi kan tjekke dette ved Histogram og qqplot, som gjort ovenfor.

############ McNemar ###############  
#Comparing XYZ and YZ
actual_class <- pred$Experiment
correct_xyz <-  pred$XYZModelPred == actual_class
correct_xy <-   pred$XYModelPred == actual_class
correct_xz <-   pred$XZModelPred == actual_class
correct_yz <-   pred$YZModelPred == actual_class
baseline <-     pred$Baseline == actual_class

#two-ways of doing McNemar:
# Against Baseline
baseline_XYZ_pvalue <- mcnemar.test(table(baseline,correct_xyz))$p.value
baseline_XY_pvalue <- mcnemar.test(table(baseline,correct_xy))$p.value
baseline_XZ_pvalue <- mcnemar.test(table(baseline,correct_xz))$p.value
baseline_YZ_pvalue <- mcnemar.test(table(baseline,correct_yz))$p.value


# Against each other
XYZ_XY_pvalue <- mcnemar.test(table(correct_xyz,correct_xy))$p.value
XYZ_XZ_pvalue <- mcnemar.test(table(correct_xyz,correct_xz))$p.value
XYZ_YZ_pvalue <- mcnemar.test(table(correct_xyz,correct_yz))$p.value

XY_XZ_pvalue <- mcnemar.test(table(correct_xy,correct_xz))$p.value
XY_YZ_pvalue <- mcnemar.test(table(correct_xz,correct_yz))$p.value

YZ_XZ_pvalue <- mcnemar.test(table(correct_yz,correct_xz))$p.value

p_vals <- data.frame(
  XYZ_XY_pvalue = XYZ_XY_pvalue,
  XYZ_XZ_pvalue = XYZ_XZ_pvalue,
  XYZ_YZ_pvalue = XYZ_YZ_pvalue,
  XY_XZ_pvalue = XY_XZ_pvalue,
  XY_YZ_pvalue = XY_YZ_pvalue,
  YZ_XZ_pvalue = YZ_XZ_pvalue
);p_vals

baseline_pvals <- data.frame(
  baseline_XYZ_pvalue = baseline_XYZ_pvalue,
  baseline_XY_pvalue = baseline_XY_pvalue,
  baseline_XZ_pvalue = baseline_XZ_pvalue,
  baseline_YZ_pvalue = baseline_YZ_pvalue
);baseline_pvals

library(xtable)
xtable(as.data.frame(p.adjust(p_vals[1,], method = "BH")))
xtable(as.data.frame(p.adjust(baseline_pvals[1,], method = "BH")))

Acc %>% ungroup()
Acc <- as.data.frame(Acc);Acc$Fold <- as.numeric(Acc$Fold)
Acc <- Acc[order(Acc$Fold),]
xtable(Acc)

boxplot(Acc[,2:ncol(Acc)],  xlab = "Accuracies Across Folds")
?boxplot



############ McNemar NOTES ###############  
#We need a contingency table. Assumption: each cell has at least 25 observatinos
# statistic = (Yes/No - No/Yes)^2 / (Yes/No + No/Yes)

#It is not commenting on whether one model is more or less accurate or error prone than another. 
#This is clear when we look at how the statistic is calculated.

#It may be useful to report the difference in error between the two classifiers on the test set. 
#In this case, be careful with your claims as the significant test does not report on the difference in error between the models, 
#only the relative difference in the proportion of error between the models.

############# Initial test combined vs. separate ############# 
########## Combined ################
multinom.fit <- multinom(formula = Experiment ~ pathDist + xzVertex + pathHeight + 
                           zVertex + zMin + yStd + Person, data = train_data)



# Predicting the values for train dataset
test_data$precticed <- predict(multinom.fit, newdata = test_data, "class")

# Building classification table
ctable_test <- table(test_data$Experiment, test_data$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error



############ Predicting distance and height separately ############

#Obstacle height
multinom.fit <- multinom(formula = obstacleHeight ~ pathDist + xzVertex + pathHeight + 
                           zVertex + zMin + yStd + Person, data = train_data)

# Doing all the things for height

test_data$precticed <- predict(multinom.fit, newdata = test_data, "class")
ctable_test <- table(test_data$obstacleHeight, test_data$precticed)
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error

#Finding indices of correctly classified
correct_obstacleHeight <- test_data$obstacleHeight == test_data$precticed


#Object distance
multinom.fit <- multinom(formula = d ~ pathDist + xzVertex + pathHeight + 
                           zVertex + zMin + yStd + Person, data = train_data)

# Doing all the things for distance
test_data$precticed <- predict(multinom.fit, newdata = test_data, "class")
ctable_test <- table(test_data$d, test_data$precticed)
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error

correct_d<- test_data$d == test_data$precticed

#See where both are correct
both_correct <- (correct_d == correct_obstacleHeight & correct_d == TRUE)
mean(both_correct)
