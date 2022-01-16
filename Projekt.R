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
multinom.fit.cv <- map(cv$train, ~multinom(Experiment ~ Person + Repetition +pathHeight+ zVertex +xzVertex
                                           + xyMax + xyMin +yShakinessMean +yShakinessStd +yzMax +yRange +
                                             yStd +xStd +zStd + zMin + pathDist, data = .))


get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

pred <- map2_df(multinom.fit.cv, cv$test, get_pred, .id = "Fold");pred

# Baseline
Baseline <- rep(NA, length(pred$pred))

for (idx in 1:folds) {
  data <- pred[which(pred$Fold == idx),]$Experiment
  guesses <- sample(data, length(which(pred$Fold == idx)))
  Baseline[which(pred$Fold == idx)] <- guesses
}
pred$Baseline <- Baseline
pred

Acc  <- pred %>% group_by(Fold) %>%
  summarise(Acc = round((sum(diag(table(Experiment, pred)))/sum(table(Experiment, pred)))*100,2),
            Baseline = round((sum(diag(table(Experiment, Baseline)))/sum(table(Experiment, Baseline)))*100,2))
Acc

# Confusionmatrix for Model
truth_predicted <- data.frame(
  obs = pred$Experiment,
  pred = pred$pred)

truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

# Confidence interval for model
CI <- quantile(pull(Acc, Acc), probs = c(0.025, 0.975));CI


histWithNorm(pull(Acc, Acc))
abline(v = c(CI[1],CI[2]), col = 1, lwd =2)
qqnorm(pull(Acc, Acc))
qqline(pull(Acc, Acc))

# Ved flere modeller kan vi sammenligne med en t-test eller McNemar
# Her antager vi at Generalisation error for modellerne er normafordelte.
# Vi kan tjekke dette ved Histogram og qqplot, som gjort ovenfor.

############ McNemar ###############  
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
