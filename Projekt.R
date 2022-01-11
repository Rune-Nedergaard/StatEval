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
      
      # Define coordinates 
      x <- repetition[,1]
      y <- repetition[,2]
      z <- repetition[,3]
      
      # Get x-value of highest point on path (vertex of path)
      x_vertex <- repetition[which.max(z), 1]
      # Get z-value of highest point on path (vertex of path)
      z_vertex <- repetition[which.max(z), 3]
      # Calculate y-value range
      y_range <- abs(max(y) - min(y))
      # y standard deviation
      y_std <- sd(y)
      
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
      
      
      paths <- rbind(paths, c(i, d, obstacle_height, j, k, path_height, z_vertex, x_vertex, y_range, y_std, z_min, curve_dist))

    }
  }
}

# Crate dataframe from matrix
paths <- data.frame(paths)

# Add column names
colnames(paths) <- c("Experiment", "d" , "obstacleHeight", "Person", "Repetition", "pathHeight", "zVertex", "xVertex", "yRange", "yStd", "zMin", "pathDist")
head(paths)

# Define variables as factors
paths$Experiment <- as.factor(paths$Experiment)
paths$Person <- as.factor(paths$Person)
paths$Repetition <- as.factor(paths$Repetition)



# =======================================================
# Explore data
# =======================================================

# ---------------------
# Boxplot
# ---------------------

# Boxplot: pathsHeigh ~ ObstacleHeight
jpeg(file= getJpgFilePath("boxplot_pathHeight_obstacleHeight"))
boxplot(paths$pathHeight ~ paths$obstacleHeight, ylab="Path height", xlab="Obstacle height")
dev.off()

# Boxplot: xVertex ~ d
jpeg(file= getJpgFilePath("boxplot_xVertex_d"))
boxplot(paths$xVertex ~ paths$d, ylab="x vertex", xlab="d")
dev.off()

# Boxplot: pathHeight ~ Person
jpeg(file= getJpgFilePath("boxplot_pathHeight_person"))
boxplot(paths$pathHeight ~ paths$Person, ylab="Path distance", xlab="Person")
dev.off()

# Boxplot: pathDist ~ Person
jpeg(file= getJpgFilePath("boxplot_pathDist_person"))
boxplot(paths$pathDist ~ paths$Person, ylab="Path distance", xlab="Person")
dev.off()


# ---------------------
# Effect of person
# ---------------------
jpeg(file= getJpgFilePath("interaction_experiment_person_pathDist"), width = 850, height = 850)
interaction.plot(paths$Experiment, paths$Person, paths$pathDist, xlab="Experiment", ylab="Path distance")
dev.off()

anv <- aov(log(pathDist) ~ Person + Experiment, data = paths)
plot(anv)
summary(anv)



# =======================================================
# Model
# =======================================================


anv <- aov(log(pathDist) ~ Person + Experiment + Person:Experiment, data = paths)
plot(anv)
summary(anv)

res.aov <- aov(pathHeight ~ Person + Experiment, data = paths)
plot(anv)
summary(res.aov)



anv <- lm(log(pathDist) ~ Person + Experiment + Person:Experiment, data = paths)
plot(anv)
summary(anv)


par(mfrow=c(1,2))

hist(log(paths$pathDist))
hist(paths$pathDist)



L <- lm(pathDist ~ Person * Experiment, data = paths)
anova(L)




L <- lm(pathHeight ~ . , data = paths)
anova(L)


hist(paths$pathDist)


interaction.plot(paths$Experiment, paths$Person, paths$pathHeight)

anv <- aov(pathDist ~ Person + Experiment, data = paths)
plot(anv)

qqplot(anv)


# L <- lm(paths$pathHeight ~ paths$Person , data = paths)
anova(L)






L <- lm(paths$xVertex ~ paths$Person , data = paths)
anova(L)


plot(L)

summary(L)

pairwise.wilcox.test(paths$pathDist, paths$Person,
                     p.adjust.method = "BH")



# 
# 
# s <- paths[paths$Experiment == 2,]
# 
# 
# 
# # path height
# boxplot(s$pathHeight ~ s$Person, ylab="path height", xlab="Person")
# 
# kruskal.test(s$pathHeight, s$Person)
# 
# pairwise.wilcox.test(s$pathHeight, s$Person,
#                      p.adjust.method = "BH")
# 
# 
# 
# 
# # path height
# boxplot(paths$pathHeight ~ paths$Person, ylab="path height", xlab="Person")
# 
# kruskal.test(paths$pathHeight, paths$Person)
# 
# pairwise.wilcox.test(paths$pathHeight, paths$Person,
#                      p.adjust.method = "BH")
# 
# 
# # x-coordinate, toppunkt
# boxplot(paths$xVertex ~ paths$Person, ylab="xVertex", xlab="Person")
# 
# kruskal.test(paths$xVertex, paths$Person)
# 
# pairwise.wilcox.test(paths$xVertex, paths$Person,
#                      p.adjust.method = "BH")
# 
# 
# # yRange
# boxplot(paths$yRange ~ paths$Person, ylab="yRange", xlab="Person")
# 
# kruskal.test(paths$yRange, paths$Person)
# 
# pairwise.wilcox.test(paths$yRange, paths$Person,
#                      p.adjust.method = "BH")
# 
# 
# 
# 
# 
# pairwise
# 
# ?kruskal.test
# 
# L <- lm(obstacleHeight ~ pathHeight, data = paths)
# anova(L)
# 
# 
# predict.lm()
# 
# L <- lm(obstacleHeight ~ xVertex, data = paths)
# anova(L)
# 
# # Check for interaction
# L <- lm(obstacleHeight ~ pathHeight * xVertex, data = paths)
# anova(L)

###########################################################
#                       MODELLING                         #
###########################################################
require(nnet)
library(modelr)
library(purrr)
library(dplyr)
library(ggplot2)
library(yardstick)

folds <- 30

cv <- crossv_kfold(paths, k = folds)

#smp_size <- floor(0.75 * nrow(paths))
#set.seed(666)


#train_idx <- sample(seq_len(nrow(paths)), size = smp_size)

#train_data <- paths[train_idx, ]
#test_data <- paths[-train_idx, ]

#####################################

###### Multinomial logistic regression ##########

#https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/ 

# Sorting 
train$Experiment <- map(cv$train, ~relevel(train$Experiment, ref = 1))
test$Experiment <- map(cv$test, ~relevel(test$Experiment, ref = 1))

# TRAIN MODEL
#multinom.fit <- multinom(Experiment ~ pathDist+ xVertex+ pathHeight+ zVertex+ 
#                           zMin+ yRange+ yStd + Repetition + Person, data=train)

# Simpler model according to stepAIC
#multinom.fit <- multinom(Experiment ~ pathDist+ xVertex+ pathHeight+ zVertex+ 
#                        zMin+ yRange+ yStd + Repetition + Person, data = train)

###### Cross-Validation ##########

# Vi kan nemt lave flere modeller ved blot: 
#multinom.fit.cv2 <- map(cv$train, ~multinom(Experiment ~ DATA, data = .))

multinom.fit.cv <- map(cv$train, ~multinom(Experiment ~ pathDist+ xVertex+ pathHeight+ zVertex+ 
                                             zMin+ yRange+ yStd + Repetition + Person, data = .))

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model)
  return(pred)
}

pred  <- map2_df(multinom.fit.cv, cv$test, get_pred, .id = "Fold")
#pred2 <- map2_df(multinom.fit.cv2, cv$test, get_pred, .id = "Fold")

Acc  <- pred %>% group_by(Fold) %>%
  summarise(Acc = round((sum(diag(table(Experiment, pred)))/sum(table(Experiment, pred)))*100,2))
  # Acc2 = round((sum(diag(table(Experiment, pred2)))/sum(table(Experiment, pred2)))*100,2)

# CM <- confusionMatrix(pred$pred, pred$Experiment, dnn = c("Prediction", "Reference"))

truth_predicted <- data.frame(
  obs = pred$Experiment,
  pred = pred$pred)

truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)

cm <- conf_mat(truth_predicted, obs, pred)

autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "orange")

quantile(pull(Acc, Acc), probs = c(0.025, 0.975))

# Plot histogram with bell curve 
histWithNorm <- function(x, breaks = 10, main = "Histogram"){
  h <- hist(x, breaks = breaks, main = main, col = 2)
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(x) 
  lines(xfit, yfit, col = "black", lwd = 2)
}

histWithNorm(pull(Acc, Acc))
qqnorm(pull(Acc, Acc))
qqline(pull(Acc, Acc))

# Ved flere modeller kan vi sammenligne med en t-test eller McNemar
# Her antager vi at Generalisation error for modellerne er normafordelte.
# Vi kan tjekke dette ved Histogram og qqplot, som gjort ovenfor.


##################

#stepAIC(multinom.fit, direction = "both")

#summary(multinom.fit)

############# Train-set ############# 

# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")

# Building classification table
ctable_train <- table(train$Experiment, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
train_test_error <- round((sum(diag(ctable_train))/sum(ctable_train))*100,2);train_test_error

############# Test-set ############# 

# Predicting the values for train dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable_test <- table(test$Experiment, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error

print(c(train_test_error, test_error))


############ Predicting distance and height separately ############

#Obstacle height
multinom.fit <- multinom(formula = obstacleHeight ~ pathDist + xVertex + pathHeight + 
                           zVertex + zMin + yStd + Person, data = train)

# Doing all the things for height
train$precticed <- predict(multinom.fit, newdata = train, "class")
ctable_train <- table(train$obstacleHeight, train$precticed)
train_test_error <- round((sum(diag(ctable_train))/sum(ctable_train))*100,2);train_test_error
test$precticed <- predict(multinom.fit, newdata = test, "class")
ctable_test <- table(test$obstacleHeight, test$precticed)
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error

#Finding indices of correctly classified
correct_obstacleHeight <- test$obstacleHeight == test$precticed


#Object distance
multinom.fit <- multinom(formula = d ~ pathDist + xVertex + pathHeight + 
                           zVertex + zMin + yStd + Person, data = train)

# Doing all the things for distance
train$precticed <- predict(multinom.fit, newdata = train, "class")
ctable_train <- table(train$d, train$precticed)
train_test_error <- round((sum(diag(ctable_train))/sum(ctable_train))*100,2);train_test_error
test$precticed <- predict(multinom.fit, newdata = test, "class")
ctable_test <- table(test$d, test$precticed)
test_error <- round((sum(diag(ctable_test))/sum(ctable_test))*100,2);test_error

correct_d<- test$d == test$precticed

#See where both are correct
both_correct <- (correct_d == correct_obstacleHeight & correct_d == TRUE)
mean(both_correct)
