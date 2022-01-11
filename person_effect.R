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

# Plot histogram with bell curve 
histWithNorm <- function(x, breaks = 40, main = "Histogram"){
  h <- hist(x, breaks = breaks, main = main)
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(x) 
  lines(xfit, yfit, col = "black", lwd = 2)
} 


# =======================================================
# Load/prepare data
# =======================================================

# Load cleaned data 
armdata <- readRDS("armdata_cleaned.rds")

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


# ---------------------
# Normal assumption
# ---------------------

pathDist <- paths$pathDist
logPathDist <- log(pathDist)

par(mfrow=c(1,2))

# normally distributed
h <- hist(pathDist, breaks = 30, main="Histogram of pathDist")
xfit <- seq(min(pathDist), max(pathDist), length = 40) 
yfit <- dnorm(xfit, mean = mean(pathDist), sd = sd(pathDist)) 
yfit <- yfit * diff(h$mids[1:2]) * length(pathDist) 
lines(xfit, yfit, col = "black", lwd = 2)


# normally distributed
h <- hist(logPathDist, breaks = 30, main="Histogram of log(pathDist)")
xfit <- seq(min(logPathDist), max(logPathDist), length = 40) 
yfit <- dnorm(xfit, mean = mean(logPathDist), sd = sd(logPathDist)) 
yfit <- yfit * diff(h$mids[1:2]) * length(logPathDist) 
lines(xfit, yfit, col = "black", lwd = 2)


par(mfrow=c(1,2))

qqnorm(pathDist, main='QQ-plot of pathDist')
qqline(pathDist)

qqnorm(logPathDist, main='QQ-plot of log(pathDist)')
qqline(logPathDist)



# Two way ANOVA model 
L <- aov(pathDist ~ Experiment * Person, data = paths)
summary(L)



# Get residuals from the model
residuals <- L$residuals

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("hist_residuals_pathDist_Experiment_Person"))
histWithNorm(residuals, breaks = 60)
dev.off()

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
qqnorm(L$residuals)
qqline(residuals)
dev.off()

# LOG-Transformed

# Two way ANOVA model 
L_log <- aov(logPathDist ~ Experiment * Person, data = paths)
summary(L_log)

# Get residuals from the model
residuals_log <- L_log$residuals

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("hist_residuals_log_pathDist_Experiment_Person"))
histWithNorm(residuals_log, breaks = 60)
dev.off()

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("qqplot_residuals_log_pathDist_Experiment_Person"))
qqnorm(residuals_log)
qqline(residuals_log)
dev.off()




pathHeight <- paths$pathHeight
logPathHeight <- log(pathHeight)

par(mfrow=c(1,2))

# normally distributed
h <- hist(pathHeight, breaks = 30, main="Histogram of pathDist")
xfit <- seq(min(pathHeight), max(pathHeight), length = 40) 
yfit <- dnorm(xfit, mean = mean(pathHeight), sd = sd(pathHeight)) 
yfit <- yfit * diff(h$mids[1:2]) * length(pathHeight) 
lines(xfit, yfit, col = "black", lwd = 2)


# normally distributed
h <- hist(logPathHeight, breaks = 30, main="Histogram of log(pathDist)")
xfit <- seq(min(logPathHeight), max(logPathHeight), length = 40) 
yfit <- dnorm(xfit, mean = mean(logPathHeight), sd = sd(logPathHeight)) 
yfit <- yfit * diff(h$mids[1:2]) * length(logPathHeight) 
lines(xfit, yfit, col = "black", lwd = 2)


par(mfrow=c(1,2))

qqnorm(pathDist, main='QQ-plot of pathDist')
qqline(pathDist)

qqnorm(logPathDist, main='QQ-plot of log(pathDist)')
qqline(logPathDist)



# Two way ANOVA model 
L <- aov(pathHeight ~ Experiment * Person, data = paths)
summary(L)

L <- aov(pathHeight ~ Experiment:Person, data = paths)
summary(L)


# Get residuals from the model
residuals <- L$residuals

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("hist_residuals_pathDist_Experiment_Person"))
histWithNorm(residuals, breaks = 60)
dev.off()

# Create histogram of residuals
par(mfrow=c(1,1))
jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
qqnorm(L$residuals)
qqline(residuals)
dev.off()

# 
# library(car)
# leveneTest(pathDist ~ Experiment * Person, data = paths)
# 
# 
# 
# 
# kruskal.test(paths$yRange, paths$Person)
# 
# inter <- interaction(paths$Experimen, paths$Person)
# 
# kruskal.test(pathDist ~ inter, data = paths)
# 
# pairwise.wilcox.test(paths$pathDist, paths$Person,
#                      p.adjust.method = "BH")
# 
# 
# shapiro.test(logPathDist)
# 
# 
# par(mfrow=c(2,2))
# L <- lm(pathDist ~ Person, data = paths)
# plot(L)
# anova(L)
# summary(L)
# 
# 
# par(mfrow=c(2,2))
# L <- lm(pathDist ~ Person + Experiment, data = paths)
# plot(L)
# anova(L)
# summary(L)
# 
# 
# 
# par(mfrow=c(2,2))
# L <- aov(pathDist ~ Person + Experiment, data = paths)
# plot(L)
# summary(L)
# 
# 
# 
# shapiro.test(logPathDist)
# 
# 
# 
# 
# paths$pathDist
# log(paths$pathDist)
# 
# anv <- aov(log(pathDist) ~ Person + Experiment, data = paths)
# plot(anv)
# summary(anv)