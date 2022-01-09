# Load cleaned data 
armdata <- readRDS("armdata_cleaned.rds")


# Create jpg file path for plots by providing a plot name
getJpgFilePath <- function(plot_name) {
  plots_path <- "plots"
  file_type <- "jpg"
  path <- paste(paste(plots_path, plot_name, sep="/"), file_type, sep=".")
  
}

# =======================================================
# 
# =======================================================

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
      
      paths <- rbind(paths, c(i, d, obstacle_height, j, k, path_height, z_vertex, x_vertex, y_range, y_std, z_min))

    }
  }
}


# Crate dataframe from matrix
paths <- data.frame(paths)

# Add column names
colnames(paths) <- c("Experiment", "d" , "obstacleHeight", "Person", "Repetition", "pathHeight", "zVertex", "xVertex", "yRange", "yStd", "zMin")
head(paths)

# Define variables as factors
s$Experiment <- as.factor(s$Experiment)
s$Person <- as.factor(s$Person)
s$Repetition <- as.factor(s$Repetition)

# We consider obstacleHeight and d as nominal data
s$obstacleHeight <- as.factor(s$obstacleHeight)
s$d <- as.factor(s$d)


jpeg(file= getJpgFilePath("boxplot_pathHeight_obstacleHeight"))
boxplot(paths$pathHeight ~ paths$obstacleHeight, ylab="path height", xlab="Obstacle height")
dev.off()

jpeg(file= getJpgFilePath("boxplot_xVertex_d"))
boxplot(paths$xVertex ~ paths$d, ylab="x vertex", xlab="d")
dev.off()


L <- lm(obstacleHeight ~ pathHeight, data = paths)
anova(L)
L <- lm(obstacleHeight ~ xVertex, data = paths)
anova(L)

# Check for interaction
L <- lm(obstacleHeight ~ pathHeight * xVertex, data = paths)
anova(L)