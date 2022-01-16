source("functions.R")

# ======================================================
# Load data
# ======================================================
load("armdata.RData")

# ======================================================
# Explore data
# ======================================================

# ----- Explore dimensions of data

# number of experiments (first layer )
n_experiments <- length(armdata);n_experiments

# number of persons (second layer)
n_participants <- length(armdata[[1]]
);n_participants

# number of repetitions (third layer)
n_repetitions <- length(armdata[[1]][[1]]);n_repetitions

# dimensions of data matrix 
dim_matrix <- dim(armdata[[1]][[1]][[1]]); dim_matrix

# Calculate total number of repetitions 
print(sprintf("Total number of repitions: %d", n_repetitions*n_participants*n_experiments))


# ----- Loop through data to find NaN values

total_nan_rows <- rep(NA, n_experiments*n_participants*n_repetitions)

nan_locations <- c()

idx <- 1

# Loop through experiments
for (i in 1:n_experiments){
  # Loop through participants
  for (j in 1:length(armdata[[i]])){
    # Loop through repetitions 
    for (k in 1:length(armdata[[i]][[j]])){
      # print(sprintf("Experiment %d, participant %d, repetition %d",i, j, k))
      # print(dim(armdata[[i]][[j]][[k]]))
      
      # count rows with nan values 
      nan_rows <- sum(rowSums(is.na(armdata[[i]][[j]][[k]])) > 0)
      nan_row_numbers <- which(rowSums(is.na(armdata[[i]][[j]][[k]])) > 0)
      
      
      if(nan_rows > 0){
        print(sprintf("Experiment %d, participant %d, repetition %d ([[%d]][[%d]][[%d]]):",i, j, k, i, j, k))
        print(sprintf("%d rows with NaN:", nan_rows))
        print(nan_row_numbers)
        
        nan_locations <- c(nan_locations, list(c(i,j,k)))
        
      }
      
      total_nan_rows[idx] <- nan_rows
      
      
      idx <- idx + 1  
      
    }
  }
}

# Total number of rows with NaN values 
print(sprintf("Total number of rows with NaN: %d",sum(total_nan_rows)))


# ----- Replace NaN values
nan_locations

# What to replace NaN values with
replace_with <- 0

nan_locations


# Loop through matrices with NaN values
for(i in 1:length(nan_locations)){
  # Get matrix of coordinates 
  arm_matrix <- armdata[[nan_locations[[i]][[1]]]][[nan_locations[[i]][[2]]]][[nan_locations[[i]][[3]]]]
  # print(arm_matrix)
  # Loop through points in matrix
  x_repl <- NA
  y_repl <- NA
  z_repl <- NA
  
  for(j in nrow(arm_matrix):1){
    
    if(is.na(sum(arm_matrix[j,])) ){
      arm_matrix[j,1] <- x_repl
      arm_matrix[j,2] <- y_repl
      arm_matrix[j,3] <- z_repl
      
    }
    
    
    x_repl <- arm_matrix[j,1]
    y_repl <- arm_matrix[j,2]
    z_repl <- arm_matrix[j,3]
    
  } 

  # print(arm_matrix)
  # Replace with new matrix 
  armdata[[nan_locations[[i]][[1]]]][[nan_locations[[i]][[2]]]][[nan_locations[[i]][[3]]]] <- arm_matrix
  
}



# Save new data to file 
saveRDS(armdata, file="armdata_cleaned.rds")

# Load data from file
# armdata_new <- readRDS("armdata_cleaned.rds")


# ======================================================
# Create new datatable
# ======================================================


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
      xy_curve_dist <- 0
      yz_curve_dist <- 0
      xz_curve_dist <- 0
      prev_point <- NA
      # Loop through all points and sum distance between points
      for (n in 1:nrow(repetition)){
        # print(n)
        # print(xy_prev_point)
        
        if (!is.na(sum(prev_point))){
          curve_dist <- curve_dist + getDist3d(repetition[n,], prev_point)    
          xy_curve_dist <- xy_curve_dist + getDist2d(repetition[n,c(1,2)], xy_prev_point)
          yz_curve_dist <- yz_curve_dist + getDist2d(repetition[n,c(2,3)], yz_prev_point)
          xz_curve_dist <- xz_curve_dist + getDist2d(repetition[n,c(1,3)], xz_prev_point)
          # print(xy_curve_dist)
        }
        
        prev_point <- repetition[n,]
        xy_prev_point <- repetition[n,c(1,2)]
        yz_prev_point <- repetition[n,c(2,3)]
        xz_prev_point <- repetition[n,c(1,3)]
        
      }
      
      paths <- rbind(paths, c(i, d, obstacle_height, j, k, path_height, z_vertex, xz_vertex, xy_max, xy_min, y_shakiness_mean, y_shakiness_std, yz_max, y_range, y_std, x_std, z_std, z_min, curve_dist, xy_curve_dist, yz_curve_dist, xz_curve_dist))
      
    }
  }
}

# Crate dataframe from matrix
paths <- data.frame(paths)

# Add column names
colnames(paths) <- c("Experiment", "d" , "obstacleHeight", "Person", "Repetition", "pathHeight", "zVertex", "xzVertex","xyMax","xyMin","yShakinessMean","yShakinessStd","yzMax", "yRange", "yStd","xStd","zStd", "zMin", "pathDist", "xyPathDist", "yzPathDist", "xzPathDist")
head(paths)

# Define variables as factors
paths$Experiment <- as.factor(paths$Experiment)
paths$Person <- as.factor(paths$Person)
paths$Repetition <- as.factor(paths$Repetition)
paths$d <- as.factor(paths$d)
paths$obstacleHeight <- as.factor(paths$obstacleHeight)

# Remove outlier. Repetition with dropped cylinder 
idx_remove <- with(paths, which(Experiment == 5 & Person == 2 & Repetition == 7))
paths <- paths[-idx_remove,]

# Save dataframe to file 
saveRDS(paths, file="paths.rds")
