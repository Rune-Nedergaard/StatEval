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
  print(arm_matrix)
  # Loop through points in matrix
  for(j in nrow(arm_matrix):1){
    x <- arm_matrix[j,1]
    y <- arm_matrix[j,2]
    z <- arm_matrix[j,3]

    if(is.na(sum(arm_matrix[j,])) ){
      arm_matrix[j,] <- c(x_repl,y_repl,z_repl)
    }
    
    
    
    x_repl <- x
    y_repl <- y
    z_repl <- z
    
  }  

  # Replace with new matrix 
  armdata[[nan_locations[[i]][[1]]]][[nan_locations[[i]][[2]]]][[nan_locations[[i]][[3]]]] <- arm_matrix
  
}


# Save new data to file 
saveRDS(armdata, file="armdata_cleaned.rds")

# Load data from file
# armdata <- readRDS("armdata_cleaned.rds")
