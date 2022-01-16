
# =======================================================
#  Define functions
# =======================================================

# Create jpg file path for plots by providing a plot name
getJpgFilePath <- function(plot_name) {
  plots_path <- "plots"
  file_type <- "jpg"
  path <- paste(paste(plots_path, plot_name, sep="/"), file_type, sep=".")
  
}

# Get euclidean distance in 3d
getDist3d <- function(v1, v2) {
  dist <- sqrt((v2[1] - v1[1])^2 + (v2[2]- v1[2])^2 + (v2[3]- v1[3])^2)
  return(dist)
}

# Get euclidean distance in 2d
getDist2d <- function(v1, v2) {
  dist <- sqrt((v2[1] - v1[1])^2 + (v2[2]- v1[2])^2)
  return(dist)
}


# Plot histogram with bell curve 
histWithNorm <- function(x, breaks = 40, main = "Histogram with normal fit"){
  h <- hist(x, breaks = breaks, main = main)
  xfit <- seq(min(x), max(x), length = 40) 
  yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(x) 
  lines(xfit, yfit, col = "black", lwd = 2)
} 
