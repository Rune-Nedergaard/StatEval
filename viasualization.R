library(rgl)
library("ggplot2")
library("ggpubr")

# Load data
load("armdata.RData")


# Define gg_plot colors 
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }



line_colors <- c("royalblue", "violetred4", "seagreen", "sienna2", "slateblue", "violet", "indianred2", "lightblue4", "darkgoldenrod")

# Code taken from project description
start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl3 <- cylinder3d(cbind(30, 0, seq(0, 30, length = 10)), radius = c(3,3,3), sides = 10, closed = -2)
shade3d(addNormals(subdivision3d(start_cyl)), col = 'seagreen4')
shade3d(addNormals(subdivision3d(target_cyl)), col = 'seagreen4')
shade3d(addNormals(subdivision3d(cyl1)), col = 'seagreen3')
shade3d(addNormals(subdivision3d(cyl2)), col = 'seagreen3', alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = 'brown1')
surface3d(c(-17, 77), c(-30, 30), matrix(0, 2, 2), col = c("white", "white"), alpha = 0.9, specular = "black", )

# number of experiments (first layer )
n_experiments <- length(armdata);n_experiments

# number of persons (second layer)
n_participants <- length(armdata[[1]]
);n_participants

# number of repetitions (third layer)
n_repetitions <- length(armdata[[1]][[1]]);n_repetitions


i <- 5

# # Loop through experiments
# for (i in 1:n_experiments){
  # Loop through participants
  for (j in 1:length(armdata[[i]])){
    # Loop through repetitions
    for (k in 1:length(armdata[[i]][[j]])){
      # Define repetition
      repetition <- armdata[[i]][[j]][[k]]
      lines3d(repetition, col=line_colors[j])
    }
  }
# }

legend3d("topright", legend = paste('Paticipant', 1:10), pch = 16, col = line_colors, cex=2, inset=c(0.02))

# 
# i <- 5
# j <- 2
# k <- 7 
# 
# 
# repetition <- armdata[[i]][[j]][[k]]
# lines3d(repetition, col=line_colors[k])


legend3d("topright", legend = paste('Repetition', 1:10), pch = 16, col = line_colors, cex=2, inset=c(0.02))

