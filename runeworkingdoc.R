#load data
load('armdata.RData')

data <- as.matrix(armdata)


s <- c() # Make a matrix of our path statistics
e <- 1
for (experiment in armdata) {
  p <- 1
  for (person in experiment) {
    r <- 1
    for (repetition in person) {
      repetition[is.na(repetition)] <- 0 # Remove NAs
      z.max <- max(repetition[,3])
      x.argmax <- which.max(repetition[,3])
      x.max <- repetition[x.argmax, 1]
      y.range <- abs(max(repetition[,2]) - min(repetition[,2]))
      y.std <- sd(repetition[,2])
      z.min <- min(repetition[,3])
      h <- z.max - z.min
      
      s <- rbind(s, c(e, p, r, h, z.max, x.max, y.range, y.std, z.min))
      r <- r + 1
    }
    p <- p + 1
  }
  e <- e + 1
}


# Make a nice data frame
s <- data.frame(s)
colnames(s) <- c("Exp", "Per", "Rep", "height", "z_max", "x", "y_range", "y_std", "z_min")
s$Exp <- as.factor(s$Exp)
s$Per <- as.factor(s$Per)
s$Rep <- as.factor(s$Rep)

boxplot(s$height ~ s$Exp)

hist(s$x, breaks=40)


l <- lm(s$height ~ s$Exp)
anova(l)

par(mfrow=c(2, 2))
boxplot(s$height ~ s$Exp)
boxplot(s$height ~ s$Per)
boxplot(s$x ~ s$Exp)
boxplot(s$x ~ s$Per)

par(mfrow=c(1,1))
interaction.plot(s$Exp, s$Per, s$height)

# P-adjust using method
p.adjust(c(2.2e-16,2.2e-16), method = 'hochberg')

