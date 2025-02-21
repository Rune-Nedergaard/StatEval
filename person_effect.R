# =======================================================
# Libraries
# =======================================================
# Import libraries 
library("ggplot2")
library("ggpubr")
library(car)
library(xtable)

source("functions.R")

# =======================================================
# Load data
# =======================================================
# Load data from file
paths <- readRDS("paths.rds")

# ============================
# Normal assumption
# ============================

# Normal assumption of the dependable variable

# pathDist
jpeg(file= getJpgFilePath("normality_of_pathDist.jpg"))
par(mfrow=c(1,2))
histWithNorm(paths$pathDist, breaks = 30)
qqnorm(paths$pathDist)
qqline(paths$pathDist)
dev.off()


# origoDist
jpeg(file= getJpgFilePath("normality_of_origoDist.jpg"))
par(mfrow=c(1,2))
histWithNorm(paths$origoDist, breaks = 30)
qqnorm(paths$origoDist)
qqline(paths$origoDist)
dev.off()


# It is seen that the data approximately follows a normal distribution

# ============================
# Homoscedasticity
# ============================


jpeg(file= getJpgFilePath("homoscedasticity_residuals.jpg"), width=1000)

L_inter <-aov(origoDist~Person + Experiment + Person:Experiment, data= paths)
residuals <- L_inter$residuals

par(mfrow=c(2,5))
plot.new()
text(0.5,0.5,"origoDist:",cex=1.8,font=2)
histWithNorm(residuals, breaks = 40)
plot(L_inter, which=1)
plot(L_inter, which=2)
plot(L_inter, which=3)


L_inter <-aov(pathDist~Person + Experiment + Person:Experiment, data= paths)
residuals <- L_inter$residuals

plot.new()
text(0.5,0.5,"pathDist:",cex=1.8,font=2)

# par(mfrow=c(2,3))
histWithNorm(residuals, breaks = 40)
plot(L_inter, which=1)
plot(L_inter, which=2)
plot(L_inter, which=3)
dev.off()


# ============================
# Models
# ============================

# Two Way ANOVA 

L_exp <- aov(origoDist~Experiment, data= paths)
SUMMARY <- summary(L_exp);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))

L_per <- aov(origoDist~Person, data= paths)
SUMMARY <- summary(L_per);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))

L_inter <- aov(pathDist~Person:Experiment, data= paths)
SUMMARY <- summary(L_inter);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))

L_exp_per <- aov(origoDist~Experiment+Person, data= paths)
SUMMARY <- summary(L_exp_per);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))

L_exp_inter <- aov(pathDist~Experiment + Person:Experiment, data= paths)
SUMMARY <- summary(L_exp_inter);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))

L_per_inter <- aov(origoDist~Person + Person:Experiment, data= paths)
SUMMARY <- summary(L_per_inter);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))



L_exp_per_inter <- aov(pathDist~Experiment + Person + Person:Experiment, data= paths)
SUMMARY <- summary(L_exp_per_inter);SUMMARY
print(xtable(SUMMARY, type = "latex", digits=c(2,2,2,2,2,-2)))


# ============================
# Models
# ============================

tukey <- TukeyHSD(L_exp_per_inter)

p_exp <- tukey$'Experiment'[,'p adj']
p_per <- tukey$'Person'[,'p adj']
p_exp_per <- tukey$'Person:Experiment'[,'p adj']
p_all <- c(p_exp, p_per, p_exp_per)

sum(p_all <= 0.05) / length(p_all)


# =======================================================
# Explore data
# =======================================================

# ---------------------
# Boxplot
# ---------------------

par(mfrow=c(1,1))

jpeg(file= getJpgFilePath("boxplot_pathDist_person"))
boxplot(paths$pathDist ~ paths$Person, ylab="Path height", xlab="Obstacle height")
dev.off()

jpeg(file= getJpgFilePath("boxplot_origoDist_person"))
boxplot(paths$origoDist ~ paths$Person, ylab="Path height", xlab="Obstacle height")
dev.off()


jpeg(file= getJpgFilePath("boxplot_pathDist_multiple"), width = 900, height = 900)
paths %>% 
  ggplot(aes(x=factor(Person),y=pathDist, fill=Experiment)) +
  geom_boxplot() +
  xlab("Person")+ 
  facet_wrap(~Experiment,ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18))
dev.off()



jpeg(file= getJpgFilePath("boxplot_origoDist_multiple"), width = 900, height = 900)
paths %>% 
  ggplot(aes(x=factor(Person),y=origoDist, fill=Experiment)) +
  geom_boxplot() +
  xlab("Person")+ 
  facet_wrap(~Experiment,ncol = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=18))
dev.off()


# ---------------------
# Boxplot
# ---------------------
# Interaction plot is made
jpeg(file= getJpgFilePath("interaction_experiment_person_pathDist"), width = 1080, height = 720)
# interaction.plot(paths$Experiment, paths$Person, paths$pathDist, xlab="Experiment", ylab="Path distance", col=1:10, lwd=2)
ggline(paths, x = "Experiment", y = "pathDist", color = "Person",
       add = c("mean_se"), size=1)
dev.off()

# Interaction plot is made
jpeg(file= getJpgFilePath("interaction_experiment_person_origoDist"), width = 1080, height = 720)
# interaction.plot(paths$Experiment, paths$Person, paths$pathDist, xlab="Experiment", ylab="Path distance", col=1:10, lwd=2)
ggline(paths, x = "Experiment", y = "origoDist", color = "Person",
       add = c("mean_se"), size=1)
dev.off()

 
# # Boxplot: pathsHeigh ~ ObstacleHeight
# jpeg(file= getJpgFilePath("boxplot_pathHeight_obstacleHeight"))
# boxplot(paths$pathHeight ~ paths$obstacleHeight, ylab="Path height", xlab="Obstacle height")
# dev.off()
# 
# # Boxplot: xVertex ~ d
# jpeg(file= getJpgFilePath("boxplot_xVertex_d"))
# boxplot(paths$xVertex ~ paths$d, ylab="x vertex", xlab="d")
# dev.off()
# 
# # Boxplot: pathHeight ~ Person
# jpeg(file= getJpgFilePath("boxplot_pathHeight_person"))
# boxplot(paths$pathHeight ~ paths$Person, ylab="Path distance", xlab="Person")
# dev.off()
# 
# # Boxplot: pathDist ~ Person
# jpeg(file= getJpgFilePath("boxplot_pathDist_person"))
# boxplot(paths$pathDist ~ paths$Person, ylab="Path distance", xlab="Person")
# dev.off()
# 
# par(mfrow=c(1,1))
# boxplot(paths$pathHeight ~ paths$Person:paths$Experiment)
# 
# 
# # ===============================================================================
# # Dependable variable: pathDist
# # ===============================================================================
# 
# # ---------------------
# # Effect of person
# # ---------------------
# # Interaction plot is made 
# jpeg(file= getJpgFilePath("interaction_experiment_person_pathDist"), width = 1080, height = 720)
# # interaction.plot(paths$Experiment, paths$Person, paths$pathDist, xlab="Experiment", ylab="Path distance", col=1:10, lwd=2)
# ggline(paths, x = "Experiment", y = "pathDist", color = "Person",
#        add = c("mean_se"), size=1)
# dev.off()
# 
# head(paths)
# 
# 
# ggboxplot(paths, x="Experiment", y="pathDist", color="Person")
# 
# jpeg(file= getJpgFilePath("boxplot_experiment_person_pathDist_multiple"), width = 900, height = 900)
# paths %>% 
#   ggplot(aes(x=factor(Person),y=pathDist, fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# dev.off()
# 
# 
# 
# jpeg(file= getJpgFilePath("log_boxplot_experiment_person_pathDist_multiple"), width = 900, height = 900)
# paths %>% 
#   ggplot(aes(x=factor(Person),y=log(pathDist), fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# dev.off()
# 
# 
# 
# # jpeg(file= getJpgFilePath("log_boxplot_experiment_person_pathDist_multiple"), width = 900, height = 900)
# paths %>% 
#   ggplot(aes(x=factor(Person),y=pathHeight, fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# # dev.off()
# 
# paths %>% 
#   ggplot(aes(x=factor(Person),y=log(pathDist), fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# 
# # bwplot(pathDist ~ Person | paste0("Experiment ", Experiment), data = paths)
# 
# 
# qqnorm(paths$pathHeight)
# qqline(paths$pathHeight)
# 
# # Two Way ANOVA 
# L <- aov(log(pathHeight)~Person + Experiment + Person:Experiment, data= paths)
# L <- aov(pathDist~Person + Experiment + Person:Experiment, data= paths)
# 
# par(mfrow=c(2,2))
# plot(L)
# 
# 
# 
# paths %>% 
#   ggplot(aes(x=factor(Person),y=pathDist, fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# 
# ggline(paths, x = "Experiment", y = "origoDist", color = "Person",
#        add = c("mean_se"), size=1)
# 
# # ---------------------
# # Assumptions
# # ---------------------
# 
# 
# # -----------------
# 
# 
# library(RVAideMemoire)
# 
# 
# perm.anova(pathDist~Experiment+Person+Experiment:Person,data=paths)
# 
# qqnorm(paths$pathDist)
# qqline(paths$pathDist)
# 
# shapiro.test(paths$pathDist)
# 
# 
# # Two Way ANOVA 
# L <- aov(pathDist~Person + Experiment + Person:Experiment, data= paths)
# 
# par(mfrow=c(2,2))
# plot(L)
# 
# residuals <- L$residuals
# 
# par(mfrow=c(1,2))
# histWithNorm(residuals, breaks = 60)
# qqnorm(residuals)
# qqline(residuals)
# 
# # anova(L)$residuals
# 
# # --- Normality assumption
# 
# # Create histogram of residuals
# jpeg(file= getJpgFilePath("hist_residuals_qq_plot_pathDist_Experiment_Person"),width=1080, height=480)
# par(mfrow=c(1,3))
# histWithNorm(residuals, breaks = 60)
# plot(L, which=1)
# plot(L, which=2)
# dev.off()
# 
# 
# # Create histogram of residuals
# par(mfrow=c(1,1))
# jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
# qqnorm(residuals)
# qqline(residuals)
# dev.off()
# 
# # Use shapiro test. 
# # H0: Distribution is normal 
# shapiro.test(x = residuals)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Residuals are not normally distributed
# 
# # --- Homoscedasticity
# 
# # Test for homogenity of variance by doing a leveneTest
# # H0: population variances are equal
# leveneTest(pathDist ~ Experiment*Person, data = paths)
# 
# 
# # -------------
# # LOG TRANSFORM
# 
# 
# # Two Way ANOVA 
# L <- aov(log(pathDist)~Person + Experiment + Person:Experiment, data= paths)
# 
# summary(L)
# 
# residuals <- L$residuals
# 
# 
# # --- Normality assumption
# 
# # Create histogram of residuals
# jpeg(file= getJpgFilePath("log_hist_residuals_qq_plot_pathDist_Experiment_Person"),width=1080, height=480)
# par(mfrow=c(1,3))
# histWithNorm(residuals, breaks = 60)
# plot(L, which=1)
# plot(L, which=2)
# dev.off()
# 
# # Use shapiro test. 
# # H0: Normal distribution is normal 
# shapiro.test(x = residuals)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Residuals are not normally distributed
# 
# 
# # --- Homoscedasticity
# 
# # Test for homogenity of variance by doing a leveneTest
# # H0: population variances are equal
# leveneTest(pathDist ~ Experiment*Person, data = paths)
# 
# 
# 
# # ===============================================================================
# # Dependable variable: pathHeight
# # ===============================================================================
# 
# # ---------------------
# # Effect of person
# # ---------------------
# # Interaction plot is made 
# jpeg(file= getJpgFilePath("interaction_experiment_person_pathHeight"), width = 1080, height = 720)
# # interaction.plot(paths$Experiment, paths$Person, paths$pathDist, xlab="Experiment", ylab="Path distance", col=1:10, lwd=2)
# ggline(paths, x = "Experiment", y = "pathHeight", color = "Person",
#        add = c("mean_se"), size=1)
# dev.off()
# 
# 
# ggboxplot(paths, x="Experiment", y="pathHeight", color="Person")
# 
# jpeg(file= getJpgFilePath("boxplot_experiment_person_pathHeight_multiple"), width = 900, height = 900)
# paths %>% 
#   ggplot(aes(x=factor(Person),y=pathHeight, fill=Experiment)) +
#   geom_boxplot() +
#   xlab("Person")+ 
#   facet_wrap(~Experiment,ncol = 4) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         legend.title = element_text(size=20,face="bold"),
#         legend.text = element_text(size=18))
# dev.off()
# 
# 
# # bwplot(pathDist ~ Person | paste0("Experiment ", Experiment), data = paths)
# 
# 
# pp <- paths[(paths$Person == 1:10),]
# 
# ggline(pp, x = "Experiment", y = "pathHeight", color = "Person",
#        add = c("mean_se"), size=1)
# 
# # ---------------------
# # Assumptions
# # ---------------------
# 
# # -----------------
# # Two Way ANOVA 
# L <- aov(pathHeight~Person + Experiment + Person:Experiment, data= paths)
# summary(L)
# residuals <- L$residuals
# 
# # anova(L)$residuals
# 
# # --- Normality assumption
# 
# # Create histogram of residuals
# jpeg(file= getJpgFilePath("hist_residuals_qq_plot_pathHeight_Experiment_Person"),width=1080, height=480)
# par(mfrow=c(1,3))
# histWithNorm(residuals, breaks = 60)
# plot(L, which=1)
# plot(L, which=2)
# dev.off()
# 
# 
# # Use shapiro test. 
# # H0: Distribution is normal 
# shapiro.test(x = residuals)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Residuals are not normally distributed
# 
# # --- Homoscedasticity
# 
# # Test for homogenity of variance by doing a leveneTest
# # H0: population variances are equal
# leveneTest(pathHeight ~ Experiment*Person)
# 
# 
# # -------------
# # LOG TRANSFORM
# 
# 
# # Two Way ANOVA 
# L <- aov(log(pathHeight)~Person + Experiment + Person:Experiment, data= paths)
# 
# summary(L)
# 
# residuals <- L$residuals
# 
# 
# # --- Normality assumption
# 
# # Create histogram of residuals
# jpeg(file= getJpgFilePath("log_hist_residuals_qq_plot_pathHeight_Experiment_Person"),width=1080, height=480)
# par(mfrow=c(1,3))
# histWithNorm(residuals, breaks = 60)
# plot(L, which=1)
# plot(L, which=2)
# dev.off()
# 
# # Use shapiro test. 
# # H0: Normal distribution is normal 
# shapiro.test(x = residuals)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Residuals are not normally distributed
# 
# 
# # --- Homoscedasticity
# 
# # Test for homogenity of variance by doing a leveneTest
# # H0: population variances are equal
# leveneTest(pathDist ~ Experiment*Person, data = paths)
# 
# 
# 
# 
# 
# 
# # ==========================================
# # Permutation tests
# # ==========================================
# 
# 
# 
# 
# 
# tfun <- function(x) mean(x[, 1:100]) - mean(x[, 101:200])
# 
# paths[1:100,]
# 
# 
# tvals <- rep(NA, 10000)
# 
# for (i in 1:10000) {
#   tvals[i] <- tfun(cables.as.matrix[, sample(9)])
# }
# hist(tvals)
# 
# mean(tvals < tobs) 
# ## pvalue is zero.
# 
# 
# 
# 
# # Standard Anova on these data
# mod1 <- lm(pathDist ~ Experiment + Person + Experiment:Person, data=paths)
# ANOVA <- summary(aov(mod1))
# ANOVA
# 
# cat( " The standard ANOVA for these data follows ","\n")
# F_Experiment <-  ANOVA[[1]]$"F value"[1]   # Saving F values for future use
# F_Person <-  ANOVA[[1]]$"F value"[2]
# F_interact <-  ANOVA[[1]]$"F value"[3]
# ANOVA
# cat( "\n")
# cat( "\n")
# 
# print( "Resampling as in Manly with unrestricted sampling of observations. ")
# # Now start resampling
# nreps <- 100
# F_Exp <- numeric(nreps)    #Set up space to store F values as calculated.
# F_Pers <- numeric(nreps)  
# F_PersExp <- numeric(nreps)
# F_EXP[1] <- F_Experiment          # The first F of our 5000 
# F_Pers[1] <- F_Person
# F_PersExp[1] <- F_interact
# for (i in 2:nreps) {
#   newPathDists <- sample(paths$pathDist, length(paths$pathDist))
#   mod2 <- lm(newPathDists ~ Experiment + Person + Experiment:Person, data = paths)
#   b <- summary(aov(mod2))
#   F_Exp[i] <- b[[1]]$"F value"[1]
#   F_Pers[i] <- b[[1]]$"F value"[2]
#   F_PersExp[i] <- b[[1]]$"F value"[3]
# }
# 
# 
# hist(sample(paths$pathDist, length(paths$pathDist)), breaks=10)
# hist(sample(paths$pathDist, length(paths$pathDist)), breaks=10)
# 
# probS <- length(F_Exp[F_Exp >= F_Experiment + .Machine$double.eps ^0.5])/nreps
# probM <- length(FM[F_Pers >= F_Person+ .Machine$double.eps ^0.5])/nreps       
# probSM  <-  length(F_PersExp[F_PersExp >= F_interact + .Machine$double.eps ^0.5])/nreps
# 
# ### The addition of "+ .Machine$double.eps" is an aid against two numbers that differ only by
# ### floating point computer calculations at the extreme.
# 
# probM
# 
# F_Experiment
# 
# F_Pers
# 
# 
# cat(" The probability value for the interaction is ",probSM, "\n")
# cat(" The probability value for Experiment is ", probS, "\n")
# cat(" The probability value for Person is ", probM, "\n")
# 
# 
# 
# 
# 
# kruskal.test(pathHeight ~ Person, data = paths)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# kruskal.test(pathDist ~ Person + Experiment + Experiment:Person, data = paths)
# 
# 
# pairwise.wilcox.test(pathDist, Person, p.adjust.method="BH", data=paths)
# 
# 
# ?kruskal.test()
# 
# 
# 
# # ???????????
# 
# # Two way ANOVA
# aov1 <- aov(pathDist ~ Person, data = paths)
# # aov1 <- aov(pathDist ~ Experiment * Person, data = paths) #Equivalent
# 
# # Get residuals from the model
# residuals <- aov1$residuals
# 
# 
# # --- Normality assumption
# 
# # Create histogram of residuals
# par(mfrow=c(1,1))
# jpeg(file= getJpgFilePath("hist_residuals_pathDist_Experiment_Person"))
# histWithNorm(residuals, breaks = 60)
# dev.off()
# 
# # Create histogram of residuals
# par(mfrow=c(1,1))
# jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
# qqnorm(aov1$residuals)
# qqline(residuals)
# dev.off()
# 
# 
# # Use shapiro test. 
# # H0: Normal distribution is normal 
# shapiro.test(x = residuals)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Residuals are not normally distributed
# 
# 
# hist(paths$Person:paths$pathDist)
# 
# # --- Homoscedasticity
# 
# # Test for homogenity of variance by doing a leveneTest
# # H0: population variances are equal
# leveneTest(pathDist ~ Experiment*Person, data = paths)
# 
# # Since p-value < 0.05, we reject the null-hypothesis. 
# # Population variances are not equal
# 
# 
# # TODO
# # Maybe it has something to do with the experiment without an obstacle? Test this
# # Test assumptions with log-transformed data also
# 
# 
# 
# # Based on this we decide to do a permutation-test to test if person has a significant effect
# 
# 
# boxplot(paths$pathDist~paths$Person)
# 
# 
# 
# 
# 
# 
# # # ==================================================
# # # TESTING CODE BELOW
# # 
# # 
# # pathDist <- paths$pathDist
# # logPathDist <- log(pathDist)
# # 
# # par(mfrow=c(1,2))
# # 
# # # normally distributed
# # h <- hist(pathDist, breaks = 30, main="Histogram of pathDist")
# # xfit <- seq(min(pathDist), max(pathDist), length = 40) 
# # yfit <- dnorm(xfit, mean = mean(pathDist), sd = sd(pathDist)) 
# # yfit <- yfit * diff(h$mids[1:2]) * length(pathDist) 
# # lines(xfit, yfit, col = "black", lwd = 2)
# # 
# # 
# # # normally distributed
# # h <- hist(logPathDist, breaks = 30, main="Histogram of log(pathDist)")
# # xfit <- seq(min(logPathDist), max(logPathDist), length = 40) 
# # yfit <- dnorm(xfit, mean = mean(logPathDist), sd = sd(logPathDist)) 
# # yfit <- yfit * diff(h$mids[1:2]) * length(logPathDist) 
# # lines(xfit, yfit, col = "black", lwd = 2)
# # 
# # 
# # par(mfrow=c(1,2))
# # 
# # qqnorm(pathDist, main='QQ-plot of pathDist')
# # qqline(pathDist)
# # 
# # qqnorm(logPathDist, main='QQ-plot of log(pathDist)')
# # qqline(logPathDist)
# # 
# # 
# # 
# # # Two way ANOVA model 
# # L <- aov(pathDist ~  Experiment + Person + Experiment:Person, data = paths)
# # summary(L)
# # 
# # 
# # 
# # # Get residuals from the model
# # residuals <- L$residuals
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("hist_residuals_pathDist_Experiment_Person"))
# # histWithNorm(residuals, breaks = 60)
# # dev.off()
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
# # qqnorm(L$residuals)
# # qqline(residuals)
# # dev.off()
# # 
# # # LOG-Transformed
# # 
# # # Two way ANOVA model 
# # L_log <- aov(logPathDist ~ Experiment + Person + Experiment:Person, data = paths)
# # summary(L_log)
# # 
# # # Get residuals from the model
# # residuals_log <- L_log$residuals
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("hist_residuals_log_pathDist_Experiment_Person"))
# # histWithNorm(residuals_log, breaks = 60)
# # dev.off()
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("qqplot_residuals_log_pathDist_Experiment_Person"))
# # qqnorm(residuals_log)
# # qqline(residuals_log)
# # dev.off()
# # 
# # 
# # pathHeight <- paths$pathHeight
# # logPathHeight <- log(pathHeight)
# # 
# # par(mfrow=c(1,2))
# # # 
# # # # normally distributed
# # # h <- hist(pathHeight, breaks = 30, main="Histogram of pathDist")
# # # xfit <- seq(min(pathHeight), max(pathHeight), length = 40) 
# # # yfit <- dnorm(xfit, mean = mean(pathHeight), sd = sd(pathHeight)) 
# # # yfit <- yfit * diff(h$mids[1:2]) * length(pathHeight) 
# # # lines(xfit, yfit, col = "black", lwd = 2)
# # # 
# # # 
# # # # normally distributed
# # # h <- hist(logPathHeight, breaks = 30, main="Histogram of log(pathDist)")
# # # xfit <- seq(min(logPathHeight), max(logPathHeight), length = 40) 
# # # yfit <- dnorm(xfit, mean = mean(logPathHeight), sd = sd(logPathHeight)) 
# # # yfit <- yfit * diff(h$mids[1:2]) * length(logPathHeight) 
# # # lines(xfit, yfit, col = "black", lwd = 2)
# # 
# # 
# # # par(mfrow=c(1,2))
# # # 
# # # qqnorm(pathDist, main='QQ-plot of pathDist')
# # # qqline(pathDist)
# # # 
# # # qqnorm(logPathDist, main='QQ-plot of log(pathDist)')
# # # qqline(logPathDist)
# # 
# # 
# # 
# # # Two way ANOVA model 
# # L <- aov(pathHeight ~ Experiment + Person + Experiment:Person, data = paths)
# # summary(L)
# # 
# # 
# # 
# # 
# # L <- aov(pathHeight ~ Experiment:Person, data = paths)
# # summary(L)
# # 
# # 
# # # Get residuals from the model
# # residuals <- L$residuals
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("hist_residuals_pathDist_Experiment_Person"))
# # histWithNorm(residuals, breaks = 60)
# # dev.off()
# # 
# # # Create histogram of residuals
# # par(mfrow=c(1,1))
# # jpeg(file= getJpgFilePath("qqplot_residuals_pathDist_Experiment_Person"))
# # qqnorm(L$residuals)
# # qqline(residuals)
# # dev.off()
# # 
