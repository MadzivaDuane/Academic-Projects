#Halibut Fish Survival
file <- "~/Documents/.../halibut.csv"
halibut_survival <- read.csv(file); head(halibut_survival); head(halibut_survival); dim(halibut_survival)

#Cox proportional regression model: http://www.karlin.mff.cuni.cz/~pesta/NMFM404/ph.html

#create a pairs plot to identify interesting relationships between variables 
library(GGally)
library(survminer)
library(survival)
ggpairs(halibut_survival[, c(1, 3, 4, 5, 6, 7)])
pairs(halibut_survival[, c(1, 3, 4, 5, 6, 7)])   #nothing is interesting here 

#histogram for survival times
hist(halibut_survival$Survival_Time, breaks = 100, 
     xlab = "Time (hours)", ylab = "Frequency", main = "Histogram of Halibut Survival Time", col = c("blue"), ylim = c(0, 60))

#univariate analysis - K-M Curve covariate categories 
table(halibut_survival$Time_Trawl)  #c(30, 100)
table(halibut_survival$Time_Difference_Tow)   #c(1, 2, 3, 4, 5, 6, 8, 10, 13, 15, 23, 49, 58)
halibut_survival$Time_Difference_Tow_Categories <- 0
halibut_survival$Time_Difference_Tow_Categories[which(halibut_survival$Time_Difference_Tow > 6)] <- 1
table(halibut_survival$Time_Difference_Tow_Categories)
table(halibut_survival$Fork_Length)  #c(less than 42, greater than 42)
halibut_survival$Fork_Length_Categories <- 0
halibut_survival$Fork_Length_Categories[which(halibut_survival$Fork_Length > 43)] <- 1
table(halibut_survival$Fork_Length_Categories)  #c(153, 141)
table(halibut_survival$Handling_Time)
halibut_survival$Handling_Time_Categories <- 0
halibut_survival$Handling_Time_Categories[which(halibut_survival$Handling_Time > 10)] <- 1
table(halibut_survival$Handling_Time_Categories)   #c(149, 145)
#survival plots
covariates <- halibut_survival[, c("Time_Trawl", "Time_Difference_Tow_Categories", "Fork_Length_Categories", "Handling_Time_Categories")]
par(mfrow=c(2,2))
plot(survfit(Surv(Survival_Time, Censoring) ~ Time_Trawl, data = halibut_survival), lwd = 2, mark.time = TRUE,
     xlab = "Time (hours)", ylab = "Overall Survival Fraction", col = c("red", "blue"), main = "Survival Analysis - Time Trawled")
legend("topright", legend = c("30 minutes", "100 minutes"), fill = c("red", "blue"))
plot(survfit(Surv(Survival_Time, Censoring) ~ Time_Difference_Tow_Categories, data = halibut_survival), lwd = 2, mark.time = TRUE,
     xlab = "Time (hours)", ylab = "Overall Survival Fraction", col = c("red", "blue"), main = "Survival Analysis - Towing Depth")
legend("topright", legend = c("<= 6 meters", "> 6 meters"), fill = c("red", "blue"))
plot(survfit(Surv(Survival_Time, Censoring) ~ Fork_Length_Categories, data = halibut_survival), lwd = 2, mark.time = TRUE,
     xlab = "Time (hours)", ylab = "Overall Survival Fraction", col = c("red", "blue"), main = "Survival Analysis - Fork Length")
legend("topright", legend = c("<= 43 cm", "> 43 cm"), fill = c("red", "blue"))
plot(survfit(Surv(Survival_Time, Censoring) ~ Handling_Time_Categories, data = halibut_survival), lwd = 2, mark.time = TRUE,
     xlab = "Time (hours)", ylab = "Overall Survival Fraction", col = c("red", "blue"), main = "Survival Analysis - Handling Time")
legend("topright", legend = c("<= 10 minutes", "> 10 minutes"), fill = c("red", "blue"))

#log rank tests
survdiff(Surv(Survival_Time, Censoring) ~ Time_Trawl, data = halibut_survival)
survdiff(Surv(Survival_Time, Censoring) ~ Time_Difference_Tow_Categories, data = halibut_survival)
survdiff(Surv(Survival_Time, Censoring) ~ Fork_Length_Categories, data = halibut_survival)
survdiff(Surv(Survival_Time, Censoring) ~ Handling_Time_Categories, data = halibut_survival)

#creating cox proportional hazards model 
mutlivariate_1 <- coxph(Surv(Survival_Time, Censoring) ~ Time_Trawl + Time_Difference_Tow + Fork_Length + Handling_Time + Log_Total_Catch, data = halibut_survival); summary(mutlivariate_1)
#from this, we see that Time_Difference_Tow is not statistically signifncant hence we remove it 
multivariate_final <- coxph(Surv(Survival_Time, Censoring) ~ Time_Trawl + Fork_Length + Handling_Time + Log_Total_Catch, data = halibut_survival)
summary(multivariate_final)
#cox proportional hazard rate ratios
exp(multivariate_final$coefficients)

#test assumptions for proportional hazards
test_assumptions <- cox.zph(multivariate_final); test_assumptions # p-value for global is statistically significant hence we cannot assume proportional hazards

#diagnostic plots - residual plots require that dispersion around 0 be equal, well dispersed points, good model
par(mfrow=c(1,2))
#martingale residual plot
martres <- residuals(multivariate_final, type = "martingale")
plot(multivariate_final$linear.predictors, martres, xlab = "Linear Predictor", ylab = "Martingale Residuals", col = c("blue"),
     main = "Martingale Residuals Plot")
abline(0,0, lty = 2)
#deviance residual plot
devres <-  residuals(multivariate_final, type="deviance")
plot(multivariate_final$linear.predictors, devres, xlab = "Linear Predictor", ylab = "Deviance Residuals", col = c("red"),
     main = "Deviance Residuals Plot")
abline(0,0, lty = 2)
par(mfrow=c(1,1))












