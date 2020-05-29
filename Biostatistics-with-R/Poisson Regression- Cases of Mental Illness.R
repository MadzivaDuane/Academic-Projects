#Cases of Mental Illness
#logistic regression evaluation: https://online.stat.psu.edu/stat504/node/169/

mental_illness <- read.csv("~/Documents/Academics/Other/BIS 505 Biostats for PH II/Datasets/mental_illness.csv")
head(mental_illness); dim(mental_illness)
colnames(mental_illness) <- c("County", "Cases", "Distance_to_Center", "Population", 
                              "Population_Density", "Percent_Cared_Home"); head(mental_illness)#given differences in population, scale population and use log of population
mental_illness$log_population <- log(mental_illness$Population); head(mental_illness)

#pairs plot for any intersting variations
library(GGally)
ggpairs(mental_illness[, 2:6])

#model using poisson model for both population and log population
#################################################################
#if the confidence interval has 1 in it it is not statistically signifncant 
#increase in 1 unit of cavariate increases or decreases the outcome by a factors of log(coefficient)
#################################################################
#population in 100s
poisson_model_ppn <- glm(Cases ~ Population, data = mental_illness, family = "poisson")
summary(poisson_model_ppn)
exp(cbind(results = coef(poisson_model_ppn), confint(poisson_model_ppn)))
#using log population
poisson_model_logppn <- glm(Cases ~ log_population, data = mental_illness, family = "poisson")
summary(poisson_model_logppn)
exp(cbind(results = coef(poisson_model_logppn), confint(poisson_model_logppn)))
#using population density
#Population Density = Number of People/Land Area
poisson_model_ppndensity <- glm(Cases ~ Population_Density, data = mental_illness, family = "poisson")
summary(poisson_model_ppndensity)
exp(cbind(results = coef(poisson_model_ppndensity), confint(poisson_model_ppndensity)))
#using distance from center
poisson_model_distance <- glm(Cases ~ Distance_to_Center, data = mental_illness, family = "poisson")
summary(poisson_model_distance)
exp(cbind(results = coef(poisson_model_distance), confint(poisson_model_distance)))

#using all variable
poisson_model_full <- glm(Cases ~ log_population+Population_Density+Distance_to_Center, data = mental_illness, family = "poisson")
summary(poisson_model_full)
exp(cbind(results = coef(poisson_model_full), confint(poisson_model_full)))

#model evalauation
#method 1: residual deviance/ DF = 1, if far above 1 model is not good enough, and 1-chisq(model$deviance, model$df.residual) > 0
poisson_model_full$deviance/poisson_model_full$df.residual  #close to one, so this is a better model than all the individual models 

#outliers and model diagnostics
inf_model <- influence(poisson_model_full)
model_hat <- fitted(poisson_model_full)
plot(model_hat, inf_model$dev.res)
br <- match(sort(inf_model$dev.res)[1], inf_model$dev.res)
text(model_hat[br], inf_model$dev.res[br], pos = 2, labels = mental_illness$County[br])

library(car)
influencePlot(poisson_model_full, main = "Residuals Plot")

#rerun model without outliers
mental_illness_no_noutliers <- mental_illness[-14, ]
poisson_model_full_no_outliers <- glm(Cases ~ log_population+Population_Density+Distance_to_Center, data = mental_illness_no_noutliers, family = "poisson")
summary(poisson_model_full_no_outliers)
exp(cbind(results = coef(poisson_model_full_no_outliers), confint(poisson_model_full_no_outliers)))
#diagnostics on final model 
poisson_model_full_no_outliers$deviance/poisson_model_full_no_outliers$df.residual











