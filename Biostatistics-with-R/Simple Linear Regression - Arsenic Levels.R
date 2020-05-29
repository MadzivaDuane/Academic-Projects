#Simple Linear Regression
#Arsenic levels in well water and toenails of 21 New Hampshire residents.

#read in data 
data_arsenic <- read.csv("~/Documents/Academics/Other/BIS 505 Biostats for PH II/Datasets/data_arsenic.csv"); head(data_arsenic)

#cor test on arsenic in toenails and water
cor_val <- cor.test(data_arsenic$Arsenic_water, data_arsenic$Arsenic_toes)
print(paste0("The Pearson correlation coefficient is: ",round(cor_val$estimate,3), " with a p-value of: ", format(cor_val$p.value, digits = 3)))
#scatter plots of arsenic in toenails by age, arsenic in water 
par(mfrow=c(3,1))
plot(data_arsenic$Age, data_arsenic$Arsenic_toes, xlab = "Age", ylab = 'Arsenic levels in Toenails',
     main = "Arsenic Levels in Toanails by Age", pch = 16)
plot(data_arsenic$Arsenic_water, data_arsenic$Arsenic_toes, xlab = "Arsenic levels in Water", ylab = "Arsenic levels in Toenails",
       main = "Arsenic Levels in Toanails by Arsenic Levels in Well-water", pch = 16)
#plot log data (arsenic in water vs arsenic in toe nails)
plot(log(data_arsenic$Arsenic_water), log(data_arsenic$Arsenic_toes), xlab = "Arsenic levels in Water", ylab = "Arsenic levels in Toenails",
main = "Arsenic Levels in Toanails by Age", pch = 16)
par(mfrow=c(1,1))
#test for normality
shapiro.test(data_arsenic$Arsenic_toes); shapiro.test(data_arsenic$Arsenic_water)
#create linear model with and without log
lm_arsenic <- lm(data_arsenic$Arsenic_toes ~ data_arsenic$Arsenic_water)
log_y <- log(data_arsenic$Arsenic_toes) 
log_x <- log(data_arsenic$Arsenic_water)
log_x[is.infinite(log_x)] <- NA; log_y[is.infinite(log_y)] <- NA  #just in case there are log(x or y) = INF
lm_log_arsenic <- lm(log_y ~ log_x , na.action=na.omit)
summary(lm_arsenic); summary(lm_log_arsenic)

#fit regression line to scatter plot 
#original data
plot(data_arsenic$Arsenic_water, data_arsenic$Arsenic_toes, xlab = "Arsenic levels in Water", ylab = "Arsenic levels in Toenails",
     main = "Arsenic Levels in Toanails by Arsenic Levels in Well-water", pch = 16)+abline(lm_arsenic)
text(0.04, 1.8, adj=c(0,0), labels="Toenails Level = 0.155 + 12.99*Well-Water Level", col = c("blue"))
if (round(lm_arsenic$coefficients[1],3) == 0.155) {
  print("Model confirmed to be correct as per book. Slope = 12.99 and intercept = 0.155")
}else {
  print("Model not confirmed")
}

#fix regression to ensure we have a non-zero itnercept 
lm_arsenic_fitted <- lm(data_arsenic$Arsenic_toes ~ 0+data_arsenic$Arsenic_water)
summary(lm_arsenic_fitted)
#plot with zero intercept
plot(data_arsenic$Arsenic_water, data_arsenic$Arsenic_toes, xlab = "Arsenic levels in Water", ylab = "Arsenic levels in Toenails",
     main = "Arsenic Levels in Toanails by Arsenic Levels in Well-water", pch = 16)+abline(lm_arsenic_fitted)
text(0.04, 1.8, adj=c(0,0), labels="Toenails Level = 14.87*Well-Water Level")

if (round(lm_arsenic_fitted$coefficients[1],2) == 14.87) {
  print("Fitted model confirmed to be correct as per book. Slope = 14.87")
} else {
  print("Fitted model not confirmed")
}

#combining both plots, with and without an intercept
plot(data_arsenic$Arsenic_water, data_arsenic$Arsenic_toes, xlab = "Arsenic levels in Water", ylab = "Arsenic levels in Toenails",
     main = "Arsenic Levels in Toanails by Arsenic Levels in Well-water", pch = 16)+abline(lm_arsenic, col = "blue")+abline(lm_arsenic_fitted, col = "red")
text(0.092, 1.3, adj=c(0,0), labels="Toenails Level = 0.155 + 12.99*Well-Water Level", col = "blue", cex = 0.5)
text(0.08, 1.8, adj=c(0,0), labels="Toenails Level = 14.87*Well-Water Level", col = "red", cex = 0.5)

#Diagnositics of Final Model (with intercept)
#plot residuals using non-zero intercept model
res <- lm_arsenic$residuals
fit_vals <- lm_arsenic$fitted.values
#plot qqplot
qqnorm(res, main = "Normal Q-Q Plot", pch = 16); qqline(res)
print("Good model, given residuals have a linear pattern")
#residual agains fitted values
plot(fit_vals,res, pch = 16, main = "Residual plot - Predicted vs Standardized Residuals", 
     xlab = "Predicted", ylab = "Standardized Residuals") + abline(h = 0, lty = 2)
print("Scatter plot not well scattered, may suggest a skew in the data. A perfect model would have a more scattered pattern")












