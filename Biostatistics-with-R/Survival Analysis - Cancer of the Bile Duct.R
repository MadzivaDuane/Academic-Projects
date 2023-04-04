#Cancer of the Bile Duct
file <- "~/Documents/.../survival.txt"
survival <- read.table(file, header = FALSE, sep = "")
colnames(survival) <- c("Time", "Censored", "Treatment"); head(survival); dim(survival)

#histogram for survival times
hist(survival$Time, breaks = length(survival$Time), 
     xlab = "Time (days)", ylab = "Frequency", main = "Histogram of Survival Time", col = c("blue"))

#load survival package
#shorter time is treatment 1 and longer time is treatment 0
#survival analysis
library(survival)
plot(survfit(Surv(Time, Censored) ~ Treatment, data = survival), lwd = 2, mark.time = TRUE,
     xlab = "Time (days)", ylab = "Overall Survival Fraction", col = c("red", "blue"), main = "Survival Analysis, Cancer of the Bile Duct")
legend(1500, 0.8, legend = c("Placebo", "Radiation and 5-FU"), fill = c("blue", "red"))
#with ggplot2's ggsurvplot
library(ggplot2)
library(survminer)
ggsurvplot(
  fit = survfit(Surv(Time, Censored) ~ Treatment, data = survival),
  pval = TRUE,
  xlab = "Days", 
  ylab = "Overall Survival probability")  #p-value argument prints log rank test pvalue

#1 year survival
survfit(Surv(Time, Censored) ~ Treatment, data = survival)

#log rank test to compare survival times 
# Log-rank or Mantel-Haenszel test
survdiff(Surv(Time, Censored) ~ Treatment, data = survival)

# Peto & Peto modification of the Gehan-Wilcoxon test
survdiff(Surv(Time, Censored) ~ Treatment, data = survival, rho = 1)

#conclusion: there is no evidence of the treatment being effective in our analysis 
