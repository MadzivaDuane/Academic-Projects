#Intermediate Linear Regression (Stepwise), OLSSR, and Intermediate Model Diagnostics
#Fertility Rates in Switzerland

#read in data 
data <- read.csv("~/Documents/.../fertility rates.csv"); head(data); dim(data)
#rename colums
fertility_rates <- data
names(fertility_rates) <- c("Province", "Fertility_Rate", "% Population with Agriculture Occupation",
                            "% High Score on Army Exams", "% Higher Education", "% Catholic", "1-Year Infant Mortality")
#find any correlations 
pairs(fertility_rates)
#create a crrelation matrix - rename and make shorter variable names for the correlation plot 
library(corrplot)
library(RColorBrewer)
names(fertility_rates) <- c("Province", "Fert_Rate", "% Agric",
                            "% High Exams", "% Higher_Ed", "% Cath", "Inf_Mort")

correlation_matrix<-cor(fertility_rates[2:length(fertility_rates)])  #only include numeric variables
corrplot(correlation_matrix, type="upper", order="hclust",col=brewer.pal(n=6, name="RdYlBu"), addCoef.col = "black", number.cex = 0.75)

# What are the determinants of the fertility rate? ------------------------
#determine skewed variables
names(fertility_rates) <- c("Province", "Fertility_Rate", "% Population with Agriculture Occupation",
                            "% High Score on Army Exams", "% Higher Education", "% Catholic", "1-Year Infant Mortality")
shapiro_tests <- apply(fertility_rates[,c(-1)], 2, shapiro.test)
for (i in 1:length(shapiro_tests)){
  print(shapiro_tests[i])
}
print("Percentage higher education and % Catholic are non-normal and potentially skewed, need adjusting in final model")

#create full model no adjustment for skewed variables
full_model_fertility_rates <- lm(`Fertility_Rate` ~ ., data = fertility_rates[,c(-1)])
summary(full_model_fertility_rates)

#adjust for skwedness
#log(education)
full_eduadj_model_fertility_rates <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+
                                           `% High Score on Army Exams`+log(`% Higher Education`)+`% Catholic`+`1-Year Infant Mortality`, data = fertility_rates)
summary(full_eduadj_model_fertility_rates)
#log(% catholic)
full_catholicadj_fertility_rates <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+
                                      `% High Score on Army Exams`+`% Higher Education`+log(`% Catholic`)+`1-Year Infant Mortality`, data = fertility_rates)
summary(full_catholicadj_fertility_rates)
#log(both education and % catholic)
full_EduAndCathAdj_fertility_rates <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+
                                           `% High Score on Army Exams`+log(`% Higher Education`)+log(`% Catholic`)+`1-Year Infant Mortality`, data = fertility_rates)
summary(full_EduAndCathAdj_fertility_rates)

#stepwise linear regression
#using the olsrr package (https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html)
library(olsrr)
ols_model <- lm(`Fertility_Rate` ~ ., data = fertility_rates[,c(-1)])
ols_best_subset <-ols_step_best_subset(ols_model)
ols_all_possible <- ols_step_all_possible(ols_model)
#select the the best model predictors from list of all linear  models 
best_predictors <- ols_all_possible$predictors[which(ols_all_possible$adjr == max(ols_all_possible$adjr))] 
print(best_predictors)
#model with best predictors-no interaction terms included 
best_model_OLSRR <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+`% High Score on Army Exams`+`% Higher Education`+
                         `% Catholic`+`1-Year Infant Mortality`, data = fertility_rates)

#model with interactions using best possible model 
model_with_interaction_terms <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+`% High Score on Army Exams`+
                                     `% Higher Education`+`% Catholic`+`1-Year Infant Mortality`+`% High Score on Army Exams`*`% Higher Education`+
                                     `% Higher Education`*`% Catholic`+`% Higher Education`*`1-Year Infant Mortality`, data = fertility_rates[,c(-1)])
ols_best_subset_interactions <- ols_step_best_subset(model_with_interaction_terms)
print(ols_best_subset_interactions)
best_predictors_interactions <- ols_best_subset_interactions$predictors[which(ols_best_subset_interactions$adjr == max(ols_best_subset_interactions$adjr))]
print(best_predictors_interactions); print(max(ols_best_subset_interactions$adjr))

#model with best predictors- interaction terms included 
best_model_OLSRR_interactions <- lm(`Fertility_Rate` ~ `% Population with Agriculture Occupation`+`% High Score on Army Exams`+`% Higher Education`+
                                      `% Catholic`+`1-Year Infant Mortality`+`% Higher Education`*`% Catholic`, data = fertility_rates)
summary(best_model_OLSRR_interactions)

#evaluation and diagnostics of final model - with interaction terms 
par(mfrow = c(2,1))
for (i in 1:2){
  plot(best_model_OLSRR_interactions, i)
}
par(mfrow = c(1,1))

#Create Summary of All Models Created In this file
#use fstatistic to calculate raw value of the p-value bcause it cannot be extracted from the summary of the model
models_results <- data.frame("Model Name" = c("full_model_fertility_rates", "full_eduadj_model_fertility_rates", "full_catholicadj_fertility_rates", 
                                              "full_EduAndCathAdj_fertility_rates", "best_model_OLSRR", "best_model_OLSRR_interactions"),
                             "Adjusted R-Squared" = c(summary(full_model_fertility_rates)$adj.r.squared, summary(full_eduadj_model_fertility_rates)$adj.r.squared, 
                                                      summary(full_catholicadj_fertility_rates)$adj.r.squared, summary(full_EduAndCathAdj_fertility_rates)$adj.r.squared,
                                                      summary(best_model_OLSRR)$adj.r.squared, summary(best_model_OLSRR_interactions)$adj.r.squared),
                             "P-values" = c(pf(summary(full_model_fertility_rates)$fstatistic[1], summary(full_model_fertility_rates)$fstatistic[2], summary(full_model_fertility_rates)$fstatistic[3], lower.tail = FALSE),
                                            pf(summary(full_eduadj_model_fertility_rates)$fstatistic[1], summary(full_eduadj_model_fertility_rates)$fstatistic[2], summary(full_eduadj_model_fertility_rates)$fstatistic[3], lower.tail = FALSE),
                                            pf(summary(full_catholicadj_fertility_rates)$fstatistic[1], summary(full_catholicadj_fertility_rates)$fstatistic[2], summary(full_catholicadj_fertility_rates)$fstatistic[3], lower.tail = FALSE),
                                            pf(summary(full_EduAndCathAdj_fertility_rates)$fstatistic[1], summary(full_EduAndCathAdj_fertility_rates)$fstatistic[2], summary(full_EduAndCathAdj_fertility_rates)$fstatistic[3], lower.tail = FALSE),
                                            pf(summary(best_model_OLSRR)$fstatistic[1], summary(best_model_OLSRR)$fstatistic[2], summary(best_model_OLSRR)$fstatistic[3], lower.tail = FALSE),
                                            pf(summary(best_model_OLSRR_interactions)$fstatistic[1], summary(best_model_OLSRR_interactions)$fstatistic[2], summary(best_model_OLSRR_interactions)$fstatistic[3], lower.tail = FALSE)))
names(models_results) = c("Model Name", "Adjusted R-Squared", "P-values")  #rename the dataframe columns
print(models_results)













