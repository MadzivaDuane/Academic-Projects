#Predictors of Body Fat

data <- read.csv("~/Documents/Academics/Other/BIS 505 Biostats for PH II/Datasets/body_fat.csv")
body_fat <- data; head(body_fat); dim(body_fat)

#website on using cook's distance or dffts to identify outliers: 
#https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html

#general relationships between variables 
pairs(body_fat, pch = 16)
boxplot_body_fat <- boxplot(body_fat$Percent_Body_Fat)
print(body_fat[which(body_fat$Percent_Body_Fat == boxplot_body_fat$out),])
print(paste0("The outlier is: ", boxplot_body_fat$out, " which is row 216"))
boxplot(body_fat$Density)

#correlation matrix to remove variables that are colinear
library(corrplot)
library(RColorBrewer)
correlation_matrix<-cor(body_fat)  #only include numeric variables
corrplot(correlation_matrix, type="upper", order="hclust",col=brewer.pal(n=6, name="RdYlBu"), addCoef.col = "black", number.cex = 0.75)

#initial model
initial_model <- lm(Percent_Body_Fat ~ Age+Weight+Height+Neck_Circ+Chest_Circ+Abdomen_Circ+Hip_Circ+
                      Thigh_Circ+Knee_Circ+Ankle_Circ+Bicep_Circ+Forearm_Circ+Wrist_Circ, data = body_fat)
summary(initial_model)
#diagnositcs and evaluation of mode: also outlier analysis 
par(mfrow = c(2,2))
for (i in 1:4){
  plot(initial_model, i)
}     #Row 39 is an outlier by cook's distance 
#additional outlier identifier
par(mfrow = c(1,1))
library(car)
influencePlot(initial_model, main="Influence Plot")
print("Row 39 and 224 are outliers")
#other useful diagnostic methods: using OLSSR
library(olsrr)
#website for instructions: https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html
ols_plot_cooksd_bar(initial_model)
ols_plot_cooksd_chart(initial_model)
ols_plot_dffits(initial_model)
ols_plot_resid_stand(initial_model)
ols_plot_resid_lev(initial_model)
#removes outliers and remove colinear variables 
final_body_fat <- body_fat[-c(39, 224),]
#confirm removal of outlier rows
dim(body_fat); dim(final_body_fat)
#removes colinear variables 
#includes both pair plots and correlations
library(GGally)
potential_predictors_data <- final_body_fat[, -c(1,2)]
ggpairs(potential_predictors_data)
#perform Farrar – Glauber test : https://datascienceplus.com/multicollinearity-in-r/
library(mctest)
omcdiag(potential_predictors_data, final_body_fat$Percent_Body_Fat)
#this confirms that of the 6 tests, 5 confirmed the presence of colinearity
#now whcih variables are the problem?
imcdiag(x = potential_predictors_data, y = final_body_fat$Percent_Body_Fat)
#from this, potential cause of colinearity are: Weight, Chest_Circ, Abdomen_Circ,
#Hip_Circ, Thigh_Circ, Knee_Circ
#conduct pairwise t-test for independence and find which variable pairs are statistically significant
library(ppcor)
pairwise_independecen_test <- pcor(potential_predictors_data, method = "pearson")
print(paste0("Maxmimum estimate is: ",max(pairwise_independecen_test$estimate[pairwise_independecen_test$estimate < 1])))
print(paste0("Minimum estimate is: ",min(pairwise_independecen_test$estimate[pairwise_independecen_test$estimate < 1])))
print(paste0("Hence range of estimates is: ", min(pairwise_independecen_test$estimate[pairwise_independecen_test$estimate < 1]),
      " to ", max(pairwise_independecen_test$estimate[pairwise_independecen_test$estimate < 1])))

#model for OLSRR package
olsrr_model <- lm(Percent_Body_Fat ~ ., data = final_body_fat[, -c(1)])
summary(olsrr_model)
ols_best_subset_interactions <- ols_step_best_subset(olsrr_model)
print(ols_best_subset_interactions)

#most parsimonious model:
best_model <- lm(Percent_Body_Fat ~ Age+Weight+Neck_Circ+Abdomen_Circ+Thigh_Circ+Ankle_Circ+Bicep_Circ+Forearm_Circ+Wrist_Circ
                   , data = final_body_fat[, -c(1)])
summary(best_model)
#final model diagnostics
par(mfrow = c(1,2))
for (i in 1:2){
  plot(initial_model, i)
}
par(mfrow = c(1,1))









