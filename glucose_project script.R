

###################
#CAPSTONE PROJECT 2
###################
#GLUCOSE PROJECT


# Note: this process could take a couple of minutes
if(!require(foreign)) install.packages("foreign")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(timeDate)) install.packages("timeDate")
if(!require(corrplot)) install.packages("corrplot")
if(!require(mlr)) install.packages("mlr")
if(!require(caret)) install.packages("caret")
                             
# All libraraies needed
library(foreign)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(timeDate)
library(corrplot)
library(mlr)
library(caret)

#\newpage

#1 EXECUTIVE SUMMARY
#The aim of this project is to show if the age and the weight influence on the glycemia (glucose). 
#The Glycemia refers to the concentration of sugar or glucose in the blood. 
#In many of the European countries blood glucose or sugar is also measured as millimol per decilitre (mmol/dl). 
#These measurements are referred to as the SI units.
#If the blood glucose level above 6.1 mmol/l or 1.10 g/l (hyperglycemia and less than 3.5 mmol/l or 0.54 g/l (hypoglycemia)
#When fasting blood glucose is above 7 mmol/l (1.26 g/l), the diagnosis of diabetes is made. 

#2 METHODS AND ANALYSIS
#In this section, I'll present the methods and the analysis of the data. Before continuing the process, 
#it's really important to discover the variables (names, types, counts, lenght etc.)

#Loading raw Data set
glucose <- read.csv("C:/Users/vnh/Desktop/glc.csv")

#Getting descriptive statistics
str(glucose)
#> This dataset contains 10 variables and 284 observations.

#Type of variable :
#- id "integer" = unique identification value per patient
#- chol "integer" = unique identification value of cholesterol per patient
#- glyhb "numeric" = unique identification value of glycemia per patient
#- location "factor" = specific city per patient
#- age "integer" = age of patient
#- gender "factor" = gender female or male
#- height "numeric" = unique identification value per patient in cm
#- weight "numeric" = unique identification value per patient in kg
#- waist "integer" = unique identification value per patient in cm
#- age_cat "factor" = age groups 20-39/40-59/60+ years


#checking for missing variables
sapply(glucose, function(x) sum(is.na(x)))

#looking closer attributes and data values for:
#glyhb variable
summary(glucose$glyhb)
skewness(glucose$glyhb)
kurtosis(glucose$glyhb)
#Histogram
dfglyhb <- data.frame(glucose$glyhb)
ggplot(dfglyhb, aes(x = glucose$glyhb), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Glycemia'))) + 
  ylab(expression(bold('Density')))
#Boxplot
boxplot(glucose$glyhb)


#weight variable
summary(glucose$weight)
skewness(glucose$weight)
kurtosis(glucose$weight)
#Histogram
dfweight <- data.frame(glucose$weight)
ggplot(dfweight, aes(x = glucose$weight), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('weight'))) + 
  ylab(expression(bold('Density')))

#age variable
summary(glucose$age)
skewness(glucose$age)
kurtosis(glucose$age)
#Histogram
dfage <- data.frame(glucose$age)
ggplot(dfage, aes(x = glucose$age), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('age'))) + 
  ylab(expression(bold('Density')))


#chol variable
summary(glucose$chol)
skewness(glucose$chol)
kurtosis(glucose$chol)
#Histogram
dfchol <- data.frame(glucose$chol)
ggplot(dfchol, aes(x = glucose$chol), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'black', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('age'))) + 
  ylab(expression(bold('Density')))
#Boxplot
boxplot(glucose$chol)



#Bivariate/Multivariate Analysis
#Correlation Plot
#We are now interested in how the 10 predictors relate to each other. 
#To see bivariate relationships among these predictors, we calculate correlations between them.

# calculate collinearity
glucose1 <- glucose[ ,c("chol","glyhb","age", "height", "weight", "waist")]
corrglucose <- cor(glucose1[,2:6])
corrplot(corrglucose, order = "hclust", tl.cex = 0.7)

# Linear regression
#Linear regression between glycemia and weight:
#Following this score and the scatter plot, there is a weak positive linear relationships between these variables.
cor(glucose$glyhb, glucose$weight, method = c("pearson", "kendall", "spearman")) #> 0.098
glycemia <- glucose$glyhb
weight <- glucose$weight
scatter.smooth(x=glycemia, y=weight, main="glycemia ~ kg")  # scatterplot


#Linear regression between glycemia and cholesterol:
#As we can see, there is a weak positive linear regression.
cor(glucose$glyhb, glucose$chol, method = c("pearson", "kendall", "spearman")) #> 0.113
glycemia <- glucose$glyhb
cholesterol <- glucose$chol
scatter.smooth(x=glycemia, y=cholesterol, main="glycemia ~ mmol/dl")  # scatterplot

#Linear regression between glyemia and age:
#Here we see a slight positive correlation between these variables.
cor(glucose$glyhb, glucose$age, method = c("pearson", "kendall", "spearman")) #> 0.307
glycemia <- glucose$glyhb
age <- glucose$age
scatter.smooth(x=glycemia, y=age, main="glycemia ~ age")  # scatterplot

#Linear regression between glyemia and wast:
#The score and the scatter plot show us a slight positive correlation between these variables.
cor(glucose$glyhb, glucose$waist, method = c("pearson", "kendall", "spearman")) #> 0.171
glycemia <- glucose$glyhb
scatter.smooth(x=glycemia, y=age, main="glycemia ~ waist")  # scatterplot


#Data processing

#Principal Components Analysis (PCA) transform
#Principal component analysis is a statistical technique that is used to analyze the interrelationships among a large number of variables and to explain these variables in terms of a smaller number of variables, called principal components, with a minimum loss of information.
#It constructs a set of orthogonal (non-collinear, uncorrelated, independent) variables and is used for making predictive models
#glucose.pca <- prcomp(glucose1, center=TRUE, scale=TRUE)
plot(glucose.pca, type="l", main='')
grid(nx = 6, ny = 5)
title(main = "Principal components weight", sub = NULL, xlab = "Components")
box()

summary(glucose.pca)


#Calculate the proportion of variance explained
pca_var <- glucose.pca$sdev^2
pve_df <- pca_var / sum(pca_var)
cum_pve <- cumsum(pve_df)
pve_table <- tibble(comp = seq(1:ncol(glucose1)), pve_df, cum_pve)

ggplot(pve_table, aes(x = comp, y = cum_pve)) + 
  geom_point() + 
  geom_abline(intercept = 0.95, color = "red", slope = 0)

pca_df <- as.data.frame(glucose.pca$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=glucose$glyhb)) + geom_point(alpha=0.5)


#Split data set in train 65% and test 35%
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y = glucose$glyhb, p = 0.65, list = FALSE)
train_set <- glucose[test_index,]
test_set <- glucose[-test_index,]

nrow(train_set) #>[1] 187
nrow(test_set)  #>[1] 97

#Overview edx and validation dataset

head(train_set) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

head(test_set) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 10,
                full_width = FALSE)

#Naive Bayes
#Average of all glycemia
mu_hat <- mean(glucose$glyhb)
mu_hat #> 4.687

#Predict the RMSE on the validation set
rmse_mean_model_result <- RMSE(test_set$glyhb, mu_hat)
rmse_mean_model_result

#Finally a dataframe of result
results <- data.frame(model="Naive Mean-Baseline Model", RMSE=rmse_mean_model_result)
results #> 4.687

results %>% 
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                position = "center",
                font_size = 12,
                full_width = FALSE)


lmMod <- lm(glyhb ~ weight, data=glucose)  # build the model
glyPred <- predict(lmMod, test_set)  # predict glycemia
summary (lmMod)  # model summary

lmMod2 <- lm(glyhb ~ age, data=glucose)  # build the model
glyPred2 <- predict(lmMod2, test_set)  # predict glycemia
summary (lmMod2)  # model summary

AIC(lmMod, lmMod2)
BIC(lmMod, lmMod2)
sigma(lmMod)/mean(glucose$glyhb) #Average prediction error rate


#Machine Learning
#Develop the model on the training data and use it to predict the distance on test data
lmMod <- lm(glyhb ~ weight, data=glucose)  # build the model
glyPred <- predict(lmMod, test_set)  # predict glycemia
summary (lmMod)  # model summary
AIC (lmMod)  # Calculate akaike information criterion

#Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test_set$glyhb, predicteds=glyPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  



#Cross-validation methods
model <- lm(glyhb ~., data = train_set)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test_set)
data.frame( R2 = R2(predictions, test_set$glyhb),
            RMSE = RMSE(predictions, test_set$glyhb),
            MAE = MAE(predictions, test_set$glyhb))
RMSE_K_FOLD_CROSS_VALIDATION <- RMSE(predictions, test_set$glyhb)
#When comparing two models, the one that produces the lowest test sample RMSE is the preferred model.
#The RMSE and the MAE are measured in the same scale as the outcome variable. Dividing the RMSE by the average value of the outcome variable will give you the prediction error rate, which should be as small as possible:
  
RMSE(predictions, test_set$glyhb)/mean(test_set$glyhb)
#Note that, the validation set method is only useful when you have a large data set that can be partitioned. 
#Therefore, the test error rate can be highly variable, depending on which observations are included in the training set and which observations are included in the validation set.


#K-fold cross-validation
#Define training control
set.seed(200, sample.kind="Rounding")
train.control <- trainControl(method = "cv", number = 10)
#Train the model
model <- train(glyhb ~., data = glucose, method = "lm",
               trControl = train.control)
#Summarize the results
print(model)

#Repeated K-fold cross-validation
#The process of splitting the data into k-folds could be repeatedas many times as wished and called repeated k-fold cross validation.
#The final model error is taken as the mean error from the number of repeats.
#Define training control
set.seed(200)
train.control2 <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
#Train the model
model2 <- train(glyhb ~., data = glucose, method = "lm",
               trControl2 = train.control2)
#Summarize the results
print(model2)


# Adding the results to the results dataset
results <- results %>% add_row(model="K-fold cross-validation", RMSE=RMSE_K_FOLD_CROSS_VALIDATION)
results


#APPENDIX
sessionInfo()

