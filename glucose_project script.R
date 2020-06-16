

###################
#CAPSTONE PROJECT 2
###################
#GLYCEMIA PROJECT


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

\newpage

#1 EXECUTIVE SUMMARY
The aim of this project is to show if the age and the weight influence on the glycemia (glucose). 
The Glycemia refers to the concentration of sugar or glucose in the blood. 
In many of the European countries blood glucose or sugar is also measured as millimol per decilitre (mmol/dl). 
These measurements are referred to as the SI units.
If the blood glucose level above 6.1 mmol/l or 1.10 g/l (hyperglycemia and less than 3.5 mmol/l or 0.54 g/l (hypoglycemia)
When fasting blood glucose is above 7 mmol/l (1.26 g/l), the diagnosis of diabetes is made. 

#2 METHODS AND ANALYSIS
#Loading raw Data set
glucose <- read.csv("C:/Users/vnh/Desktop/glc.csv")

#Getting descriptive statistics
str(glucose)
#> This dataset contains 10 variables and 284 observations.

#Type of variable :
- id "integer" = unique identification value per patient
- chol "integer" = unique identification value of cholesterol per patient
- glyhb "numeric" = unique identification value of glycemia per patient
- location "factor" = specific city per patient
- age "integer" = age of patient
- gender "factor" = gender female or male
- height "numeric" = unique identification value per patient in cm
- weight "numeric" = unique identification value per patient in kg
- waist "integer" = unique identification value per patient in cm
- age_cat "factor" = age groups 20-39/40-59/60+ years


"str(glucose$location)
locations <- c("Buckingham", "Louisa")
locations.factor <- factor(locations)
locations.factor
as.character(locations.factor)"

#checking for missing variables
sapply(glucose, function(x) sum(is.na(x)))

#looking closer attributes and data values for:
#glyhb
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


#weight
summary(glucose$weight)
skewness(glucose$weight)
kurtosis(glucose$weight)
#Histogram
dfweight <- data.frame(glucose$weight)
ggplot(dfweight, aes(x = glucose$weight), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'blue', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('weight'))) + 
  ylab(expression(bold('Density')))

#age
summary(glucose$age)
skewness(glucose$age)
kurtosis(glucose$age)
#Histogram
dfage <- data.frame(glucose$age)
ggplot(dfage, aes(x = glucose$age), binwidth = 2) + 
  geom_histogram(aes(y = ..density..), fill = 'green', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('age'))) + 
  ylab(expression(bold('Density')))


#chol
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
glucose1
corrglucose <- cor(glucose1[,2:6])
corrplot(corrglucose, order = "hclust", tl.cex = 0.7)

cor(glucose$age, glucose$glyhb, method = c("pearson", "kendall", "spearman"))
cor(glucose$weight, glucose$waist, method = c("pearson", "kendall", "spearman"))
cor(glucose$height, glucose$glyhb, method = c("pearson", "kendall", "spearman"))


# Simple Scatterplot
attach(glucose$age)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
#Scatter plot
set.seed(1705)
n <- 284
d <- data.frame(a = rnorm(n))
d$b <- .4 * (d$a + rnorm(n))

ggplot(d, aes(glucose$glyhb, glucose$weight, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")
