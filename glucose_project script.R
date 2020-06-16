

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

                             
# All libraraies needed
library(foreign)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)


**EXECUTIVE SUMMARY**
The aim of this project is to show if the age and the weight influence on the glycemia (glucose). 
The Glycemia refers to the concentration of sugar or glucose in the blood. 
In many of the European countries blood glucose or sugar is also measured as millimol per decilitre (mmol/dl). 
These measurements are referred to as the SI units.
If the blood glucose level above 6.1 mmol/l or 1.10 g/l (hyperglycemia and less than 3.5 mmol/l or 0.54 g/l (hypoglycemia)
When fasting blood glucose is above 7 mmol/l (1.26 g/l), the diagnosis of diabetes is made. 


#Loading raw Data set
Glucose.rawdata <- read.csv("C:/Users/vnh/Desktop/glc.csv")

#Getting descriptive statistics
str(Glucose.rawdata)






