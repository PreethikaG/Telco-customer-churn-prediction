#Load dependencies

library(tidyverse)
#install.packages("naniar")
library(naniar)


# Read the data set

telco <- read.csv("C:/Users/conne/OneDrive/Documents/TelcoChurn.csv")

# Basic exploration
dim(telco)  # 7043 R * 21 C

str(telco)

colnames(telco)

glimpse(telco) 


vars <- c('gender', 'SeniorCitizen', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService',
'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies',
'Contract', 'PaperlessBilling' , 'PaymentMethod', 'Churn') 

#- should be converted as factors

# Variable classification
# classify a variable to be continuous if it has more than 10 distinct values
apply(telco, 2, unique)


# "customerID" - continuous
# "gender"  - binary     

telco_clean$gender <- as.factor(telco_clean$gender)




# "SeniorCitizen"  - binary
telco_clean$SeniorCitizen <- as.factor(telco_clean$SeniorCitizen)
# "Partner"  - binary

telco_clean$Partner <- as.factor(telco_clean$Partner)
# "Dependents" - binary  
telco_clean$Dependents <- as.factor(telco_clean$Dependents)
# "tenure"  - continuous         
# "PhoneService"  - binary
telco_clean$PhoneService <- as.factor(telco_clean$PhoneService)

# "MultipleLines"   - nominal
telco_clean$MultipleLines <- as.factor(telco_clean$MultipleLines)


# "InternetService" - nominal 
# "OnlineSecurity"  - nominal 
# "OnlineBackup"   - nominal
# "DeviceProtection" - nominal
# "TechSupport" - nominal   
# "StreamingTV"   - nominal   
# "StreamingMovies" - nominal  
# "Contract"   - nominal     
# "PaperlessBilling" - binary
# "PaymentMethod"  - nominal  
# "MonthlyCharges"  - continuous 
# "TotalCharges"  - continuous
# "Churn"   - binary

# Look at missing values - only 11 missing values in TotalCharges
#So we can drop these values
sapply(telco, function(x)  colSums(is.na(telco)))

#Visulaize missing values 
vis_miss(telco)

#Drop missing values from the dataframe

telco_clean <- telco[complete.cases(telco),]
View(telco_clean)

#Change the character columns to factor 








# check for rare event modeling (only for classification problems. if yes, then do undersampling)
#this implies no rare event sampling in the dataset

table(telco_clean$Churn)
# No  Yes 
# 5163 1869 

prop.table(table(telco_clean$Churn))
# No      Yes 
# 0.734215 0.265785 







# set seed and split in to train-valid-test
# explore relationships b/w x and y varibales
# Check multi collinearity in variables
# Start model building
# Check assumptions (for linear regression)
# Run multiple models (logistic, random forest, etc on the trainig data)
# Run all models on validation
#Select the best model from validation and run it on test


# Set seed
set.seed(12345)

#Split the dataset in to 



