#Load dependencies

library(tidyverse)
#install.packages("naniar")
library(naniar)
#install.packages('vcdExtra')
library(vcdExtra)
library(glmnet)
library(car)
#library(gam)
#install.packages("smbinning")
library(smbinning)


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


# Look at missing values - only 11 missing values in TotalCharges
#So we can drop these values
sapply(telco, function(x)  colSums(is.na(telco)))

#Visulaize missing values 
vis_miss(telco)

#Drop missing values from the dataframe

telco_clean <- telco[complete.cases(telco),]
View(telco_clean)

# check for rare event modeling (only for classification problems. if yes, then do undersampling)
#this implies no rare event sampling in the dataset

table(telco_clean$Churn)
# No  Yes 
# 5163 1869 

prop.table(table(telco_clean$Churn))
# No      Yes 
# 0.734215 0.265785 

#Change the column in to factors
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
telco_clean$InternetService <- as.factor(telco_clean$InternetService)

# "OnlineSecurity"  - nominal 
telco_clean$OnlineSecurity <- as.factor(telco_clean$OnlineSecurity)

# "OnlineBackup"   - nominal
telco_clean$OnlineBackup <- as.factor(telco_clean$OnlineBackup)

# "DeviceProtection" - nominal
telco_clean$DeviceProtection <- as.factor(telco_clean$DeviceProtection)

# "TechSupport" - nominal   
telco_clean$TechSupport <- as.factor(telco_clean$TechSupport)

# "StreamingTV"   - nominal   
telco_clean$StreamingTV <- as.factor(telco_clean$StreamingTV)

# "StreamingMovies" - nominal
telco_clean$StreamingMovies <- as.factor(telco_clean$StreamingMovies)

# "Contract"   - nominal   
telco_clean$Contract <- as.factor(telco_clean$Contract)

# "PaperlessBilling" - binary
telco_clean$PaperlessBilling <- as.factor(telco_clean$PaperlessBilling)

# "PaymentMethod"  - nominal  
telco_clean$PaymentMethod <- as.factor(telco_clean$PaymentMethod)

# "MonthlyCharges"  - continuous 
# "TotalCharges"  - continuous

# "Churn"   - binary
#telco_clean$Churn <- as.factor(telco_clean$Churn)

telco_clean$Churn <-ifelse(telco_clean$Churn == "Yes",1,0)


#Split the dataset in to training,validation,testing (70-30)

set.seed(12345)

telco_clean <- telco_clean %>% mutate(id = row_number())

telco_train <- telco_clean %>% sample_frac(0.7)

telco_test <- anti_join(telco_clean, telco_train, by = 'id')

# Change the target variable :

telco_train$good <- abs(telco_train$Churn - 1)

# explore relationships variables in the training dataset

# Bin the continuous variables : tenure, MonthlyCharges, TotalCharges

result_tenure <- smbinning(df = telco_train, y = "good", x = "tenure")
result_tenure$ivtable

result_mcharge <- smbinning(df = telco_train, y = "good", x = "MonthlyCharges")
result_mcharge$ivtable

result_tcharge <- smbinning(df = telco_train, y = "good", x = "TotalCharges")
result_tcharge$ivtable

# Create binned variables 

telco_train <- smbinning.gen(df = telco_train, ivout = result_tenure, chrname = "tenure_bin")

telco_train <- smbinning.gen(df = telco_train, ivout = result_mcharge, chrname = "mcharge_bin")

telco_train <- smbinning.gen(df = telco_train, ivout = result_tcharge, chrname = "tcharge_bin")

View(telco_train)



# 1. Count of people churning 

ggplot(data = telco_train) +
  geom_bar(mapping = aes(x = Churn))


table(telco_train$Churn)
#No  Yes 
#3605 1317  

prop.table(table(telco_train$Churn))
#No       Yes 
#0.7324258 0.2675742 


# 2. Check for convergence issues in the training dataset - quasi or complete separation issues

table(telco_train$gender, telco_train$Churn) # no issues
table(telco_train$SeniorCitizen, telco_train$Churn) # no issues
table(telco_train$Partner, telco_train$Churn) # no issues
table(telco_train$Dependents, telco_train$Churn) # no issues
table(telco_train$PhoneService, telco_train$Churn) # no issues
table(telco_train$MultipleLines, telco_train$Churn) # no issues
table(telco_train$InternetService, telco_train$Churn) # no issues
table(telco_train$OnlineSecurity, telco_train$Churn) # no issues
table(telco_train$OnlineBackup, telco_train$Churn) # no issues
table(telco_train$DeviceProtection, telco_train$Churn) # no issues
table(telco_train$TechSupport, telco_train$Churn) # no issues
table(telco_train$StreamingTV, telco_train$Churn) # no issues
table(telco_train$StreamingMovies, telco_train$Churn) # no issues
table(telco_train$Contract, telco_train$Churn) # no issues
table(telco_train$PaperlessBilling, telco_train$Churn) # no issues
table(telco_train$PaymentMethod, telco_train$Churn) # no issues

table(telco_train$tenure_bin, telco_train$Churn) # no issues
table(telco_train$mcharge_bin, telco_train$Churn) #no issues
table(telco_train$tcharge_bin, telco_train$Churn) #no issues




# 4. Find significant binary variable 

CMHtest(table(telco_train$gender, telco_train$Churn))$table[1,]

CMHtest(table(telco_train$SeniorCitizen, telco_train$Churn))$table[1,]

CMHtest(table(telco_train$Partner, telco_train$Churn))$table[1,]

CMHtest(table(telco_train$Dependents, telco_train$Churn))$table[1,]

CMHtest(table(telco_train$PhoneService, telco_train$Churn))$table[1,]

# 5. Find significant nominal variables

chisq.test(table(telco_train$MultipleLines, telco_train$Churn))

chisq.test(table(telco_train$InternetService, telco_train$Churn))

chisq.test(table(telco_train$OnlineSecurity, telco_train$Churn))

chisq.test(table(telco_train$OnlineBackup, telco_train$Churn))

chisq.test(table(telco_train$DeviceProtection, telco_train$Churn))

chisq.test(table(telco_train$TechSupport, telco_train$Churn))

chisq.test(table(telco_train$StreamingTV, telco_train$Churn))

chisq.test(table(telco_train$StreamingMovies, telco_train$Churn))

chisq.test(table(telco_train$Contract, telco_train$Churn))

chisq.test(table(telco_train$PaymentMethod, telco_train$Churn))

# Initial model 

model1 <- glm(Churn ~ 1, data = telco_train, family = binomial(link = "logit"))

summary(model1)


model2 <- glm(Churn ~ gender , data = telco_train, family = binomial(link = "logit"))
summary(model2)

model3 <- glm(Churn ~ ., data = telco_train, family = binomial(link = "logit"))
summary(model3)

fit.gam <- gam(Churn ~ s(tenure) + gender,
               data = telco_train, family = binomial(link = 'logit'),
               method = 'REML')


# all variables

model4 <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService +
              OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
              Contract + PaperlessBilling + PaymentMethod + tenure_bin +
              mcharge_bin + tcharge_bin,  
              data = telco_train, family = binomial(link = "logit"))
summary(model4)




