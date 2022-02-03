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
#library(plyr)
#library(dplyr)
library(ggplot2)
install.packages('plotROC')
library(plotROC)
install.packages('tidyverse')
install.packages("MASS")
install.packages("visreg")
install.packages("brglm")
install.packages("car")
install.packages("mgcv")
install.packages("unbalanced")
install.packages("multcomp")
install.packages("rJava")
install.packages("glmulti")
install.packages("givitiR")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("ROCR")
install.packages("InformationValue")
install.packages("brant")
install.packages("VGAM")
install.packages("nnet")

library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)
library(unbalanced)
library(multcomp)
library(rJava)
library(glmulti)
library(givitiR)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(brant)
library(VGAM)
library(nnet)











# Read the data set

telco <- read.csv("C:/Users/conne/OneDrive/Documents/TelcoChurn.csv")

# Basic exploration
dim(telco)  # 7043 R * 21 C
str(telco)
colnames(telco)
glimpse(telco) 

# Categorical variables list
vars <- c('gender', 'SeniorCitizen', 'Partner', 'Dependents', 'PhoneService', 'MultipleLines', 'InternetService',
'OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport', 'StreamingTV', 'StreamingMovies',
'Contract', 'PaperlessBilling' , 'PaymentMethod', 'Churn') 


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


# Combine groups -- MultipleLines,OnlineSecurity,OnlineBackup,DeviceProtection,
# TechSupport, StreamingTV, StreamingMovies

# telco_clean %>% mutate(MultipleLines=recode(MultipleLines,
#                          "No" = "No phone service"))




telco_clean$MultipleLines <- plyr:: mapvalues(telco_clean$MultipleLines, 
                                                 from=c("No phone service"),
                                                  to=c("No"))

for(i in 10:15){
   telco_clean[,i] <- plyr:: mapvalues(telco_clean[,i],
                                 from= c("No internet service"), to= c("No"))
 }
 
# # Change the SeniorCitizen column values
telco_clean$SeniorCitizen <- plyr:: mapvalues(telco_clean$SeniorCitizen, 
                                        from=c("0","1"),
                                        to=c("No","Yes"))


#Change the column in to factors


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

#Check for multicollinearity for continuous variables - correlation exists, so drop any one of the variable

numeric_variable <- c('MonthlyCharges','TotalCharges')
cor(telco_train[,numeric_variable]) # 0.6513654

cor.test(telco_train$MonthlyCharges, telco_train$TotalCharges)

pairs(telco_train[,numeric_variable])

# Change the target variable : to use the smbinning function

telco_train$good <- abs(telco_train$Churn - 1)

# explore relationships variables in the training dataset

# Bin the continuous variables : tenure, MonthlyCharges, TotalCharges

result_tenure <- smbinning(df = telco_train, y = "good", x = "tenure")
result_tenure$ivtable

# result_mcharge <- smbinning(df = telco_train, y = "good", x = "MonthlyCharges")
# result_mcharge$ivtable

result_tcharge <- smbinning(df = telco_train, y = "good", x = "TotalCharges")
result_tcharge$ivtable

# Create binned variables 

telco_train <- smbinning.gen(df = telco_train, ivout = result_tenure, chrname = "tenure_bin")

#telco_train <- smbinning.gen(df = telco_train, ivout = result_mcharge, chrname = "mcharge_bin")

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
#table(telco_train$mcharge_bin, telco_train$Churn) #no issues
table(telco_train$tcharge_bin, telco_train$Churn) #no issues




# 4. Find significant binary variable 

CMHtest(table(telco_train$gender, telco_train$Churn))$table[1,] # not significant

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

# Initial intercept model 

model1 <- glm(Churn ~ 1, data = telco_train, family = binomial(link = "logit"))

summary(model1)


# all variables

model2 <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService +
              OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
              Contract + PaperlessBilling + PaymentMethod + tenure_bin +
               tcharge_bin,  
              data = telco_train, family = binomial(link = "logit"))
summary(model2)


# Use backward selection model to find out significant variables

full.model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService +
                      OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies +
                      Contract + PaperlessBilling + PaymentMethod + tenure_bin +
                      tcharge_bin,  
                    data = telco_train, family = binomial(link = "logit"))

empty.model <- glm(Churn ~ 1, data = telco_train, family = binomial(link = "logit"))

step.model <- step(full.model,
                     scope = list(lower=formula(empty.model),
                                  upper=formula(full.model)),
                     direction = "backward",k = 2) 



# Step:  AIC=4057.67 -- Model from backward selection 
# Churn ~ SeniorCitizen + PhoneService + MultipleLines + InternetService + 
#   OnlineSecurity + TechSupport + StreamingTV + StreamingMovies + 
#   Contract + PaperlessBilling + PaymentMethod + tenure_bin + 
#   tcharge_bin

# use forward slection
model3 <- glm(Churn ~ SeniorCitizen + PhoneService + MultipleLines + InternetService + 
                  OnlineSecurity + TechSupport + StreamingTV + StreamingMovies + 
                Contract + PaperlessBilling + PaymentMethod + tenure_bin + 
                 tcharge_bin, data = telco_train, family = binomial(link = "logit"))

summary(model3)


# Use forward selection to investigate possible interactions


# int.model <- glm(Churn ~ (SeniorCitizen + PhoneService + MultipleLines + InternetService + 
#                    OnlineSecurity + TechSupport + StreamingTV + StreamingMovies + 
#                    Contract + PaperlessBilling + PaymentMethod + tenure_bin + 
#                    tcharge_bin)^2, data = telco_train, family = binomial(link = "logit"))
# 
# 
# for.model <- step(model3,
#                   scope = list(lower=formula(model3),
#                                upper=formula(int.model)),
#                   direction = "forward",k = 2) 
# Churn ~ SeniorCitizen + PhoneService + MultipleLines + InternetService + 
#   OnlineSecurity + TechSupport + StreamingTV + StreamingMovies + 
#   Contract + PaperlessBilling + PaymentMethod + tenure_bin + 
#   tcharge_bin + OnlineSecurity:TechSupport + StreamingMovies:Contract + 
#   PhoneService:TechSupport + TechSupport:PaperlessBilling + 
#   InternetService:StreamingMovies + PhoneService:StreamingMovies + 
#   OnlineSecurity:PaymentMethod + TechSupport:tenure_bin + TechSupport:Contract + 
#   SeniorCitizen:PaymentMethod + PaperlessBilling:PaymentMethod + 
#   StreamingTV:tcharge_bin + TechSupport:StreamingMovies


dev.off()
# Model Assessment

# 1. Coefficient of discrimination

telco_train$p_hat <- predict(model3, type = "response")

p1 <- telco_train$p_hat[telco_train$Churn == 1]
p0 <- telco_train$p_hat[telco_train$Churn == 0]
coef_discrim <- mean(p1) - mean(p0)

ggplot(telco_train, aes(p_hat, fill = factor(Churn))) +
  geom_density(alpha = 0.7) +
  scale_fill_grey() +
  labs(x = "Predicted Probability",
       fill = "Outcome",
       title = paste("Coefficient of Discrimination = ",
                     round(coef_discrim, 3), sep = ""))

# confusion Matrix

confusionMatrix(telco_train$Churn, telco_train$p_hat, threshold = 0.5)

#Find out Youden Index / cut-off = 0.26?
sens <- NULL
spec <- NULL
youden <- NULL
cutoff <- NULL

for(i in 1:49){
  cutoff = c(cutoff, i/50)
  sens <- c(sens, sensitivity(telco_train$Churn, telco_train$p_hat, threshold = i/50))
  spec <- c(spec, specificity(telco_train$Churn, telco_train$p_hat, threshold = i/50))
  youden <- c(youden, youdensIndex(telco_train$Churn, telco_train$p_hat, threshold = i/50))
}

ctable <- data.frame(cutoff, sens, spec, youden)

print(ctable[order(-youden),])


# 2. ROC curve

plotROC(telco_train$Churn, telco_train$p_hat)
































