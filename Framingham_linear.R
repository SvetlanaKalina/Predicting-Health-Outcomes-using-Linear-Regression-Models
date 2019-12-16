#####################################linear regression with framingham dataset############################
# dataset framingham
wd <- choose.dir()
setwd(wd)

# install.packages('readr')
library(readr)
framingham_raw <- read_csv("framingham.csv")
View(framingham_raw)

# examine the structure of the dataset'
str(framingham_raw)

# categorical: male, education, currentSmoker, BPmeds, PrevStroke, prevhyp,diabetes, heartreate, tenyearCHD
# numerical: age, cigsperday, totchol, sysBP, diaBP, heartRate, glucose

framingham_no_na <- framingham_raw
# missing values
sapply(framingham_no_na, function(x) sum(is.na(x)))

# install.packages('Amelia')
library(Amelia)
missmap(framingham_no_na, main = "Missing values vs observed")
# many NA in glucose, education, BPMeds, totChol, cigsPerDay, BMI, heartrate

# numeric
framingham_no_na$cigsPerDay[is.na(framingham_no_na$cigsPerDay)] <- mean(framingham_no_na$cigsPerDay, na.rm = TRUE)
framingham_no_na$BMI[is.na(framingham_no_na$BMI)] <- mean(framingham_no_na$BMI, na.rm = TRUE)
framingham_no_na$totChol[is.na(framingham_no_na$totChol)] <- mean(framingham_no_na$totChol, na.rm = TRUE)
framingham_no_na$heartRate[is.na(framingham_no_na$heartRate)] <- mean(framingham_no_na$heartRate, na.rm = TRUE)
framingham_no_na$glucose[is.na(framingham_no_na$glucose)] <- mean(framingham_no_na$glucose, na.rm = TRUE)

# categorical
framingham_complete <- na.omit(framingham_no_na)

# any missing values left?
sapply(framingham_complete, function(x) sum(is.na(x)))

# manipulate variables

framingham_complete$male <- as.integer(factor(framingham_complete$male, 
                                          levels = c(0, 1),
                                          labels = c("female",
                                                     "male")))
framingham_complete$education <- as.integer(factor(framingham_complete$education, 
                                               levels = c(1, 2, 3, 4),
                                               labels = c("9_years",
                                                          "highschool",
                                                          "college",
                                                          "college+")))
framingham_complete$currentSmoker <- as.integer(factor(framingham_complete$currentSmoker, 
                                                   levels = c(0, 1),
                                                   labels = c("no",
                                                              "yes")))
framingham_complete$BPMeds <- as.integer(factor(framingham_complete$BPMeds, 
                                            levels = c(0, 1),
                                            labels = c("no",
                                                       "yes")))

framingham_complete$prevalentHyp <- as.integer(factor(framingham_complete$prevalentHyp, 
                                                  levels = c(0, 1),
                                                  labels = c("no",
                                                             "yes")))

framingham_complete$prevalentStroke <- as.integer(factor(framingham_complete$prevalentStroke, 
                                                     levels = c(0, 1),
                                                     labels = c("no",
                                                                "yes")))

framingham_complete$diabetes <- as.integer(factor(framingham_complete$diabetes, 
                                              levels = c(0, 1),
                                              labels = c("no",
                                                         "yes")))

framingham_complete$TenYearCHD <- as.integer(factor(framingham_complete$TenYearCHD, 
                                                levels = c(0, 1),
                                                labels = c("no",
                                                           "yes")))

 
# control
sapply(framingham_complete, function(x) sum(is.na(x)))

# research question: Which factors predict cholesterol?

# backwards selection based on Adjusted R-squared and p-value:

# full model
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + currentSmoker 
              + cigsPerDay 
              + BPMeds 
              + prevalentHyp 
              + prevalentStroke 
              + diabetes 
              + sysBP 
              + diaBP  
              + heartRate 
              + glucose 
              + TenYearCHD 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 0.09785

# exclude glucose
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + currentSmoker 
              + cigsPerDay 
              + BPMeds 
              + prevalentHyp 
              + prevalentStroke 
              + diabetes 
              + sysBP 
              + diaBP  
              + heartRate 
              + TenYearCHD 
              + BMI
              , data = framingham_complete)
summary(cholest)
# without glucose R2 = 0,098

# exclude prevHyp
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + currentSmoker 
              + cigsPerDay 
              + BPMeds 
              + prevalentStroke 
              + diabetes 
              + sysBP 
              + diaBP  
              + heartRate 
              + TenYearCHD 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09828

# exclude tenyearCHD
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + currentSmoker 
              + cigsPerDay 
              + BPMeds 
              + prevalentStroke 
              + diabetes 
              + sysBP 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09848

# exclude currentsmoker
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + cigsPerDay 
              + BPMeds 
              + prevalentStroke 
              + diabetes 
              + sysBP 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09868

# exclude diabetes
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + cigsPerDay 
              + BPMeds 
              + prevalentStroke 
              + sysBP 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09883

# exclude prevstroke
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + cigsPerDay 
              + BPMeds 
              + sysBP 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09851 -> R2 did not improve

# exclude BPmeds
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + cigsPerDay 
              + sysBP 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09876 -> slight improvement again

# exclude sysbp
cholest <- lm(totChol ~ male 
              + age 
              + education 
              + cigsPerDay 
              + diaBP  
              + heartRate 
              + BMI
              , data = framingham_complete)
summary(cholest)
# R2 = 0,09833 -> decrease again, now all variables are significant

' we can say that with 95% certainty and all else equal the total cholesterol 

decreases for
- being male by 7,79 units

and increases by
- 1,3 per year
- 1,59 per higher level of education
- 0,21 for each additional cigarette per day
- 0,34 for each unit of diastolic bloodpressure
- 0,21 for each unit of increased heart rate
- 0,65 for each unit of BMI'

#########################################Predictions###############################################

# create training and test data -
set.seed(100)  
# setting seed to reproduce results of random sampling

# row indices for training data
trainingRowIndex <- sample(1:nrow(framingham_complete), 0.8*nrow(framingham_complete))  

# model training and test data
trainingData <- framingham_complete[trainingRowIndex, ]  
testData  <- framingham_complete[-trainingRowIndex, ]   

# predict distance
distPred <- predict(cholest, testData)  

# make actual predictions
actuals_preds <- data.frame(cbind(actuals=testData$totChol, predicted=distPred))  

# correlation of predictions
correlation_accuracy <- cor(actuals_preds) #very low correlation

# actual values and predictions side by side
head(actuals_preds)
'******************************************************************************************************'
