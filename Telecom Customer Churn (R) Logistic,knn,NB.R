getwd()
setwd("D:/R_wd")
#######################################################################################################################
#                                                                                                                    #
# Purpose:       Predictive Modelling Project                                                                        #
#                                                                                                                    #
# Author:        Ramprasad                                                                                           #                                                    
#                                                                                                                    #
# Code created:  2020-09-08                                                                                          #
# Last updated:  2020-09-21                                                                                         #
# Source:        C:/Users/indway/Desktop                                                                             #
#                                                                                                                    #
# Comment:                                                                                                           #
#                                                                                                                    #
######################################################################################################################

pacman::p_load(readxl,ggplot2,funModeling,car,DataExplorer,psych,MLmetrics,plotly,mvoutlier,factoextra,summarytools,corrplot,OutlierDetection,InformationValue)

Telecom <- read_xlsx(file.choose())

class(Telecom)
Telecom <- as.data.frame(Telecom)
str(Telecom)
summary(Telecom)

###############################################################################################################
#                                         Exploratory Data Analysis                                           #
###############################################################################################################

summarytools::view(dfSummary(Telecom))
plot_missing_2(Telecom)
funModeling::plot_num(Telecom)
psych::pairs.panels(Telecom[,c(2,5,7:11)],hist.col = "blue")
car::scatterplotMatrix(Telecom[,c(2,5,7:11)])

## ----------------------------------- Univariate Box & bar plots --------------------------------------#####

ggplot(Telecom,aes(x=AccountWeeks)) +
  geom_boxplot()

ggplot(Telecom,aes(x=DataUsage)) +
  geom_boxplot()
ggplot(Telecom,aes(x=DayMins)) +
  geom_boxplot()
ggplot(Telecom,aes(x=DayCalls)) +
  geom_boxplot()
ggplot(Telecom,aes(x=MonthlyCharge)) +
  geom_boxplot()

ggplot(Telecom,aes(x=OverageFee)) +
  geom_boxplot()

ggplot(Telecom,aes(x=RoamMins)) +
  geom_boxplot()

ggplot(data=Telecom, aes(x=Churn)) +
  geom_bar(fill="salmon") +
  geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)+
  coord_flip()

ggplot(data=Telecom, aes(x=ContractRenewal)) +
  geom_bar(fill="steelblue") +
  geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)+
  coord_flip()

ggplot(data=Telecom, aes(x=DataPlan)) +
  geom_bar(fill="green4") +
  geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)+
  coord_flip()

ggplot(data=Telecom, aes(x=CustServCalls)) +
  geom_bar(fill="maroon") +
  geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

## ----------------------------------- Bivariate Plots --------------------------------------#####

esquisse::esquisser()

### ------------------------------------------ Outlier Treatment  ---------------------------------------------#####

OutlierDetection::UnivariateOutlierDetection(Telecom$AccountWeeks)
quantile(Telecom$AccountWeeks,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$AccountWeeks[which(Telecom$AccountWeeks > 195)] <- 195

OutlierDetection::UnivariateOutlierDetection(Telecom$DataUsage)
quantile(Telecom$DataUsage,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$DataUsage[which(Telecom$DataUsage > 4.10)] <- 4.10

OutlierDetection::UnivariateOutlierDetection(Telecom$DayCalls)
quantile(Telecom$DayCalls,c(0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$DayCalls[which(Telecom$DayCalls > 152)] <- 152
Telecom$DayCalls[which(Telecom$DayCalls < 47.66)] <- 47.66

OutlierDetection::UnivariateOutlierDetection(Telecom$DayMins)
quantile(Telecom_fc$DayMins,c(0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$DayMins[which(Telecom$DayMins > 340)] <- 340
Telecom$DayMins[which(Telecom$DayMins < 25 )] <- 25

OutlierDetection::UnivariateOutlierDetection(Telecom$MonthlyCharge)
quantile(Telecom$MonthlyCharge,c(0.0025,0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$MonthlyCharge[which(Telecom$MonthlyCharge > 105)] <- 105
Telecom$MonthlyCharge[which(Telecom$MonthlyCharge < 15 )] <- 15

OutlierDetection::UnivariateOutlierDetection(Telecom$OverageFee)
quantile(Telecom$OverageFee,c(0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$OverageFee[which(Telecom$OverageFee > 16)] <- 16
Telecom$OverageFee[which(Telecom$OverageFee < 3 )] <- 3

OutlierDetection::UnivariateOutlierDetection(Telecom$RoamMins)
quantile(Telecom$RoamMins,c(0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99,1))
Telecom$RoamMins[which(Telecom$RoamMins > 18.5)] <- 18.5

boxplot(Telecom[,c(2,7,8,9)])
boxplot(Telecom[,c(5,10,11)])

Telecom_fc <- Telecom

# Telecom$CustServCalls <- as.factor(Telecom$CustServCalls)

Telecom$ContractRenewal <- as.factor(Telecom$ContractRenewal)
Telecom$DataPlan <- as.factor(Telecom$DataPlan)
Telecom$Churn <- as.factor(Telecom$Churn)

## Normality test
library(MVN)
MVN::mvn(Telecom_fc[,c(2,5,7:11)])

###############################################################################################################
#                           Multicollinearity Check and its treatment                                         #
###############################################################################################################

corr <- cor(Telecom_fc)
corrplot(corr)
cor.plot(corr,upper = FALSE,las=2)
corrplot(corr,method = "color",type = "lower",outline = TRUE,addCoef.col = "Black",is.corr = FALSE)
corrplot(corr,method = "pie",type = "lower",outline = TRUE)

logittest <- glm(Telecom$Churn ~ . ,family = 'binomial',data = Telecom)
logittestmc <- glm(Telecom_fc$Churn ~ . ,family = 'binomial',data = Telecom_fc)
summary(logittest)
car::vif(logittest)

## Note : mctest will not work if converted to factors###

library(mctest)
mctest::mc.plot(logittestmc)
mctest::imcdiag(logittestmc)
mctest::omcdiag(logittestmc)

##--------------------------Removing the required variables------------------------####

vif_custfn<-dget("D:/BACP/Predictive Modelling/Final Project/vif_fun.R")
vif_custfn(in_frame = Telecom_fc[,-1],thresh = 5,trace = TRUE)

#---- performing the test again without MonthlyCharge and DataUsage -----------------------------------###

fmla <- as.formula(Churn ~ AccountWeeks + ContractRenewal + DataPlan + CustServCalls + DayMins + DayCalls + OverageFee + RoamMins)
logittest2 <- glm(formula = fmla,family = 'binomial',data = Telecom)
logittest2mc <- glm(formula = fmla,family = 'binomial',data = Telecom_fc)
summary(logittest2)
car::vif(logittest2)

mctest::mc.plot(logittest2mc)
mctest::imcdiag(logittest2mc)
mctest::omcdiag(logittest2mc)

###############################################################################################################
#                           Split the data into training and testing model                                    #
###############################################################################################################

library(rsample)
set.seed(123)
datasplitting <- initial_split(data = Telecom,prop = 0.7,strata = "Churn")

Telecom_train <- training(datasplitting)
Telecom_test <- testing(datasplitting)

head(Telecom_train)
prop.table(table(Telecom_train$Churn))
prop.table(table(Telecom_test$Churn))

###############################################################################################################
#                                                                                                             #
#                           First Part :  Logistic Regression                                                 #
#                                                                                                             #
###############################################################################################################

pacman::p_load(lmtest,pscl,ROCit,popbio,FSA)

lgisticfinal <- glm(formula = fmla,family = 'binomial',data = Telecom_train)
summary(lgisticfinal)
plot(lgisticfinal)

caret::varImp(lgisticfinal)
car::residualPlots(lgisticfinal,col="lightblue")

## ----------- Significance test--------------------###

lmtest::lrtest(lgisticfinal)

## -----------McFadden R2 -----------------------------####

pscl::pR2(lgisticfinal)

## Explanatory power of Odds ##

exp(coef(lgisticfinal))

## Probability ##

(exp(coef(lgisticfinal)))/(1+exp(coef(lgisticfinal)))

## ------------------ plot ---------------------------------###

popbio::logi.hist.plot(Telecom_train$ContractRenewal,Telecom_train$Churn)
abline(h=0.5,lty = 3)
popbio::logi.hist.plot(Telecom_train$CustServCalls,Telecom_train$Churn,xlabel = "Customer Service Calls")
abline(h=0.5,lty = 3)
popbio::logi.hist.plot(Telecom_train$DayMins,Telecom_train$Churn,xlabel = "Average Daytime Mins per month")
abline(h=0.5,lty = 3)
popbio::logi.hist.plot(Telecom_train$OverageFee,Telecom_train$Churn,xlabel = "Overage Fee")
abline(h=0.5,lty = 3)
popbio::logi.hist.plot(Telecom_train$DataPlan,Telecom_train$Churn,xlabel = "Data Plan")

popbio::logi.hist.plot(Telecom_train$DayCalls,Telecom_train$Churn,xlabel = "Average number of daytime calls")
abline(h=0.5,lty = 3)
popbio::logi.hist.plot(Telecom_train$RoamMins,Telecom_train$Churn,xlabel = "Average number of Roaming Mins")
abline(h=0.5,lty = 3)

## -----------------------Predict for train & test ----------------------------------------###

trainlg.pred <- predict(lgisticfinal,newdata = Telecom_train[,-1],type = "response")
Telecom_train$predictedChurn <- ifelse(trainlg.pred > 0.5,1,0)
Telecom_train$probabilities <- predict(lgisticfinal,newdata = Telecom_train[,-1],type = "response")
Telecom_train$predictedChurn <- factor(Telecom_train$predictedChurn,levels = c(0,1))
caret::confusionMatrix(Telecom_train$predictedChurn,Telecom_train$Churn,positive = "1",mode = "everything")

## ------------------------------- ROCR , AUC, GINI, KS ----------------------------------------------------------------------------------###

## ROC Curve ###

ROClogiobj<-ROCit::rocit(score = Telecom_train$probabilities,class = Telecom_train$Churn)
plot(ROClogiobj)

library(ROCR)
library(pROC)

predlogi <- ROCR::prediction(Telecom_train$probabilities,Telecom_train$Churn)
perflogi <- ROCR::performance(predlogi, "tpr", "fpr")

ROCR::plot(perflogi,lwd = 2, main = paste("AUC =", 0.812),colorize = TRUE)
abline(0,1)

logiauc <- performance(predlogi, "auc")

## AUC

plot(pROC::roc(Telecom_train$Churn,Telecom_train$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

##  Gini & KS ###
Telecom_train$predictedChurn <- varhandle::unfactor(Telecom_train$predictedChurn)
Telecom_train$Churn <- varhandle::unfactor(Telecom_train$Churn)

InformationValue::ks_stat(Telecom_train$Churn,Telecom_train$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(Telecom_train$Churn,Telecom_train$predictedChurn)
MLmetrics::Gini(Telecom_train$Churn,Telecom_train$predictedChurn)

## -----------------------  For test set ----------------------##

testlg.pred <- predict(lgisticfinal,newdata = Telecom_test[,-1],type = "response")
Telecom_test$predictedChurn <- ifelse(testlg.pred > 0.5,1,0)
Telecom_test$probabilities <- predict(lgisticfinal,newdata = Telecom_test[,-1],type = "response")
Telecom_test$predictedChurn<- factor(Telecom_test$predictedChurn, levels = c(0,1))

caret::confusionMatrix(Telecom_test$predictedChurn,Telecom_test$Churn,positive="1",mode="everything")

## ------------------------------- ROCR , AUC, GINI, KS ----------------------------------------------------------------------------------###

## ROC Curve ###

ROClogiobj1<-ROCit::rocit(score = Telecom_test$probabilities,class = Telecom_test$Churn)
plot(ROClogiobj1)

library(ROCR)
library(pROC)

predlogi1 <- ROCR::prediction(Telecom_test$probabilities,Telecom_test$Churn)
perflogi1 <- ROCR::performance(predlogi1, "tpr", "fpr")

logiauc1 <- performance(predlogi1, "auc")
ROCR::plot(perflogi1, lwd=2,main = paste("AUC =", 0.83),colorize = TRUE)
abline(0,1)

## AUC

plot(pROC::roc(Telecom_test$Churn,Telecom_test$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

## -- Gini & KS -###

Telecom_test$predictedChurn <- varhandle::unfactor(Telecom_test$predictedChurn)
Telecom_test$Churn <- varhandle::unfactor(Telecom_test$Churn)

InformationValue::ks_stat(Telecom_test$Churn,Telecom_test$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(Telecom_test$Churn,Telecom_test$predictedChurn)
MLmetrics::Gini(Telecom_test$Churn,Telecom_test$predictedChurn)

### ------------------------------------------------------- With Cross Validation --------------------------------------------####
library(caret)

cntrl = trainControl(method = "repeatedcv", number = 10, repeats =3)

set.seed(123)
logi.cvmodel = train(fmla, data = Telecom_train, method="glm",family=binomial(),trControl= cntrl,tuneLength= 10)

plot(varImp(logi.cvmodel))

logi_predcv <- predict(logi.cvmodel,newdata = Telecom_train[,-1])
caret::confusionMatrix(logi_predcv,Telecom_train$Churn,positive = "1",mode = "everything")

set.seed(123)
logi.cvmodel1 = train(fmla, data = Telecom_test, method="glm",family=binomial(),trControl= cntrl,tuneLength= 10)

plot(varImp(logi.cvmodel1))

logitest_predcv <- predict(logi.cvmodel1,newdata = Telecom_test[,-1])
caret::confusionMatrix(logitest_predcv,Telecom_test$Churn,positive = "1",mode = "everything")

###############################################################################################################
#                                                                                                             #
#                           Second Part :  K - nearest neighbour                                              #
#                                                                                                             #
###############################################################################################################

pacman::p_load(caret,class,devtools,Hmisc,klaR,MASS,pROC,kknn)

##--------------------- Scale the data -----------------------------------------------------------#############
## Using min - max normalization

normalize<-function(x)
{
  +return((x-min(x))/(max(x)-min(x)))
}

Telecom_knn <- Telecom_fc

Telecom_knn$Churn <- normalize(Telecom_knn$Churn)
Telecom_knn$AccountWeeks <- normalize(Telecom_knn$AccountWeeks)
Telecom_knn$ContractRenewal <- normalize(Telecom_knn$ContractRenewal)
Telecom_knn$DataPlan <- normalize(Telecom_knn$DataPlan)
Telecom_knn$DataUsage <- normalize(Telecom_knn$DataUsage)
Telecom_knn$CustServCalls <- normalize(Telecom_knn$CustServCalls)
Telecom_knn$DayMins <- normalize(Telecom_knn$DayMins)
Telecom_knn$DayCalls <- normalize(Telecom_knn$DayCalls)
Telecom_knn$MonthlyCharge <- normalize(Telecom_knn$MonthlyCharge)
Telecom_knn$OverageFee <- normalize(Telecom_knn$OverageFee)
Telecom_knn$RoamMins <- normalize(Telecom_knn$RoamMins)

set.seed(123)
datasplitting2 <- initial_split(data = Telecom_knn,prop = 0.7,strata = "Churn")

Telecom_knntrain <- training(datasplitting2)
Telecom_knntest <- testing(datasplitting2)

head(Telecom_knntrain)

###############################################################################################################                                                                                                            #
#                           Code to choose the optimal K - value                                              #                                                                                                       #
###############################################################################################################

acc<-numeric()
bcc <- numeric()
pred <- numeric()
for(i in 1:40)
{
  predict1<-kknn::kknn(formula = Churn ~ . ,train = Telecom_knntrain,test = Telecom_knntest,k = i)
  pred <- predict(predict1,newdata =Telecom_knntest,type = "raw")
  acc <- ifelse(pred>0.5,1,0)
  trtab <- prop.table(table(acc,Telecom_knntest$Churn))
  er <- trtab[1,2] + trtab[2,1]
  bcc <- c(bcc,er)
}

ccc <- numeric()
k.optm=1
pred1 <- numeric()
for (j in 1:40)
  {
  knn.mod <-kknn::kknn(formula = Churn ~ . ,train = Telecom_knntrain,test = Telecom_knntest,k = j)
  pred1 <- predict(knn.mod,newdata =Telecom_knntest,type = "raw")
  ccc <- ifelse(pred1>0.5,1,0)
  k.optm[j] <- 100 * (sum(Telecom_knntest$Churn == ccc)/NROW(Telecom_knntest$Churn))
}

par(mar = c(5, 5, 3, 5))
plot(bcc,type="b",ylab="Error Rate",xlab="K- Value",col="Red")
par(new = TRUE)
plot(k.optm, type="b",xaxt = "n",yaxt = "n",col="Green",ylab="Error Rate",xlab="K- Value")
axis(side = 4)
legend("bottomright", c("Error rate", "Accuracy"),col = c("red", "green"), lty = c(1, 2))
abline(v = 13)

#--------------------------------------------------------------------------------------------###
## Got K optimal value to be 13 to perform the final KNN model####

knnfinal <- kknn::kknn(Telecom_knntrain$Churn ~ .,train = Telecom_knntrain,test = Telecom_knntest,k = 13)
Telecom_knntest$probabilities<- predict(knnfinal,newdata =Telecom_knntest,type = "raw")
Telecom_knntest$predictedChurn <- ifelse(Telecom_knntest$probabilities > 0.5, 1,0)

knnfinal1 <- kknn::kknn(Telecom_knntrain$Churn ~ .,train = Telecom_knntrain,test = Telecom_knntrain,k = 13)
Telecom_knntrain$probabilities<- predict(knnfinal1,newdata =Telecom_knntrain,type = "raw")
Telecom_knntrain$predictedChurn <- ifelse(Telecom_knntrain$probabilities > 0.5, 1,0)

## ---------------------------------- Confusion Matrix and model evaluation --------------------------------#####

Telecom_knntrain$Churn <- as.factor(Telecom_knntrain$Churn)
Telecom_knntrain$predictedChurn <- as.factor(Telecom_knntrain$predictedChurn)
Telecom_knntest$Churn <- as.factor(Telecom_knntest$Churn)
Telecom_knntest$predictedChurn <- as.factor(Telecom_knntest$predictedChurn)

caret::confusionMatrix(Telecom_knntrain$predictedChurn,Telecom_knntrain$Churn,positive = "1",mode = "everything")

caret::confusionMatrix(Telecom_knntest$predictedChurn,Telecom_knntest$Churn,positive = "1",mode = "everything")

## ------------------------------- ROCR , AUC, GINI, KS ----------------------------------------------------------------------------------###
## ROC Curve ###

ROCknnobj<-ROCit::rocit(score = Telecom_knntrain$probabilities,class = Telecom_knntrain$Churn)
plot(ROCknnobj)

library(ROCR)
library(pROC)

predknn <- ROCR::prediction(Telecom_knntrain$probabilities,Telecom_knntrain$Churn)
perfknn <- ROCR::performance(predknn, "tpr", "fpr")

ROCR::plot(perfknn,lwd = 2, main = paste("AUC =", 0.985),colorize = TRUE)
abline(0,1)

knnauc <- performance(predknn, "auc")

## AUC

plot(pROC::roc(Telecom_knntrain$Churn,Telecom_knntrain$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

## ----- Gini & KS ---------------------------------###
library(varhandle)
Telecom_knntrain$predictedChurn <- varhandle::unfactor(Telecom_knntrain$predictedChurn)
Telecom_knntrain$Churn <- varhandle::unfactor(Telecom_knntrain$Churn)

InformationValue::ks_stat(Telecom_knntrain$Churn,Telecom_knntrain$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(Telecom_knntrain$Churn,Telecom_knntrain$predictedChurn)
MLmetrics::Gini(Telecom_knntrain$Churn,Telecom_knntrain$predictedChurn)

## For test set ##

## ROC Curve ###

ROCknnobj1<-ROCit::rocit(score = Telecom_knntest$probabilities,class = Telecom_knntest$Churn)
plot(ROCknnobj1)

library(ROCR)
library(pROC)

predknn1 <- ROCR::prediction(Telecom_knntest$probabilities,Telecom_knntest$Churn)
perfknn1 <- ROCR::performance(predknn1, "tpr", "fpr")

ROCR::plot(perfknn1,lwd=2, main = paste("AUC =", 0.885),colorize = TRUE)
abline(0,1)

knnauc1 <- performance(predknn1, "auc")

## AUC

plot(pROC::roc(Telecom_knntest$Churn,Telecom_knntest$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

## ----- Gini & KS ---------------------------------###

library(varhandle)
Telecom_knntest$predictedChurn <- varhandle::unfactor(Telecom_knntest$predictedChurn)
Telecom_knntest$Churn <- varhandle::unfactor(Telecom_knntest$Churn)

InformationValue::ks_stat(Telecom_knntest$Churn,Telecom_knntest$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(Telecom_knntest$Churn,Telecom_knntest$predictedChurn)
MLmetrics::Gini(Telecom_knntest$Churn,Telecom_knntest$predictedChurn)

## ---------------------------With Cross Validation ----------------------------------------------------------######

cntrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

Telecom_knntrain$Churn <- as.factor(Telecom_knntrain$Churn)

set.seed(123)
knn.cvmodel <- train(Churn ~ ., data = Telecom_knntrain, method='kknn',trControl= cntrl)
knn.cvmodel
plot(knn.cvmodel)
plot(varImp(knn.cvmodel))

knn_cvpred <- predict(knn.cvmodel,Telecom_knntrain)

confusionMatrix(knn_cvpred, Telecom_knntrain$Churn,positive = "1",mode = "everything")

Telecom_knntest$Churn <- as.factor(Telecom_knntest$Churn)

set.seed(123)
knn.cvmodel1 <- train(Churn ~ ., data = Telecom_knntest, method='kknn', trControl= cntrl)
plot(knn.cvmodel1)
knn.cvmodel1
plot(varImp(knn.cvmodel1))

knn_cvpred1 <- predict(knn.cvmodel1,Telecom_knntest)

confusionMatrix(knn_cvpred1, Telecom_knntest$Churn,positive = "1",mode = "everything")

#####################################################################################################################
#                                                                                                                   #
#                                   Third Part :  Naive Bayes                                                       #
#                                                                                                                   #
#####################################################################################################################

library(klaR)

TelecomNB_train <- Telecom_train
TelecomNB_test <- Telecom_test

NBModel <- NaiveBayes(fmla, data = TelecomNB_train)
NBModel

NBproppred <- predict(NBModel,newdata = TelecomNB_train[,-1],type = "class")
TelecomNB_train$predictedChurn <- NBproppred$class
Nbtemp <- NBproppred$posterior[,2]
TelecomNB_train$probabilities <- Nbtemp

caret::confusionMatrix(TelecomNB_train$predictedChurn,TelecomNB_train$Churn,positive = "1",mode = "everything")

## ------------------------------- ROCR , AUC, GINI, KS ----------------------------------------------------------------------------------###

## ROC Curve ###

ROCNBobj<-ROCit::rocit(score = TelecomNB_train$probabilities,class = TelecomNB_train$Churn)
plot(ROCNBobj)

library(ROCR)
library(pROC)

predNB <- ROCR::prediction(TelecomNB_train$probabilities,TelecomNB_train$Churn)
perfNB <- ROCR::performance(predNB, "tpr", "fpr")

ROCR::plot(perfNB,lwd = 2, main = paste("AUC =", 0.864),colorize = TRUE)
abline(0,1)

NBauc <- performance(predNB, "auc")

## AUC

plot(pROC::roc(TelecomNB_train$Churn,TelecomNB_train$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

## ----- Gini & KS ---------------------------------###
library(varhandle)
TelecomNB_train$predictedChurn <- varhandle::unfactor(TelecomNB_train$predictedChurn)
TelecomNB_train$Churn <- varhandle::unfactor(TelecomNB_train$Churn)

InformationValue::ks_stat(TelecomNB_train$Churn,TelecomNB_train$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(TelecomNB_train$Churn,TelecomNB_train$predictedChurn)
MLmetrics::Gini(TelecomNB_train$Churn,TelecomNB_train$predictedChurn)

## For test set ##

NBproppred1 <- predict(NBModel,newdata = TelecomNB_test[,-1],type = "class")
TelecomNB_test$predictedChurn <- NBproppred1$class
Nbtemp1 <- NBproppred1$posterior[,2]
TelecomNB_test$probabilities <- Nbtemp1

caret::confusionMatrix(TelecomNB_test$predictedChurn,TelecomNB_test$Churn,positive = "1",mode = "everything")

## ROC Curve ###

ROCNBobj1<-ROCit::rocit(score = TelecomNB_test$probabilities,class = TelecomNB_test$Churn)
plot(ROCNBobj1)

library(ROCR)
library(pROC)

predNB1 <- ROCR::prediction(TelecomNB_test$probabilities,TelecomNB_test$Churn)
perfNB1 <- ROCR::performance(predNB1, "tpr", "fpr")

ROCR::plot(perfNB1,lwd=2, main = paste("AUC =", 0.87),colorize = TRUE)
abline(0,1)

NBauc <- performance(predNB1, "auc")

## AUC

plot(pROC::roc(TelecomNB_test$Churn,TelecomNB_test$probabilities),print.auc = TRUE,auc.polygon = TRUE,grid=c(0.1, 0.2),grid.col = c("green", "red"),max.auc.polygon = TRUE,auc.polygon.col = "skyblue",print.thres = TRUE,print.auc.x = 0.3,print.auc.y = 0.2)

## ----- Gini & KS ---------------------------------###
library(varhandle)
TelecomNB_test$predictedChurn <- varhandle::unfactor(TelecomNB_test$predictedChurn)
TelecomNB_test$Churn <- varhandle::unfactor(TelecomNB_test$Churn)

InformationValue::ks_stat(TelecomNB_test$Churn,TelecomNB_test$predictedChurn,returnKSTable = TRUE)
InformationValue::ks_plot(TelecomNB_test$Churn,TelecomNB_test$predictedChurn)
MLmetrics::Gini(TelecomNB_test$Churn,TelecomNB_test$predictedChurn)


### ------------------------------------- With Cross Validation ---------------------------------------------------####

cntrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(123)
NB.cvmodel = train(fmla, data = TelecomNB_train, method='nb',trControl= cntrl,tuneLength= 10)
summary(NB.cvmodel)
plot(varImp(NB.cvmodel))

NB_predcv <- predict(NB.cvmodel,newdata = TelecomNB_train[,-1])
caret::confusionMatrix(NB_predcv,TelecomNB_train$Churn,positive = "1",mode = "everything")

## For test

set.seed(123)
NB.cvmodel1 = train(fmla, data = TelecomNB_test, method='nb',trControl= cntrl,tuneLength= 10)

plot(varImp(NB.cvmodel1))

NB_predcv1 <- predict(NB.cvmodel1,newdata = TelecomNB_test[,-1])
caret::confusionMatrix(NB_predcv1,TelecomNB_test$Churn,positive = "1",mode = "everything")

#####################################################################################################################
#                                                                                                                   #
#                             Model Comparison of the three ML models                                               #
#                                                                                                                   #
#####################################################################################################################

# for cross validated models 

train.results <- resamples(list(Logistic_Regression = logi.cvmodel,k_Nearest_Neighbour = knn.cvmodel,Naive_Bayes = NB.cvmodel))
lattice::bwplot(train.results)

test.results <- resamples(list(Logistic_Regression = logi.cvmodel1,k_Nearest_Neighbour = knn.cvmodel1,Naive_Bayes = NB.cvmodel1))
lattice::bwplot(test.results)

## for without CV models i.e orginal

logitraincfmat <- caret::confusionMatrix(Telecom_train$predictedChurn,Telecom_train$Churn,positive = "1",mode = "everything")
logitestcfmat <- caret::confusionMatrix(Telecom_test$predictedChurn,Telecom_test$Churn,positive="1",mode="everything")

knntraincfmat <- caret::confusionMatrix(Telecom_knntrain$predictedChurn,Telecom_knntrain$Churn,positive = "1",mode = "everything")
knntestcfmat <- caret::confusionMatrix(Telecom_knntest$predictedChurn,Telecom_knntest$Churn,positive = "1",mode = "everything")

NBtraincfmat <- caret::confusionMatrix(TelecomNB_train$predictedChurn,TelecomNB_train$Churn,positive = "1",mode = "everything")
NBtestcfmat <- caret::confusionMatrix(TelecomNB_test$predictedChurn,TelecomNB_test$Churn,positive = "1",mode = "everything")

dftrain <-  as.data.frame(logitraincfmat$byClass)
dftrain <- cbind(dftrain,knntraincfmat$byClass,NBtraincfmat$byClass)
colnames(df)
NBtestcfmat
write.csv(dftrain,file = "MEvaltrain.csv")

dftest <- as.data.frame(logitestcfmat$byClass)
dftest <- cbind(dftest,knntestcfmat$byClass,NBtestcfmat$byClass)

write.csv(dftest,file = "MEvaltest.csv")

df <- read.csv(file.choose(),header = TRUE)
esquisse::esquisser(df)

#####################################################################################################################
#                                                                                                                   #
#                                  Conclusion plots to draw inferences                                              #
#                                                                                                                   #
#####################################################################################################################

Telecom <- read_xlsx(file.choose())
Telecom <- as.data.frame(Telecom)

Telecom$CustServCalls <- as.factor(Telecom$CustServCalls)
Telecom$ContractRenewal <- as.factor(Telecom$ContractRenewal)
Telecom$DataPlan <- as.factor(Telecom$DataPlan)
Telecom$Churn <- as.factor(Telecom$Churn)

esquisse::esquisser(Telecom)

dayMins <- cut(Telecom$DayMins, include.lowest = TRUE, breaks = seq(0, 400, by = 30))
ggplot(Telecom, aes(dayMins, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

ggplot(Telecom, aes(ContractRenewal, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

ggplot(Telecom, aes(CustServCalls, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

ggplot(Telecom, aes(DataPlan, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

Overfee <- cut(Telecom$OverageFee, include.lowest = TRUE, breaks = seq(0, 30, by = 1))
ggplot(Telecom, aes(Overfee, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

Roam <- cut(Telecom$RoamMins, include.lowest = TRUE, breaks = seq(0, 25, by = 1))
ggplot(Telecom, aes(Roam, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

Monthly_Bill <- cut(Telecom$MonthlyCharge, include.lowest = TRUE, breaks = seq(0, 200, by = 5))
ggplot(Telecom, aes(Monthly_Bill, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

Day_Calls <- cut(Telecom$DayCalls, include.lowest = TRUE, breaks = seq(0, 200, by = 10))
ggplot(Telecom, aes(Day_Calls, fill = Churn)) + geom_bar(position="dodge") + scale_fill_manual(values=c("green4","Red")) + geom_text(stat='count',aes(label = after_stat(count)),vjust = -1)

################------------------------------xxxxxxxxxxxxxxxxxxxxxxx--------------------------------------#################################

