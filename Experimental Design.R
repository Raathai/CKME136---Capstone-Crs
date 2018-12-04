library(caTools)
library(caret)
library(e1071)
library(rminer)
library(rpart.plot)
library(ROSE)
library(DMwR)
library(PRROC)
library(pROC)
library(precrec)

#First I Used only the top 20 variables selected by info gain to build the models

CreditCardS <- CreditCardC[, c("V14", "V17", "V12", "V10", "V11", "V16", "V4", "V3", "V9", "V18", "V7", "V2", "V21", "V6", "V27", "V5", "V1", "V8", "V28", "V19", "Class")]
summary(CreditCardS)

set.seed(2222)

#Data Partition 70%- Train, 30%-Test

split <- sample.split(CreditCardS$Class, SplitRatio = 0.7)
split
train_select <- subset(CreditCardS, split == TRUE)
test_select <- subset(CreditCardS, split == FALSE)

table(train_select$Class)
table(test_select$Class)


#====================================Sampling - SMOTE=====================================

train_select_smote <- SMOTE(Class ~ ., data = train_select)
table(train_select_smote$Class)

#===================================Logistic Regression===================================

#Logistic Regression on train_select_smote dataset
lgmodel3 <- glm(Class ~ ., data=train_select_smote, family = binomial)
summary(lgmodel3)

lgmodel3_Pred <- predict(lgmodel3, test_select, type="response")
lgmodel3_Pred

#ROC curve and AUC
colAUC(lgmodel3_Pred, test_select[["Class"]], plotROC = TRUE)

#Precision - Recall curve and AUC
lgpred3 <- prediction(lgmodel3_Pred, test_select$Class)
lg_RP_perf3 <- performance(lgpred3, "prec", "rec")
plot(lg_RP_perf3, asp=1)
lg_RP_perf3@y.values[[1]][is.nan(lg_RP_perf3@y.values[[1]])] <- 1
caTools::trapz(x=lg_RP_perf3@x.values[[1]], y=lg_RP_perf3@y.values[[1]])

#Confusion Matrix
lgmodel3_PredClass <- ifelse(lgmodel3_Pred > 0.5, "Normal", "Fraud")
confusionMatrix(lgmodel3_PredClass, test_select[["Class"]], dnn = c("Prediction", "Actual"), mode = "prec_recall")


#=====================================Decision Tree====================================

#Decision Tree on train_select_smote dataset
dtmodel3 <- rpart(Class ~ ., data = train_select_smote, method = "class")
prp(dtmodel3)

#Prediction on Test dataset
dtmodel3_Pred <- predict(dtmodel3, test_select, type="class")

#Confusion Matrix
confusionMatrix(dtmodel3_Pred, test_select$Class, dnn = c("Prediction", "Actual"),mode = "prec_recall")

#Prediction by probability on Test dataset
dtmodel3_Pred1 <- predict(dtmodel3, test_select, type="prob")
dtmodel3_Pred1

#ROC curve and AUC
auc <- auc(test_select$Class, dtmodel3_Pred1[,2])
auc
plot(roc(test_select$Class, dtmodel3_Pred1[,2]))

#Recall-Precision curve and AUC
dtpred3 <- prediction(dtmodel3_Pred1[,2], test_select$Class)
dt_RP_perf3 <- performance(dtpred3, "prec", "rec")
plot(dt_RP_perf3, asp=1)
dt_RP_perf3@y.values[[1]][is.nan(dt_RP_perf3@y.values[[1]])] <- 1
caTools::trapz(x=dt_RP_perf3@x.values[[1]], y=dt_RP_perf3@y.values[[1]])
