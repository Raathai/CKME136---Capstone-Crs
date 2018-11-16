library(caTools)
library(caret)
library(e1071)
library(rminer)
library(rpart.plot)
library(ROSE)
library(DMwR)
library(PRROC)

CreditCardC <- CreditCardZ
CreditCardC$Class <- ifelse(CreditCardC$Class=="0", "Normal", "Fraud")
CreditCardC$Class <- as.factor(CreditCardC$Class)
summary(CreditCardC)

#Data Partition 70%- Train, 30%-Test.
set.seed(123)
split <- sample.split(CreditCardC$Class, SplitRatio = 0.7)
split
train <- subset(CreditCardC, split == TRUE)
test <- subset(CreditCardC, split == FALSE)

table(train$Class)
table(test$Class)

#First the dataset is used as it is to build the base model and predict the accuracy of model.

#Logistic regression
lgmodel1 <- glm(Class ~ ., data=train, family = binomial)
summary(lgmodel1)
#AIC value is a measure of the quality of the model. 
#Here AIC is very high, which means the model is not reliable.

#Prediction on Test dataset
lgmodel1_Pred <- predict(lgmodel1, test, type="response") 
lgmodel1_Pred

#ROC Curve
colAUC(lgmodel1_Pred, test[["Class"]], plotROC = TRUE)

#Confusion Matrix
lgmodel1_PredClass <- ifelse(lgmodel1_Pred > 0.86, "Normal", "Fraud")
confusionMatrix(lgmodel1_PredClass, test[["Class"]], dnn = c("Prediction", "Actual"), mode = "prec_recall")

#Decision Tree
dtmodel1 <- rpart(Class ~ ., data = train, method = "class")
prp(dtmodel1)

#Prediction on Test dataset
dtmodel1_Pred <- predict(dtmodel1, test, type="class")

#Confusion Matrix
confusionMatrix(dtmodel1_Pred, test$Class, dnn = c("Prediction", "Actual"),mode = "prec_recall")






#Training dataset is imbalanced
print(table(train$Class))

#Sampling training dataset - SMOTE
set.seed(100)

train_smote <- SMOTE(Class ~ ., data = train, perc.over = 100, perc.under = 200)
table(train_smote$Class)

#Logistic Regression on train_smote dataset
lgmodel2 <- glm(Class ~ ., data=train_smote, family = binomial)
summary(lgmodel2)

lgmodel2_Pred <- predict(lgmodel2, test, type="response")
lgmodel2_Pred

colAUC(lgmodel2_Pred, test[["Class"]], plotROC = TRUE)

lgmodel2_PredClass <- ifelse(lgmodel2_Pred > 0.5, "Normal", "Fraud")
confusionMatrix(lgmodel2_PredClass, test[["Class"]], dnn = c("Prediction", "Actual"), mode = "prec_recall")


pr.curve(test$Class, lgmodel1_Pred)
