#The model built Using all 30 variables gave the best performance

CreditCardS1 <- CreditCardC
summary(CreditCardS1)

set.seed(2222)

split1 <- sample.split(CreditCardS1$Class, SplitRatio = 0.7)
train_select1 <- subset(CreditCardS1, split1 == TRUE)
test_select1 <- subset(CreditCardS1, split1 == FALSE)

table(train_select1$Class)
table(test_select1$Class)

#====================================Sampling - SMOTE=====================================
train_select_smote1 <- SMOTE(Class ~ ., data = train_select1)
table(train_select_smote1$Class)

#===================================Logistic Regression===================================

#Logistic Regression on train_select_smote dataset

lgmodel4 <- glm(Class ~ ., data=train_select_smote1, family = binomial)
summary(lgmodel4)

#Predict on test data

lgmodel3_Pred4 <- predict(lgmodel4, test_select1, type="response")

#ROC curve and AUC

colAUC(lgmodel3_Pred4, test_select1[["Class"]], plotROC = TRUE)

#Recall-Precision curve and AUC

lgpred4 <- prediction(lgmodel3_Pred4, test_select1$Class)
lg_RP_perf4 <- performance(lgpred4, "prec", "rec")
plot(lg_RP_perf4, asp=1)
lg_RP_perf4@y.values[[1]][is.nan(lg_RP_perf4@y.values[[1]])] <- 1
caTools::trapz(x=lg_RP_perf4@x.values[[1]], y=lg_RP_perf4@y.values[[1]])

#Confusion Matrix

lgmodel4_PredClass <- ifelse(lgmodel3_Pred4 > 0.5, "Normal", "Fraud")
confusionMatrix(lgmodel4_PredClass, test_select1[["Class"]], dnn = c("Prediction", "Actual"), mode = "prec_recall")

#Accuracy vs cut-off threshold curve

conf <- table(test_select1$Class, as.integer(lgmodel3_Pred4 > 0.5))
acc_at_thresh <- function(threshold) {
  conf <- table(test_select1$Class, lgmodel3_Pred4 > threshold)
  sum(diag(conf))/sum(conf)
}
threshold <- seq(from=0.001, to=1, length=100)
accuracy <- sapply(threshold, acc_at_thresh)
ggplot() + geom_line(aes(x=threshold, y=accuracy))

