#Load the Data
CreditCard <- read.csv("creditcard.csv", header = TRUE)

head(CreditCard)

#Check the overall structure of the dataset
str(CreditCard)

#Convert the 'class' variable as factor
CreditCard$Class = as.factor(CreditCard$Class)

#View the summary statistics of the Dataset
summary(CreditCard)

#Check for missing values
apply(CreditCard,2, function(x) sum(is.na(x)))

#Check the imbalance of the target variable
summary(CreditCard$Class) # '0' - 284315, '1' - 492
barplot(table(CreditCard$Class), col = c("green", "red"), names=c("class 0 - Genuine", "class 1 - Fraud"), main = 'Transactions by class')

#Subsetting fraud transactions in Day-1 and Day-2
fraudDay1 <- subset(CreditCard, CreditCard$Class=='1' & CreditCard$Time <= 86400)
fraudDay2 <- subset(CreditCard, CreditCard$Class=='1' & CreditCard$Time > 86400)

#Subsetting genuine transactions in Day-1 and Day-2
genuineDay1 <- subset(CreditCard, CreditCard$Class=='0' & CreditCard$Time <= 86400)
genuineDay2 <- subset(CreditCard, CreditCard$Class=='0' & CreditCard$Time > 86400)

#Plot each day transactions separately for fraud and genuine transactions 
hist(fraudDay1$Time, main = "Day -1 Fraud Transactions", xlab = "Time", col = "red", breaks = 20)
hist(fraudDay2$Time, main = "Day -2 Fraud Transactions", xlab = "Time", col = "red", breaks = 20)
hist(genuineDay1$Time, main = "Day -1 Genuine Transactions", xlab = "Time", col = "green", breaks = 20)
hist(genuineDay2$Time, main = "Day -2 Genuine Transactions", xlab = "Time", col = "green", breaks = 20)

#subset transactions by 'class'
fraud <- subset(CreditCard, CreditCard$Class=='1')
genuine <- subset(CreditCard, CreditCard$Class=='0')

summary(fraud$Amount)
summary(genuine$Amount)

#Correlation Matrix
cor(CreditCard)

#Correlation between each variable and target variable
correlation <- cor(CreditCard[1:30], CreditCard$Class, method = "pearson")
correlation

#Visualize correlation matrix
install.packages("corrplot")
library("corrplot")
corrplot(cor(CreditCard))
