# Variables V1-V28 are PCA transformed. Hence, I assume they are scaled. 
# Here I am scaling 'Time' and 'Amount' variables
CreditCardZ <- CreditCard
CreditCardZ["Time"] <- scale(CreditCardZ["Time"])
CreditCardZ["Amount"] <- scale(CreditCardZ["Amount"])

head(CreditCardZ)
summary(CreditCardZ)

# Information Gain  for feature Selection
install.packages("FSelector")
library("FSelector")
infogain <- information.gain(Class~., data = CreditCardZ)
print(infogain)
row.names(infogain)[order(infogain, decreasing = TRUE)]



