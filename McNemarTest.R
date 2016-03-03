#install.packages("ISLR")
#install.packages("tree")
#install.packages("party")

library(ISLR)
library(tree)

attach(Carseats)
head(Carseats)

#Some data preprocessing
range(Sales)

High = ifelse(Sales >= 8, "Yes", "No")

new_data = data.frame(Carseats, High)
new_data = new_data[,-1]
#Split the data
set.seed(123)

train = sample(1:nrow(new_data), 3*nrow(new_data)/4)
test = -train

train_data = new_data[train,]
test_data = new_data[test, ]
test_High = High[test]

#Fit the data
#Model 1
tree_model = tree(High ~ ., train_data)
plot(tree_model)
text(tree_model, pretty = 0)

#Model 2
require(party)
ct_model = ctree(High ~ ., data = train_data)

#Predict

tree_pred = predict(tree_model, test_data, type = "class")

ct_pred = predict(ct_model, test_data)

#Errors
sum(tree_pred!= test_High)
#Misclassified: 34/100
sum(ct_pred!=test_High)
#Misclassified: 35/100