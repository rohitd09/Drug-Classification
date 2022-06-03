# Random Forest Classification
rm(list = ls())

# Importing the dataset
dataset = read.csv('drug200.csv')
dataset = dataset[2:6]

# Encoding the target feature as factor
dataset$Drug = factor(dataset$Drug, levels = c('DrugY', 'drugC', 'drugX', 'drugA', 'drugB'), labels = c(0, 1, 2, 3, 4))
dataset$Sex = factor(dataset$Sex, levels = c('F', 'M'), labels = c(0, 1))
dataset$BP = factor(dataset$BP, levels = c('LOW', 'NORMAL', 'HIGH'), labels = c(0, 1, 2))
dataset$Cholesterol = factor(dataset$Cholesterol, levels = c('HIGH', 'NORMAL', 'LOW'), labels = c(2, 1, 0))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Drug, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[-5] = scale(training_set[-5])
# test_set[-5] = scale(test_set[-5])

# Fitting Naive Bayes Classification to the Training set
library(class)
y_pred = knn(train = training_set[, -5],
             test = test_set[, -5],
             cl = training_set[, 5],
             k = 5,
             prob = TRUE)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-6])

# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred)
print(cm)