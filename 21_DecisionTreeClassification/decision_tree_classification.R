# Decision Tree Classification

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# it is not necessary for this model
train_set[-3] = scale(train_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
# rpart = Recursive Partitioning and Regression Trees
classifier = rpart(formula = Purchased ~ .,
                   data = train_set)
summary(classifier)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
print(y_pred) # probabilities of the two classes for each prediction
y_pred_class = predict(classifier, newdata = test_set[-3], type = 'class')
print(y_pred_class) # only one class for prediction

# Making the Confusion Matrix
confusion = table(test_set[, 3], y_pred_class) # (0 1)
print(confusion)
# 0 no False: 53/59 correct predictions (6 wrong)
# 1 yes True: 30/41 correct predictions (11 wrong)

# Visualising the Training set results
library(ElemStatLearn)
set = train_set
X1 = seq(min(set[, 1]) - 0.25, max(set[, 1]) + 0.25, by = 0.01)
X2 = seq(min(set[, 2]) - 0.25, max(set[, 2]) + 0.25, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classification (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 0.25, max(set[, 1]) + 0.25, by = 0.01)
X2 = seq(min(set[, 2]) - 0.25, max(set[, 2]) + 0.25, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree Classification (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Plotting the tree
plot(classifier, margin = 0.1)
text(classifier)