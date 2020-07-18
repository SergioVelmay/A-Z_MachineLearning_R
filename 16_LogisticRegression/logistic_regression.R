# Logistic Regression

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
# 1 = userId, 2 = gender, 3 = age, 4 = salary, 5 = purchased
dataset = dataset[, 3:5]
# 1 = age, 2 = salary, 3 = purchased

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
train_set[, 1:2] = scale(train_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting Logistic Regression to the Training set
# glm = Generalized Linear Models
classifier = glm(formula = Purchased ~ ., # . = all independent variables
                 family = binomial, # two-column response, yes & no, 1 & 0
                 data = train_set)
summary(classifier)

# Predicting the Test set results
y_pred_prob = predict(classifier,
                      type = 'response', # probabilities
                      newdata = test_set[-3]) # indexes 1 and 2
print(y_pred_prob)
y_pred = ifelse(y_pred_prob > 0.5, 1, 0) # prop > 0.5 ? 1 : 0
print(y_pred)

# Making the Confusion Matrix
#confusion = table(test_set[, 3], y_pred) # (0 1)
confusion = table(test_set[, 3], y_pred > 0.5) # (True False)
print(confusion)
# 0 no False: 57/67 correct predictions (10 wrong)
# 1 yes True: 26/33 correct predictions (7 wrong)

# Visualising the Training set results
# install.packages("C:/.../ElemStatLearn_2015.6.26.tar.gz", repos = NULL, type = "source")
library(ElemStatLearn)
set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Train set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))