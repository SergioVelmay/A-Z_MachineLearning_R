# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('California', 'Florida', 'New York'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)

print(split)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling (is not necessary)
# train_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
# formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State
# lm() --> Fitting Linear Models (F1)
regressor = lm(formula = Profit ~ ., # . --> all independent variables
               data = train_set)
summary(regressor)
# Pr --> p-value (good if p-value < 5%)
# *** statistically significant or statistical significance

# Fitting Simple Linear Regression to the Training set
# only the independent variable with statistical significance
regressor_simple = lm(formula = Profit ~ R.D.Spend,
                      data = train_set)
summary(regressor_simple)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
print(y_pred)
# in this case we get same results with multiple and simple regressors
y_pred_simple = predict(regressor, newdata = test_set)
print(y_pred_simple)

difference = (y_pred - test_set$Profit) * 100 / test_set$Profit # percentage
results = data.frame(pred_multi = y_pred, 
                     pred_simple = y_pred_simple, 
                     test_truth = test_set$Profit,
                     error_perc = difference)
print(results)

# Building the optimal model using Backward Elimination
regressor_elimination = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                           data = dataset)
summary(regressor_elimination)
# remove the variable with the highest p-value --> State (0.9 >>> 0.05)
regressor_elimination = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                           data = dataset)
summary(regressor_elimination)
# remove the variable with the highest p-value --> Administration (0.6 >>> 0.05)
regressor_elimination = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                           data = dataset)
summary(regressor_elimination)
# remove the variable with the highest p-value --> Marketing.Spend (0.06 > 0.05) ¿¿¿???

# Comparing Default Regressor and Backward Elimination Regressor
y_pred_elimination = predict(regressor_elimination, newdata = test_set)
print(y_pred_elimination)

difference_elimination = (y_pred_elimination - test_set$Profit) * 100 / test_set$Profit # percentage
elim_versus_multi = abs(difference_elimination) < abs(difference)
results_elimination = data.frame(pred_multi = y_pred, 
                                 err_multi = difference, 
                                 test_truth = test_set$Profit,
                                 err_elim = difference_elimination, 
                                 pred_elim = y_pred_elimination,
                                 elim_wins = elim_versus_multi)
print(results_elimination)

# Automatic implementation of Backward Elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(regressor)
}

SL = 0.05 # significance level of 5% --> results on one dependent variable
# SL = 0.065 # significance level of 6.5% --> results on two dependent variables
back_elim_set = dataset[, c(1,2,3,4,5)]

regressor_automatic = backwardElimination(back_elim_set, SL)
print(regressor_automatic)
summary(regressor_automatic)