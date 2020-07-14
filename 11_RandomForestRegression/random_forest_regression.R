# Random Forest Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[-2], # matrix of features (independent variables)
                         y = dataset$Salary, # dependent variable vector (Salary)
                         ntree = 500) # number of trees built in the random forest
# 10 to 500 trees: same number of steps but more precision in step values 
summary(regressor)

# Predicting a new result with Decision Tree Regression
x_new1 = data.frame(Level = 6.5)
y_pred1 = predict(regressor, x_new1)
print(y_pred1)
x_new2 = data.frame(Level = 6.501)
y_pred2 = predict(regressor, x_new2)
print(y_pred2)

# Visualising the Decision Tree Regression results (low resolution)
# install.packages('ggplot2')
library(ggplot2)
y_pred = predict(regressor, newdata = data.frame(Level = dataset$Level))
ggplot() + # bad representation of the model
  geom_line(aes(x = dataset$Level, y = y_pred),
            colour = 'blue') +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_point(aes(x = x_new1$Level, y = y_pred1),
             colour = 'green') +
  geom_point(aes(x = x_new2$Level, y = y_pred2),
             colour = 'cyan') +
  ggtitle('Decision Tree Regression (low resolution)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Decision Tree Regression results (high resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
y_grid = predict(regressor, newdata = data.frame(Level = x_grid))
ggplot() + # good representation of the model
  geom_line(aes(x = x_grid, y = y_grid),
            colour = 'blue') +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_point(aes(x = x_new1$Level, y = y_pred1),
             colour = 'green') +
  geom_point(aes(x = x_new2$Level, y = y_pred2),
             colour = 'cyan') +
  ggtitle('Decision Tree Regression (high resolution)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the regressor (error and trees)
plot(regressor)