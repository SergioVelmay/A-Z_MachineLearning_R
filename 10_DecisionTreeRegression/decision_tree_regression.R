# Decision Tree Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart) # Recursive Partitioning and Regression Trees
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))
# minsplit = minimum number of observations that must exist in a node
summary(regressor)

# Predicting a new result with Decision Tree Regression
x_new1 = data.frame(Level = 6.5)
y_pred1 = predict(regressor, x_new1)
print(y_pred1)
x_new2 = data.frame(Level = 6.499)
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

# Plotting the tree
plot(regressor)
text(regressor)