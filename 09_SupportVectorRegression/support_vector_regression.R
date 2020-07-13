# Support Vector Regression (SVR)

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# Fitting SVR to the dataset
# install.packages('e1071')
library(e1071) # SVM: Support Vector Machines
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression')
summary(regressor)

# Predicting a new result
new_level = 6.5
new_pred = predict(regressor, data.frame(Level = new_level))
print(new_pred)

# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)

y_pred = predict(regressor, newdata = dataset)

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_point(aes(x = new_level, y = new_pred),
             colour = 'green') +
  geom_line(aes(x = dataset$Level, y = y_pred),
            colour = 'blue') +
  ggtitle('Support Vector Regression') +
  xlab('Level') +
  ylab('Salary')

# Visualising the SVR results (for higher resolution and smoother curve)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
y_grid = predict(regressor, newdata = data.frame(Level = x_grid))

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_point(aes(x = new_level, y = new_pred),
             colour = 'green') +
  geom_line(aes(x = x_grid, y = y_grid),
            colour = 'blue') +
  ggtitle('Support Vector Regression (Smooth)') +
  xlab('Level') +
  ylab('Salary')