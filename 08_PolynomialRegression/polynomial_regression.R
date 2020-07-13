# Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3] # 2nd and 3rd columns only (Level and Salary)

# Splitting the dataset into the Training set and Test set
# it is not necessary this time

# Feature Scaling
# it is not necessary this time

# Fitting Linear Regression to the dataset
regresssor_linear = lm(formula = Salary ~ .,
                       data = dataset)
summary(regresssor_linear)

# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
regresssor_polyn = lm(formula = Salary ~ .,
                      data = dataset)
summary(regresssor_polyn)

# install.packages('ggplot2')
library(ggplot2)

# Predicting with the Simple Linear Regressor
y_pred_linear = predict(regresssor_linear, newdata = dataset)

# Visualising the Linear Regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             color ='red') +
  geom_line(aes(x = dataset$Level, y = y_pred_linear), 
            color ='blue') +
  ggtitle('Simple Linear Regression') +
  xlab('Level') +
  ylab('Salary')

# Predicting with the Polynomial Regressor
y_pred_polyn = predict(regresssor_polyn, newdata = dataset)

# Visualising the Polynomial Regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             color ='red') +
  geom_line(aes(x = dataset$Level, y = y_pred_linear), 
            color ='gray') +
  geom_line(aes(x = dataset$Level, y = y_pred_polyn), 
            color ='blue') +
  ggtitle('Polynomial Regression') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results
# (for higher resolution and smoother curve)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
y_grid = predict(regresssor_polyn, newdata = data.frame(Level = x_grid,
                                                        Level2 = x_grid^2,
                                                        Level3 = x_grid^3,
                                                        Level4 = x_grid^4))
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             color ='red') +
  geom_line(aes(x = dataset$Level, y = y_pred_linear), 
            color ='gray') +
  geom_line(aes(x = x_grid, y = y_grid),
            colour = 'blue') +
  ggtitle('Polynomial Regression (Smooth)') +
  xlab('Level') +
  ylab('Salary')

# New Level to predict
lev = 6.5

# Predicting a new result with Linear Regression
y_prediction_linear = predict(regresssor_linear, data.frame(Level = lev))
print(y_prediction_linear) # bad prediction

# Predicting a new result with Polynomial Regression
y_prediction_polyn = predict(regresssor_polyn, data.frame(Level = lev,
                                                          Level2 = lev^2,
                                                          Level3 = lev^3,
                                                          Level4 = lev^4))
print(y_prediction_polyn) # good prediction