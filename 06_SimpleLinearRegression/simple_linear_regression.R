# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience, # S is proportional to E
               data = train_set)

# Show regressor properties
summary(regressor) # show regressor properties
# Simple Linear Regrassion Equation: y = p0 + p1*x
# constant --> (Inercept) Estimate --> p0 = 25592
# coef -> YearsExperience Estimate --> p1 = 9365
# Final Equation: Salary = 25592 + 9365*Experience

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
y_pred # show prediction values in the console

# Visualising the Simple Linear Regression line
# install.packages('ggplot2')
library(ggplot2)
x_axis_values = train_set$YearsExperience
y_axis_values = predict(regressor, newdata = train_set)
ggplot() + # aes -> generate aesthetic mappings
  geom_line(aes(x = x_axis_values, # draw the Simple Linear Regression line
                y = y_axis_values),
            color = 'blue') +
  ggtitle('Simple Linear Regression Line')

# Visualising the Training set results
ggplot() +
  geom_point(aes(x = x_axis_values, # prediction values from model
                 y = y_axis_values),
             color = 'green') +
  geom_line(aes(x = x_axis_values,
                y = y_axis_values),
            color = 'blue',
            ) +
  geom_point(aes(x = x_axis_values, # observation values from train_set
                 y = train_set$Salary),
             color = 'red') +
  ggtitle('Salary vs Experience (Train)') +
  xlab('Experience (Years)') +
  ylab('Salary (USDollars)')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, # prediction values from y_pred
                 y = y_pred),
             color = 'green') +
  geom_line(aes(x = x_axis_values,
                y = y_axis_values),
            color = 'blue',) +
  geom_point(aes(x = test_set$YearsExperience, # observation values from test_set
                 y = test_set$Salary),
             color = 'red') +
  ggtitle('Salary vs Experience (Test)') +
  xlab('Experience (Years)') +
  ylab('Salary (USDollars)')

# Predicting the Salary for a 12 years Experience employee
exp_test = data.frame(YearsExperience = c(12)) # set variable name and value
exp_test
salary_pred = predict(regressor, newdata = exp_test) # newdata --> data.frame()
salary_pred
# Experience = 12 Years --> Salary = 137973.1 USDollars