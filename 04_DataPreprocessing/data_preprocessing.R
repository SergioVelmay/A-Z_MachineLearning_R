# Data Preprocessing

# Importing the dataset
dataset = read.csv('Data.csv') # press F1 to view method info in Help tab

# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, # not count NA cells for main
                         FUN = function (x) mean(x, na.rm = TRUE)),
                     dataset$Age)

dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, # not count NA cells for main
                            FUN = function (x) mean(x, na.rm = TRUE)),
                        dataset$Salary)

# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Germany', 'Spain'),
                         labels = c(1, 2, 3)) # c() is a vector in R

dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1)) # c() is a vector in R

# install.packages('caTools') # comment this line after installation
library(caTools) # select the library in Packages tab

# Splitting the dataset into the Training set and Test set
set.seed(3) # for random selection
split = sample.split(dataset$Purchased,
                     SplitRatio = 0.8) # 80% for train set (TRUE)

train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
train_set[, 2:3] = scale(train_set[, 2:3]) # scale only columns 2nd and 3rd
test_set[, 2:3] = scale(test_set[, 2:3]) # scale only columns 2nd and 3rd

# EXAMPLE Feature Scaling

# install.packages('caret') # comment this line after installation
# library(caret) # select the library in Packages tab
# 
# train_set <- data.frame(v1 = rnorm(15,3,1), v2 = rnorm(15,2,2))
# test_set <- data.frame(v1 = rnorm(5,3,1), v2 = rnorm(5,2,2))
# 
# scaleParam <- preProcess(train_set, method="scale")
# test_set <- predict(scaleParam, test_set)