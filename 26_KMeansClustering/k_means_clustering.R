# K-Means Clustering

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5] # only two columns

# Splitting the dataset into Train and Test
# Feature Scaling

# Using the elbow method to find the optimal number of clusters
set.seed(123)
wcss = vector()
for (n in 1:10) wcss[n] = sum(kmeans(dataset, n)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('Elbow Method'),
     xlab = 'Number of Clusters',
     ylab = 'WCSS - Sum(Withinss)')

# Fitting K-Means to the dataset
set.seed(123)
kmeans = kmeans(x = dataset, centers = 6)
summary(kmeans)

y_kmeans = kmeans$cluster
print(y_kmeans)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Mall Customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')