concrete <- read.csv("data.csv")

library(psych)
library(dplyr)

# Prepare Data
# Remove CCS from data frame
concrete.df <- concrete[,c(-9)]

# Correlation

plot(concrete.df,main="Pairwise Plot", sub="Pariwise plot of dataset")

cor(concrete)
corrplot::corrplot(cor(concrete), main="Correlation Plot")

# Normalization

normalize <- function(x) { ((x-min(x)) / (max(x) - min(x))) }
zscore <- function(x) { (x - mean(x))/ sd(x) }

concrete.df.norm <- as.data.frame(lapply(concrete.df,normalize))
concrete.df.znorm <- as.data.frame(lapply(concrete.df,scale))


# Test reverting to original value
# x = norm(x) * (max(x)-min(x) ) + min(x)

cement.min <- min(concrete.df$Cement)
cement.max <- max(concrete.df$Cement)
cement <- 0.52625571 * (cement.max - cement.min) + cement.min

# Test zScore
# zscore = (x - mean(x))/sd(x)
# x = zscore * sd(x) + mean(x)


zCement <- concrete.znorm[1,1]
x <- zCement * sd(concrete.df$Cement) + mean(concrete.df$Cement)



# Kmeans Clustering

wssplot <- function(data, nc=15,seed=1234) {
  wss <- (nrow(data)-1)* sum(apply(data,2,var))
  for(i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab = "Number of clusters", ylab = "Within groups sum of squares")
}

wssplot(concrete.df.norm,nc=6,seed=12324)

# Use the fviz_nbclust() method in factoextra to get an estimate of the optimal number of clusters.
factoextra::fviz_nbclust(concrete.df.norm, FUNcluster=kmeans,print.summary=TRUE)


concrete.df.norm.k2 = kmeans(concrete.df.norm,centers=2,nstart=25)
concrete.df.norm.k3 = kmeans(concrete.df.norm,centers=3,nstart=25)
concrete.df.norm.k4 = kmeans(concrete.df.norm,centers=4,nstart=25)
concrete.df.norm.k5 = kmeans(concrete.df.norm,centers=5,nstart=25)
concrete.df.norm.k6 = kmeans(concrete.df.norm,centers=6,nstart=25)

# Plot k2
factoextra::fviz_cluster(concrete.df.norm.k2,concrete.df.norm)
# Plot k3
factoextra::fviz_cluster(concrete.df.norm.k3,concrete.df.norm)
# Plot k4
factoextra::fviz_cluster(concrete.df.norm.k4,concrete.df.norm)
# Plot k5
factoextra::fviz_cluster(concrete.df.norm.k5,concrete.df.norm)
# Plot k6
factoextra::fviz_cluster(concrete.df.norm.k6,concrete.df.norm)

# KNN

concrete.df.knn.nrows <- nrow(concrete.df.norm)
concrete.df.knn.sample <- 0.7
concrete.df.knn.train.index <- sample(concrete.df.knn.nrows,concrete.df.knn.sample*concrete.df.knn.nrows)

concrete.df.knn.train <- concrete.df.norm[concrete.df.knn.train.index,]
concrete.df.knn.test <- concrete.df.norm[-concrete.df.knn.train.index,]

concrete.df.knn.train[1:10,]
concrete.df.knn.test[1:10,]

# create training labels for k4
concrete.df.knn.train.k4 <- kmeans(concrete.df.knn.train,centers = 4)

# knn returns feature vector with predicted values for each of test values
library(class)
concrete.df.knn.test.k4 <- knn(concrete.df.knn.train,concrete.df.knn.test, concrete.df.knn.train.k4$cluster, k = 4)

# Generate Labels
concrete.df.knn.train.labels <- concrete.df.knn.train.k4$cluster
# Create test labels via kmeans
concrete.df.knn.test.k4 <- kmeans(concrete.df.knn.test, centers = 4)
concrete.df.knn.test.labels <- concrete.df.knn.test.k4$cluster


# Evaluating kNN
library(gmodels)
concrete.df.knn.predict <- knn(concrete.df.knn.train,concrete.df.knn.test,concrete.df.knn.train.k4$cluster, k=4)

concrete.df.knn.ct <- CrossTable(concrete.df.knn.test.labels, concrete.df.knn.predict,prop.chisq = FALSE)

# 
# library(ggcorrplot)
# library(dplyr)
# 
# 
# 
# ggcorrplot(raw)
# 
# 
# 
# mature = filter(raw, Age >= 28)
# plot(mature)
# asdasdsad

# concrete.filtered <- concrete %>% filter(Age < 50 & Age >20)
# 
# concrete.cw <- concrete.filtered[,c(1,4,5,9)]
# concrete.cw.norm <- as.data.frame(lapply(concrete.cw,normalize))