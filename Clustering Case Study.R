library(dplyr)

# Download file from the Internet and extract zip archives
if(file.exists("Dataset.zip")) file.remove("Dataset.zip")
download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "Dataset.zip")
unzip(zipfile = "Dataset.zip")

# Read a file in table format 
X_train <- read.table(file = "./UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train <- read.table(file = "./UCI HAR Dataset/train/y_train.txt", header = FALSE)
subject_train <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
features <- read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE)
activity_labels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt", header = FALSE)

# change the name of data frame
samsungData <- X_train

# Add labels the data set with descriptive variable names. 
colnames(samsungData) <- features$V2

# Add column activity
activity_labels_full <- inner_join(x = y_train, y = activity_labels, "V1")
samsungData <- cbind(samsungData, activity = activity_labels_full$V2)

# Add column subject
samsungData <- cbind(samsungData, subject = subject_train$V1)

# Remove unnecessary data frame from an Environment
rm(X_train, y_train, subject_train, features, activity_labels, activity_labels_full)

#####################################

# Plotting average acceleration for first subject
windows()
par(mfrow = c(1,2), mar=c(5,4,1,1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
plot(sub1[,1], col = sub1$activity, ylab= names(sub1)[1])
plot(sub1[,2], col = sub1$activity, ylab= names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), 
       col = unique(sub1$activity), pch = 1)

# Clustering based just on average acceleration
distanceMatrix <- dist(x = sub1[, 1:3])
hclustering <- hclust(d = distanceMatrix)
windows()
plot(hclustering)

# Plotting max acceleration for the first subject
par(mfrow = c(1, 2))
plot(sub1[,10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[,11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

# clustering based on maximum acceleration
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)
windows()
plot(hclustering)

# Singular Value Decomposition
svd1 <- svd(x = scale(x = sub1[, -c(562, 563)])) 
windows()
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col=sub1$activity, pch = 19)
plot(svd1$u[, 2], col=sub1$activity, pch = 19)

# Find maximum contributor
plot(svd1$v[, 2], pch = 19)

# New clustering with maximum contributor
maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(x = sub1[,c(10:12, maxContrib)])
hclustering <- hclust(d = distanceMatrix)
plot(hclustering)

# K-means clustering (nstart = 1, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)

# K-means clustering (nstart = 1, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1)
table(kClust$cluster, sub1$activity)

# K-means clustering (nstart = 100, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

# Cluster 1 Variable Centers (Laying)
plot(kClust$centers[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

# Cluster 2 Variable Centers (Walking)
plot(kClust$centers[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

