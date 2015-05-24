library(data.table)
library(dplyr)

# download dataset if it it doesn't exist in the current working directory
if (!file.exists("UCI HAR Dataset")) {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip", method="curl", quiet=TRUE)
  unzip("data.zip")
  file.remove("data.zip")
}

# read features
features <- read.csv('UCI HAR Dataset/features.txt', sep="", header=FALSE)

# read test data and labels
testX <- data.table(read.csv('UCI HAR Dataset/test/X_test.txt', sep="", header=FALSE, col.names=make.names(features$V2)))
testY <- data.table(read.csv('UCI HAR Dataset/test/y_test.txt', sep="", header=FALSE, col.names=c("activity")))
testsubject <- data.table(read.csv('UCI HAR Dataset/test/subject_test.txt', sep="", header=FALSE, col.names=c("subject")))
test <- cbind(testsubject, testX, testY)

# read training data and labels
trainX <- data.table(read.csv('UCI HAR Dataset/train/X_train.txt', sep="", header=FALSE, col.names=make.names(features$V2)))
trainY <- data.table(read.csv('UCI HAR Dataset/train/y_train.txt', sep="", header=FALSE, col.names=c("activity")))
trainsubject <- data.table(read.csv('UCI HAR Dataset/train/y_train.txt', sep="", header=FALSE, col.names=c("subject")))
train <- cbind(trainsubject, trainX, trainY)

# append training data to test data
all <- rbindlist(list(test, train))

# read activity names
activities <- read.csv("UCI HAR Dataset//activity_labels.txt", sep="", header=FALSE, col.names=c("id", "name"))

# get just the columns we're interested in
all <- select(all, subject, activity, matches(".[.]std[.]."), matches(".[.]mean[.]."))

# define a function that can name activities and then apply it
actlist <- as.list(activities[,2])
nameact <- function(x) {tolower(actlist[[x]])}
all$activity <- sapply(all$activity, nameact)

# use tidy data principles to set column names - no punctuation, all lowercase
setnames(all, colnames(all), tolower(gsub("[.]", "", colnames(all))))

# now group this data by subject and activity and then calculate the mean for all columns of these groups
uciharsummary <- summarise_each(group_by(all, subject, activity), funs(mean))
write.csv(uciharsummary, file="uciharsummary.csv", row.names=FALSE)