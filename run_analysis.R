## load dplyr package
library(dplyr)

## set URL for file download and path for data files
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
dataPath <- "./UCI HAR Dataset/"

download.file(fileURL,"UCI HAR Dataset.zip")
unzip("UCI HAR Dataset.zip")

## read the ordered set of features (column names) and activity labels
features <- read.table(paste0(dataPath,"features.txt"))
labels <- read.table(paste0(dataPath, "activity_labels.txt"))

## read the values for subjects, features and labels for the test cohort
## combine them into the test data set
testSubj <- read.table(paste0(dataPath,"/test/subject_test.txt"))
testFeatures <- read.table(paste0(dataPath,"test/X_test.txt"))
testLabels <- read.table(paste0(dataPath,"test/y_test.txt"))
testData <- cbind(testLabels, testSubj, testFeatures)

## read the values for subjects, features and labels for the train cohort
## combine them into the train data set
trainSubj <- read.table(paste0(dataPath,"train/subject_train.txt"))
trainFeatures <- read.table(paste0(dataPath,"train/X_train.txt"))
trainLabels <- read.table(paste0(dataPath,"train/y_train.txt"))
trainData <- cbind(trainLabels, trainSubj, trainFeatures)


## merge the test and train data
## add column names from the second column of the features data frame
combinedData <- rbind(testData, trainData)
names(combinedData) <- c("activity", "subjectid", features[,2])

## remove all the temporary data frames
remove(testSubj); remove(testFeatures); remove(testLabels)
remove(trainSubj); remove(trainFeatures); remove(trainLabels)
remove(testData); remove(trainData); remove(features)

## add activity column labels corresponding to the integer value of  the activity
## convert activity to a factor variable
activityLookup <- function(x) {labels[x,2]}
combinedData$activity <- activityLookup(combinedData$activity)
combinedData$activity <- as.factor(combinedData$activity)

## select columns to retain in the data set (activity, subjectid, features with mean or std)
combinedData <- select(combinedData, activity, subjectid, matches(".*(mean|std)\\(\\)"))

## rename the data frame columns to remove "()" or "()-"
names(combinedData) <- gsub("\\(\\)|\\(\\)-", "", names(combinedData))

## create summary data frame grouped by activity and subjectid and
## calculate the mean of each grouping over the remaining columns
summaryData <- group_by(combinedData, activity, subjectid) %>%
               summarize(across(everything(), mean), .groups='drop')

## write summaryData to text file summary.txt
write.table(summaryData, file = paste0(dataPath,"summary.txt"), row.names = FALSE)
