## load dplyr package
library(dplyr)

## set path for data files
dataPath <- "./data/UCI HAR Dataset/"

## function to extract the second element from a list
## if not present defaults to first element

secondElement <- function(x) {if(!is.na(x[2])) x[2] else x[1]}

## read the ordered set of features and labels
## remove the numeric value at the start of each line of the labels
features <- readLines(paste0(dataPath,"features.txt"))
labels <- readLines(paste0(dataPath, "activity_labels.txt"))
labels <- strsplit(labels, " ") |> sapply(secondElement)

## read the values for subjects, features and labels for the test cohort
testSubj <- as.numeric(readLines(paste0(dataPath,"/test/subject_test.txt")))
testFeatures <- read.fwf(paste0(dataPath,"test/X_test.txt"), rep.int(16,561))
testLabels <- as.numeric(readLines(paste0(dataPath,"test/y_test.txt")))


## combine subjects, features and labels in single data frame
testData <- cbind(testLabels, testSubj, testFeatures)
names(testData) <- c("activityid", "subjectid", features)
testData <- mutate(testData, dataset = "TEST")
remove(testSubj); remove(testFeatures); remove(testLabels)

## read the values for subjects, features and labels for the train cohort
trainSubj <- as.numeric(readLines(paste0(dataPath,"train/subject_train.txt")))
trainFeatures <- read.fwf(paste0(dataPath,"train/X_train.txt"), rep.int(16,561))
trainLabels <- as.numeric(readLines(paste0(dataPath,"train/y_train.txt")))

## combine subjects, features and labels in single data frame
trainData <- cbind(trainLabels, trainSubj, trainFeatures)
names(trainData) <- c("activityid", "subjectid", features)
trainData <- mutate(trainData, dataset = "TRAIN")
remove(trainSubj); remove(trainFeatures); remove(trainLabels)

## merge the test and train data
combinedData <- rbind(testData, trainData)
remove(testData); remove(trainData)

## add activity column label corresponding to the value of activityid
combinedData <- mutate(activity = labels[activityid], combinedData)
## convert dataset and activity columns to factor variables
combinedData$dataset <- as.factor(combinedData$dataset)
combinedData$activity <- as.factor(combinedData$activity)

## select columns to retain in the data set (dataset, subjectid, fields with mean or std and activity)
combinedData <- select(combinedData, activity, subjectid, matches(".*(mean|std)\\(\\)"), dataset)

## rename the data frame columns to remove preceding numbers and "()" or "()-"
names(combinedData) <- strsplit(names(combinedData), " ") |> sapply(secondElement)
names(combinedData) <- gsub("\\(\\)|\\(\\)-", "", names(combinedData))

## remove dataset column -> group by activity and subjectid -> 
## calculate mean of each column by the grouping
summaryData <- select(combinedData, -dataset) |> group_by(activity, subjectid) |>
    summarize(across(everything(), mean), .groups='drop')

## write summaryData to text file summary.txt
write.table(summaryData, file = paste0(dataPath,"summary.txt"), row.names = FALSE)