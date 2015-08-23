##
##run_analysis.R
##

library(data.table)
library(dplyr)
library(reshape2)

## setup url, filepath, filename
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path <- "C:\\RamReddy\\Coursera\\3.Getting and Cleaning Data"
file <- "Dataset.zip"

#if path does not exists, create
if (!file.exists(path)) {dir.create(path)}

#download zipfile
download.file(url,file.path(path,file))

#unzip the files 
unzip(file.path(path,file),exdir=path)


#set working path - this where files are copied earlier!
setwd(file.path(path,"UCI HAR Dataset"))

#Read Train Data
subjectTrain  <- read.table("train/subject_train.txt")
activityTrain <- read.table("train/y_train.txt")
featuresTrain <- read.table("train/X_train.txt") 

#Read Test Data
subjectTest  <- read.table("test/subject_test.txt")
activityTest <- read.table("test/y_test.txt")
featuresTest <- read.table("test/X_test.txt")

#Read FeatureNames and Activity Labels Data
featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt")

#Append Train and Test Data 
subject  <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# rename the column names
# features: v1--v561, featureNames:v1-V2 w/ 561 Rows with corresponding names for features.
colnames(features) <- t(featureNames[2]) #transpose the column V2 and name it to features
colnames(activity) <- "Activity" #only one clumn: V1
colnames(subject) <- "Subject"   #only one clumn: V2

#Merge All three datasets 
All_Data <- cbind(features,activity,subject)

# Column indices with Mean, STD
col_MeanSTD <- grep(".*Mean.*|.*Std.*", names(All_Data), ignore.case=TRUE)

# include other 2 columns(Activity and Subect) we need
col_required <- c(col_MeanSTD, 562, 563)

# Data with columns of interest
Sub_Data <- All_Data[,col_required]

#Update Activity Labels- WALKING....LAYING  etc
currentActivity = 1 
for (currentActivityLabel in activityLabels$V2) { 
   Sub_Data$Activity <- gsub(currentActivity, currentActivityLabel, Sub_Data$Activity) 
   currentActivity <- currentActivity + 1 
} 

# Convert Subject and Activity to Factors (from integer and Character respectively)
Sub_Data$Activity <- as.factor(Sub_Data$Activity) 
Sub_Data$Subject <- as.factor(Sub_Data$Subject) 

# Descriptive Varibale Names - little more self explonatory
names(Sub_Data)<-gsub("^t", "Time", names(Sub_Data))
names(Sub_Data)<-gsub("^f", "Frequency", names(Sub_Data))
names(Sub_Data)<-gsub("tBody", "TimeBody", names(Sub_Data))
names(Sub_Data)<-gsub("angle", "Angle", names(Sub_Data))
names(Sub_Data)<-gsub("gravity", "Gravity", names(Sub_Data))

# Average of each variable in the datset for each activity and each subject
tidy_Data <- aggregate(. ~ Subject + Activity, data=Sub_Data, mean)

#Order the tidy data by Subject and Activity
tidy_Data <- tidy_Data[order(tidy_Data$Subject,tidy_Data$Activity),]

#Write out the tidy data to a text file
write.table(tidy_Data, file = "Tidy_Data.txt", row.names = FALSE)
