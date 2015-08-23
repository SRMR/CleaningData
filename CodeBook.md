
# Code Book for Analysis.R
#### Sunday, August 23, 2015

This document describes the steps followed from reading the raw data files to transforming them it to a tiny data file that is ready for anaysis.
Also describes the variables, the data, and transformations performed during the above process.


### 1.Reading Data
   First we will read all the test and train data files for all Subject, Activity and Features



`Read Train Data`  
subjectTrain  <- read.table("train/subject_train.txt")  
activityTrain <- read.table("train/y_train.txt")  
featuresTrain <- read.table("train/X_train.txt")   

***

```{r, eval=FALSE}
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
```

***
### 2. Combine Data 
   Put together all data files with only required variables 

```{r, eval=FALSE}
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
```


### 3. Transform and Create Final Dataset
```{r, eval=FALSE}
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
```

### 4. Create a file for the tidy data

```{r, eval=FALSE}
#Write out the tidy data to a text file
write.table(tidy_Data, file = "Tidy_Data.txt", row.names = FALSE)
```

## List of Variables on Tidy Data:
```{r, eval=FALSE}
> names(tidy_Data)
 [1] "Subject"                                 "Activity"                                "TimeBodyAcc-mean()-X"                   
 [4] "TimeBodyAcc-mean()-Y"                    "TimeBodyAcc-mean()-Z"                    "TimeBodyAcc-std()-X"                    
 [7] "TimeBodyAcc-std()-Y"                     "TimeBodyAcc-std()-Z"                     "TimeGravityAcc-mean()-X"                
[10] "TimeGravityAcc-mean()-Y"                 "TimeGravityAcc-mean()-Z"                 "TimeGravityAcc-std()-X"                 
[13] "TimeGravityAcc-std()-Y"                  "TimeGravityAcc-std()-Z"                  "TimeBodyAccJerk-mean()-X"               
[16] "TimeBodyAccJerk-mean()-Y"                "TimeBodyAccJerk-mean()-Z"                "TimeBodyAccJerk-std()-X"                
[19] "TimeBodyAccJerk-std()-Y"                 "TimeBodyAccJerk-std()-Z"                 "TimeBodyGyro-mean()-X"                  
[22] "TimeBodyGyro-mean()-Y"                   "TimeBodyGyro-mean()-Z"                   "TimeBodyGyro-std()-X"                   
[25] "TimeBodyGyro-std()-Y"                    "TimeBodyGyro-std()-Z"                    "TimeBodyGyroJerk-mean()-X"              
[28] "TimeBodyGyroJerk-mean()-Y"               "TimeBodyGyroJerk-mean()-Z"               "TimeBodyGyroJerk-std()-X"               
[31] "TimeBodyGyroJerk-std()-Y"                "TimeBodyGyroJerk-std()-Z"                "TimeBodyAccMag-mean()"                  
[34] "TimeBodyAccMag-std()"                    "TimeGravityAccMag-mean()"                "TimeGravityAccMag-std()"                
[37] "TimeBodyAccJerkMag-mean()"               "TimeBodyAccJerkMag-std()"                "TimeBodyGyroMag-mean()"                 
[40] "TimeBodyGyroMag-std()"                   "TimeBodyGyroJerkMag-mean()"              "TimeBodyGyroJerkMag-std()"              
[43] "FrequencyBodyAcc-mean()-X"               "FrequencyBodyAcc-mean()-Y"               "FrequencyBodyAcc-mean()-Z"              
[46] "FrequencyBodyAcc-std()-X"                "FrequencyBodyAcc-std()-Y"                "FrequencyBodyAcc-std()-Z"               
[49] "FrequencyBodyAcc-meanFreq()-X"           "FrequencyBodyAcc-meanFreq()-Y"           "FrequencyBodyAcc-meanFreq()-Z"          
[52] "FrequencyBodyAccJerk-mean()-X"           "FrequencyBodyAccJerk-mean()-Y"           "FrequencyBodyAccJerk-mean()-Z"          
[55] "FrequencyBodyAccJerk-std()-X"            "FrequencyBodyAccJerk-std()-Y"            "FrequencyBodyAccJerk-std()-Z"           
[58] "FrequencyBodyAccJerk-meanFreq()-X"       "FrequencyBodyAccJerk-meanFreq()-Y"       "FrequencyBodyAccJerk-meanFreq()-Z"      
[61] "FrequencyBodyGyro-mean()-X"              "FrequencyBodyGyro-mean()-Y"              "FrequencyBodyGyro-mean()-Z"             
[64] "FrequencyBodyGyro-std()-X"               "FrequencyBodyGyro-std()-Y"               "FrequencyBodyGyro-std()-Z"              
[67] "FrequencyBodyGyro-meanFreq()-X"          "FrequencyBodyGyro-meanFreq()-Y"          "FrequencyBodyGyro-meanFreq()-Z"         
[70] "FrequencyBodyAccMag-mean()"              "FrequencyBodyAccMag-std()"               "FrequencyBodyAccMag-meanFreq()"         
[73] "FrequencyBodyBodyAccJerkMag-mean()"      "FrequencyBodyBodyAccJerkMag-std()"       "FrequencyBodyBodyAccJerkMag-meanFreq()" 
[76] "FrequencyBodyBodyGyroMag-mean()"         "FrequencyBodyBodyGyroMag-std()"          "FrequencyBodyBodyGyroMag-meanFreq()"    
[79] "FrequencyBodyBodyGyroJerkMag-mean()"     "FrequencyBodyBodyGyroJerkMag-std()"      "FrequencyBodyBodyGyroJerkMag-meanFreq()"
[82] "Angle(TimeBodyAccMean,Gravity)"          "Angle(TimeBodyAccJerkMean),GravityMean)" "Angle(TimeBodyGyroMean,GravityMean)"    
[85] "Angle(TimeBodyGyroJerkMean,GravityMean)" "Angle(X,GravityMean)"                    "Angle(Y,GravityMean)"                   
[88] "Angle(Z,GravityMean)"                   
```

```{r, eval=FALSE}

```

## Details of Varibales on the raw data and tidy data
- **subject** : The ID of the test subject
- **activity**: The type of activity performed when the corresponding measurements were taken

- *mean* : Mean value
- *std* : Standard deviation



`features.txt` has all the features extracted with two varibales V1: Feature#, V2:Feature Name

     V1                    V2
     1                    tBodyAcc-mean()-X  
     2                    tBodyAcc-mean()-Y  
     3                    tBodyAcc-mean()-Z  
     4                     tBodyAcc-std()-X  
     5                     tBodyAcc-std()-Y  
     ..                    ...               
     ..                    ... 
    
`activity.txt` has activities  
   V1                  V2    
   1              WALKING  
   2     WALKING_UPSTAIRS  
   3   WALKING_DOWNSTAIRS  
   4              SITTING  
   5             STANDING  
   6               LAYING  
   

###
    

