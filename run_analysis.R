library(reshape)
library(reshape2)
library(dplyr)

##Download the zip file and extract the various files in the data set
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp, mode="wb")
test_set <- unzip(temp,"UCI HAR Dataset/test/X_test.txt")
subject_test_set <-unzip(temp,"UCI HAR Dataset/test/subject_test.txt")
activity_test_set <-unzip(temp,"UCI HAR Dataset/test/y_test.txt")
train_set <- unzip(temp,"UCI HAR Dataset/train/X_train.txt")
subject_train_set <-unzip(temp,"UCI HAR Dataset/train/subject_train.txt")
activity_train_set <-unzip(temp,"UCI HAR Dataset/train/y_train.txt")
variable_names_file <- unzip(temp, "UCI HAR Dataset/features.txt")
activity_labels_file <- unzip(temp, "UCI HAR Dataset/activity_labels.txt")
unlink(temp)

##Read the various files in the test data set into data frames
test_df <- read.table(test_set)
test_activity_df <- read.table(activity_test_set)
names(test_activity_df) <- c("Activity") ## Give the name to the Activity column
test_subject_df <- read.table(subject_test_set)
names(test_subject_df) <- c("Subject") ## Give the name to the Subject column

##Read the various files in the training data set into data frames
train_df <- read.table(train_set)
train_activity_df <- read.table(activity_train_set)
names(train_activity_df) <- c("Activity") ## Give the name to the Activity column
train_subject_df <- read.table(subject_train_set)
names(train_subject_df) <- c("Subject")  ## Give the name to the Subject column

##Read the descriptive names for the signal columns from the features.txt file
variable_names_df <- read.table(variable_names_file)
variable_names <- as.character(variable_names_df[,2])

##Read the activity descriptions
activity_labels_df <- read.table(activity_labels_file)
activity_labels <- as.character(activity_labels_df[,2])

##One of the variables has a repition of the word Body. Replacing with a single occurence. 
variable_names <- gsub("BodyBody","Body",variable_names)

##Assign the cleaned variable_names to the names of the test and train data frames
names(test_df) <- variable_names
names(train_df) <- variable_names

##Some of the columns are duplicates. Cleaning the datasets to have non duplicate columns.
test_df <- test_df[,!duplicated(colnames(test_df))]
train_df <- train_df [,!duplicated(colnames(train_df ))]

##Extract the columns that are means and standard deviations
test_mean_df <- select(test_df,contains("-mean()"))
test_std_df <- select(test_df, contains("-std()"))
train_mean_df <- select(train_df,contains("-mean()"))
train_std_df <- select(train_df, contains("-std()"))

##Create new data frames with just the subject, activity, means and standard deviation columns
test_df <- bind_cols(test_subject_df, test_activity_df,test_mean_df,test_std_df)
train_df <- bind_cols(train_subject_df, train_activity_df,train_mean_df,train_std_df)

##Combine the test and train data frames to a single data frame
df <- bind_rows(test_df,train_df)

##Give descriptive names for the Activity column values. 
df$Activity <- as.factor(df$Activity)
levels(df$Activity) <- activity_labels

## Create a new data set with the average of each variable for each activity and each subject.

## Melt the data frame to be grouped on Subject and Activity
variable_names_other_than_subject_and_activity <- names(df)[!(names(df) %in% c("Subject", "Activity"))]
dfMelt <- melt(df, id=c("Subject","Activity"), measure.vars = variable_names_other_than_subject_and_activity)
## Reshape the molten data frame to a new data frame which has averages of each variable for each activity and subject.
averages_df <- dcast(dfMelt, Subject + Activity ~ variable, mean)

## Adding descriptive column names for the newly created columns which are averages
varibale_names_of_signals <- names(averages_df)[3:68]
varibale_names_of_signals <- sapply(varibale_names_of_signals,function(x){paste0("Average-",x)})
names(averages_df)[3:68] <- varibale_names_of_signals
View(averages_df)  #Manually verify if the data is correct
