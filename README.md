#Coursera: Getting and Cleaning Data - Course Project
###Constructing a combined data set with just the mean and standard deviation variables from both the training and test data sets

* Download the "run_analyis.R" file and source the file in your R environment. `source("run_analysis.R")`

* The project has one R script called "run_analysis.R" which extracts different files from the dataset that was     generated by Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity   Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop    of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

* Once the files are extracted, the training and test data set files are read into their respective data frames
* The activity labels and the subject data are read into their individual data frames for both training and test    data sets.
* The traning and test data frames in their inital state don't have any column names. The variable names from the   variables.txt file were read and assigned as the names to the data frames. Before assigning the names, couple of   data cleaning steps were performed.
 * There were few columns which had the word "Body" repeated in their names. So, we replaced all the                 occurences of "BodyBody" with "Body"
                 `
                 variable_names <- gsub("BodyBody","Body",variable_names)
                `
 * There were also a group of columns that were duplicates. So, we removed the duplicates.
                 `
                        test_df <- test_df[,!duplicated(colnames(test_df))]
                        train_df <- train_df [,!duplicated(colnames(train_df ))]
                 `
* The mean and standard deviation variables were then extracted from both the training and test data sets.
* Two new data frames were created by combining the columns of the mean, standard deviations, subject and activity   was created for both training and test data sets. Later, a single data set was created by combining the rows of   both the data frames.
* The activity column in the new data set is an integer variable and is not descriptive. So, the activity variable   was first converted to a factor variable and then the levels were assigned the descriptions of the activities     read from the activity_descriptions.txt file.
        `
        df$Activity <- as.factor(df$Activity)
        levels(df$Activity) <- activity_labels
        `
* The data set and its column names were then written to the output folder as two separate text files. **mean_and_standard_deviations.txt**, **mean_sd_features.tx**


###Constructing a data set with averages of the mean and standard deviation variables for every subject and activity combination

* First, the combined data set from above was melted to be grouped by Subject and Activity.
        `
        dfMelt <- melt(df, id=c("Subject","Activity"),measure.vars=variable_names_other_than_subject_and_activity)
        `
* Then the melted data set was then reshaped to calculated the averages for mean and standard deviation feature     for every combination of Subject and Activity
        `
        averages_df <- dcast(dfMelt, Subject + Activity ~ variable, mean)
        `
* Finally, the data set and its column names were written to the disk as two separate text files in the output      folder. **averages_features.txt**, **averages_mean_sd_features.txt**